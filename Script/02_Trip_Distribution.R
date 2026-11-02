#Trip Distribution
#create road network
jakarta_network <- st_read(
  "D:/Github/schoolMobility/Data/jakarta_network.gpkg"
)

road_classes <- c(
  "bridleway", "footway", "path", "pedestrian",
  "cycleway", "living_street", "track",
  "residential", "service",
  "steps", "unclassified",
  "tertiary", "tertiary_link",
  "secondary", "secondary_link",
  "primary", "primary_link",
  "trunk", "trunk_link"
)


jakarta_road <- jakarta_network %>%
  filter(fclass %in% road_classes) %>%
  st_cast("LINESTRING") %>%
  mutate(
    oneway = FALSE,
    length_m = as.numeric(st_length(geom)),
    edge_id = row_number()
  )

jakarta_road_4326 <- jakarta_road %>%
  st_transform(4326)

#convert road network into graph
graph <- weight_streetnet(
  jakarta_road_4326,
  wt_profile = "foot",
  type_col = "fclass"
)

#create origin destination point
#create origin point
origin <- jakarta_rw %>%
  mutate(
    zone_id = KDFPUM
  )

origin_points <- origin %>%
  mutate(
    centroid_x = ifelse(
      is.na(centroid_x),
      st_coordinates(st_centroid(geom))[, 1],
      centroid_x
    ),
    centroid_y = ifelse(
      is.na(centroid_y),
      st_coordinates(st_centroid(geom))[, 2],
      centroid_y
    )
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(
    coords = c("centroid_x", "centroid_y"),
    crs = 32748,
    remove = FALSE
  )

origin_4326 <- st_transform(
  origin_points, 4326)

#create destination point
destination <- jakarta_rwA %>%
  mutate(
    zone_id = KDFPUM
  )

destination_points <- destination %>%
  mutate(
    centroid_x = ifelse(
      is.na(centroid_x),
      st_coordinates(st_centroid(geom))[, 1],
      centroid_x
    ),
    centroid_y = ifelse(
      is.na(centroid_y),
      st_coordinates(st_centroid(geom))[, 2],
      centroid_y
    )
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(
    coords = c("centroid_x", "centroid_y"),
    crs = 32748,
    remove = FALSE
  )

destination_4326 <- st_transform(
  destination_points, 4326)

#Combine origin and destination into the similar graph
#Origin points
origin_all <- origin_4326 %>%
  mutate(
    point_type = "origin",
    point_id = paste0("O_", row_number())
  )

#Destination points
destination_all <- destination_4326 %>%
  mutate(
    point_type = "destination",
    point_id = paste0("D_", row_number())
  )

#Combine all OD points
od_points <- bind_rows(
  origin_all,
  destination_all
)

#Add origin AND destination points to the same graph
graph_with_od <- add_nodes_to_graph(
  graph,
  od_points
)

#Extract vertices from the combined graph
graph_vertices_od <- dodgr_vertices(
  graph_with_od
)

#Match all OD points to vertices
od_vertex_idx <- match_points_to_verts(
  graph_vertices_od,
  od_points
)

#Create OD vertex lookup table
od_vertex_df <- od_points %>%
  st_drop_geometry() %>%
  transmute(
    point_id,
    point_type,
    zone_id,
    vertex_idx = od_vertex_idx,
    vertex_id = graph_vertices_od$id[od_vertex_idx]
  )

#Separate Origin and destination vertices from the combined graph
origin_vertex_df <- od_vertex_df %>%
  filter(point_type == "origin") %>%
  transmute(
    origin_zone = zone_id,
    origin_vertex = vertex_id
  )

destination_vertex_df <- od_vertex_df %>%
  filter(point_type == "destination") %>%
  transmute(
    destination_zone = zone_id,
    destination_vertex = vertex_id
  )

#Calculate network distances between all OD pairs
od_distances <- dodgr_dists(
  graph_with_od,
  from = origin_vertex_df$origin_vertex,
  to = destination_vertex_df$destination_vertex
)

# Convert matrix to long format using row/column positions
od_distances_df <- as.data.frame(
  as.table(od_distances),
  responseName = "distance"
) %>%
  transmute(
    origin_idx = as.integer(Var1),
    destination_idx = as.integer(Var2),
    distance = as.numeric(distance)
  ) %>%
  mutate(
    origin_vertex = origin_vertex_df$origin_vertex[origin_idx],
    destination_vertex =
      destination_vertex_df$destination_vertex[destination_idx],
    origin_zone = origin_vertex_df$origin_zone[origin_idx],
    destination_zone =
      destination_vertex_df$destination_zone[destination_idx]
  ) %>%
  select(
    origin_zone, origin_vertex,
    destination_zone, destination_vertex,
    distance
  )

summary(od_distances_df$distance)

od_distances_df$distance[
  is.na(od_distances_df$distance) &
    od_distances_df$origin_zone == od_distances_df$destination_zone
] <- 0

na_distance <- 3*max(od_distances_df$distance, na.rm = TRUE)
od_distances_df$distance[
  is.na(od_distances_df$distance)
] <- na_distance

#generate trip distribution using gravity model - doubly constrained
trip_production <- jakarta_rw %>%
  st_drop_geometry() %>%
  select(centroid_x,
         centroid_y,
         KDFPUM,
         trip_production)

trip_attraction <- jakarta_rwA %>%
  st_drop_geometry() %>%
  select(centroid_x,
         centroid_y,
         KDFPUM,
         trip_attraction = trip_attraction_proportion)

trip_production <- trip_production %>%
  mutate(KDFPUM = as.character(KDFPUM))

trip_attraction <- trip_attraction %>%
  mutate(KDFPUM = as.character(KDFPUM))

od_distances_df <- od_distances_df %>%
  mutate(
    origin_zone = as.character(origin_zone),
    destination_zone = as.character(destination_zone)
  )

#convert distance into km
od_distances_df <- od_distances_df %>%
  mutate(distance = distance / 1000)

#convert OD distance dataframe into matrix
zone_ids <- sort(trip_production$KDFPUM)
N <- length(zone_ids)

od_pairs <- expand_grid(
  origin_zone = zone_ids,
  destination_zone = zone_ids
)

od_distances_complete <- od_pairs %>%
  left_join(
    od_distances_df %>%
      select(origin_zone, destination_zone, distance),
    by = c("origin_zone", "destination_zone")
  )

distance_matrix <- matrix(
  NA_real_,
  nrow = N,
  ncol = N,
  dimnames = list(zone_ids, zone_ids)
)

i <- match(
  od_distances_complete$origin_zone,
  zone_ids
)

j <- match(
  od_distances_complete$destination_zone,
  zone_ids
)

distance_matrix[cbind(i, j)] <-
  od_distances_complete$distance

#GENERATE TRIP DISTRIBUTION
#create production and attraction vector

P <- trip_production$trip_production[
  match(zone_ids, trip_production$KDFPUM)
]

A <- trip_attraction$trip_attraction[
  match(zone_ids, trip_attraction$KDFPUM)
]

names(P) <- zone_ids
names(A) <- zone_ids

#trip distribution based on gravity model doubly constrained
#create gravity model function
run_gravity <- function(
    distance_matrix,
    P,
    A,
    beta,
    tol = 1e-8,
    max_iter = 10000
) {
  # A. Validasi input
  stopifnot(
    is.matrix(distance_matrix),
    nrow(distance_matrix) == length(P),
    ncol(distance_matrix) == length(A),
    identical(rownames(distance_matrix), names(P)),
    identical(colnames(distance_matrix), names(A)),
    all(is.finite(distance_matrix)),
    all(distance_matrix >= 0),
    all(is.finite(P)),
    all(is.finite(A)),
    all(P >= 0),
    all(A >= 0),
    is.finite(beta),
    beta > 0
  )
  
  if (!isTRUE(all.equal(
    sum(P), sum(A), tolerance = 1e-6
  ))) {
    stop("Total production and attraction was not balance")
  }
  
  #impedance function
  F <- exp(-beta * distance_matrix)
  
  if (any(!is.finite(F)) || any(F < 0)) {
    stop("impedance matrices was not valid.")
  }
  
  #initial matrix
  T <- outer(P, A) * F
  
  if (any(!is.finite(T))) {
    stop("initial matrix has infinite value.")
  }
  
  if (any(P > 0 & rowSums(T) <= 0) ||
      any(A > 0 & colSums(T) <= 0)) {
    stop(
      "zone target doesnt have weighted OD"
    )
  }
  
  #Iterative proportional fitting
  converged <- FALSE
  err_P <- Inf
  err_A <- Inf
  
  for (iter in seq_len(max_iter)) {
    
    rs <- rowSums(T)
    
    factor_P <- numeric(length(P))
    factor_P[P > 0] <- P[P > 0] / rs[P > 0]
    
    T <- sweep(T, 1, factor_P, "*")
    
    cs <- colSums(T)
    
    factor_A <- numeric(length(A))
    factor_A[A > 0] <- A[A > 0] / cs[A > 0]
    
    T <- sweep(T, 2, factor_A, "*")
    
    err_P <- max(
      abs(rowSums(T) - P) / pmax(P, 1)
    )
    
    err_A <- max(
      abs(colSums(T) - A) / pmax(A, 1)
    )
    
    if (max(err_P, err_A) < tol) {
      converged <- TRUE
      break
    }
  }
  
  #Output
  
  list(
    od_matrix = T,
    impedance_matrix = F,
    beta = beta,
    iterations = iter,
    converged = converged,
    error_production = err_P,
    error_attraction = err_A
  )
}

#running scenarios
h_values <- c(5, 10, 15)

beta_scenarios <- data.frame(
  scenario = paste0("h_", h_values, "km"),
  half_decay_km = h_values,
  beta = log(2) / h_values
)

gravity_results <- setNames(
  lapply(seq_len(nrow(beta_scenarios)), function(k) {
    
    run_gravity(
      distance_matrix = distance_matrix,
      P = P,
      A = A,
      beta = beta_scenarios$beta[k]
    )
    
  }),
  beta_scenarios$scenario
)

#OD matrices on each scenario

od_matrix_h5 <- gravity_results[["h_5km"]]$od_matrix

od_matrix_h10 <- gravity_results[["h_10km"]]$od_matrix

od_matrix_h15 <- gravity_results[["h_15km"]]$od_matrix

#impedance matrices
impedance_matrix_h5 <-
  gravity_results[["h_5km"]]$impedance_matrix

impedance_matrix_h10 <-
  gravity_results[["h_10km"]]$impedance_matrix

impedance_matrix_h15 <-
  gravity_results[["h_15km"]]$impedance_matrix

#convergence summary on each scenario
convergence_summary <- data.frame(
  scenario = names(gravity_results),
  beta = sapply(gravity_results, `[[`, "beta"),
  iterations = sapply(gravity_results, `[[`, "iterations"),
  converged = sapply(gravity_results, `[[`, "converged"),
  error_production = sapply(
    gravity_results, `[[`, "error_production"
  ),
  error_attraction = sapply(
    gravity_results, `[[`, "error_attraction"
  )
)

print(convergence_summary)

#model evaluation
evaluate_scenario <- function(od, distance_matrix, P, A) {
  
  total_trips <- sum(od)
  
  if (total_trips <= 0) {
    stop("Total perjalanan harus lebih besar dari nol.")
  }
  
  data.frame(
    total_trips = total_trips,
    
    mean_distance_km =
      sum(od * distance_matrix) / total_trips,
    
    intrazonal_share =
      sum(diag(od)) / total_trips,
    
    max_production_error =
      max(abs(rowSums(od) - P)),
    
    max_attraction_error =
      max(abs(colSums(od) - A)),
    
    max_production_relative_error =
      max(abs(rowSums(od) - P) / pmax(P, 1)),
    
    max_attraction_relative_error =
      max(abs(colSums(od) - A) / pmax(A, 1))
  )
}

scenario_evaluation <- do.call(
  rbind,
  lapply(seq_along(gravity_results), function(k) {
    
    res <- gravity_results[[k]]
    
    cbind(
      beta_scenarios[k, ],
      evaluate_scenario(
        od = res$od_matrix,
        distance_matrix = distance_matrix,
        P = P,
        A = A
      ),
      iterations = res$iterations,
      converged = res$converged
    )
  })
)

print(scenario_evaluation)

#od matrix final == od_matrix_h10
od_matrix_final <- od_matrix_h10

#convert matrix into dataframe
od_matrix_final_df <- as.data.frame(as.table(od_matrix_final)) |>
  dplyr::rename(
    origin = Var1,
    destination = Var2,
    trips = Freq
  ) |>
  dplyr::mutate(
    origin = as.character(origin),
    destination = as.character(destination)
  )

rw_attributes <- jakarta_rw %>%
  st_drop_geometry() %>%
  select(KDFPUM,WADMKK,WADMKC,WADMKD,WADMRW,centroid_x,centroid_y)

od_trip_attributes <- od_matrix_final_df %>%
  left_join(rw_attributes, by= c("origin" = "KDFPUM")) %>%
  select(
    kota_origin      = WADMKK,
    kecamatan_origin = WADMKC,
    kelurahan_origin = WADMKD,
    RW_origin        = WADMRW,
    centroid_x_origin = centroid_x,
    centroid_y_origin = centroid_y,
    origin,
    destination,
    trips
  )

rw_attributesA <- jakarta_rwA %>%
  st_drop_geometry() %>%
  select(KDFPUM,WADMKK,WADMKC,WADMKD,WADMRW,centroid_x,centroid_y)

od_trip_attributes2 <- od_trip_attributes %>%
  left_join(rw_attributesA, by =c("destination" = "KDFPUM")) %>%
  select(
    kota_origin,
    kecamatan_origin,
    kelurahan_origin,
    RW_origin,
    centroid_x_origin,
    centroid_y_origin,
    origin,
    destination,
    kota_destination = WADMKK,
    kecamatan_destination = WADMKC,
    kelurahan_destination = WADMKD,
    RW_destination        = WADMRW,
    centroid_x_destination = centroid_x,
    centroid_y_destination = centroid_y,
    trips
  )
