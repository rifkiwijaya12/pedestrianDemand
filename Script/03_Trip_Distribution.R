#Trip Distribution
library("sf")
library("dplyr")
library("osmdata")
library("dodgr")
library("tidyr")
library("sfnetworks")
library("tidygraph")
library("igraph")
library("ggplot2")

#create origin destination point
origin <- jakarta_rw %>%
  mutate(
    zone_id = KDFPUM
  )

destination <- jakarta_rwA %>%
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

#create road network
jakarta_network <- st_read(
  "D:/Github/schoolMobility/Data/jakarta_network.gpkg"
)


road_classes <- c(
  "motorway",
  "motorway_link",
  "trunk",
  "trunk_link",
  "primary",
  "primary_link",
  "secondary",
  "secondary_link",
  "tertiary",
  "tertiary_link",
  "unclassified",
  "residential",
  "living_street"
)


jakarta_roads <- jakarta_network %>%
  filter(fclass %in% road_classes) %>%
  st_cast("LINESTRING") %>%
  mutate(
    oneway = FALSE,
    length_m = as.numeric(st_length(geom)),
    edge_id = row_number()
  )

#build network topology
road_network <- jakarta_roads %>%
  st_make_valid() %>%
  as_sfnetwork(directed = FALSE) %>%
  to_spatial_subdivision()

road_network <- road_network$subdivision


road_network <- road_network %>%
  activate("edges") %>%
  mutate(
    length_m = as.numeric(st_length(geom))
  )

#snap centroid zones to road
points_to_snap <- bind_rows(
  origin_points %>%
    select(zone_id, geometry) %>%
    mutate(point_type = "origin"),
  
  destination_points %>%
    select(zone_id, geometry) %>%
    mutate(point_type = "destination")
  )

road_network <- st_network_blend(
  road_network,
  points_to_snap
)

network_nodes <- road_network %>%
  activate("nodes") %>%
  st_as_sf()


origin_node <- st_nearest_feature(
  origin_points,
  network_nodes
)

destination_node <- st_nearest_feature(
  destination_points,
  network_nodes
)

#convert network to igraph
road_network <- road_network %>%
  activate("edges") %>%
  mutate(
    length_m = as.numeric(st_length(geom))
  )


g <- as.igraph(road_network)

edge_length <- road_network %>%
  activate("edges") %>%
  pull(length_m)


E(g)$length_m <- edge_length

#calculate network distance
destination_node_unique <- unique(destination_node)

# shortest path distance
distance_matrix_unique <- igraph::distances(
  g,
  v = origin_node,
  to = destination_node_unique,
  weights = E(g)$length_m,
  mode = "all"
)

destination_match <- match(
  destination_node,
  destination_node_unique
)

distance_matrix_m <- distance_matrix_unique[
  ,
  destination_match,
  drop = FALSE
]

max_valid_dist <- max(distance_matrix_m[is.finite(distance_matrix_m)], na.rm = TRUE)
penalty_dist <- max_valid_dist * 2

od_distance <- as.data.frame(distance_matrix_m) %>%
  mutate(
    origin_id = origin_points$zone_id
  ) %>%
  pivot_longer(
    cols = -origin_id,
    names_to = "destination_index",
    values_to = "distance_m"
  ) %>%
  mutate(
    destination_index = as.integer(
      gsub("V", "", destination_index)
    ),
    destination_id = destination_points$zone_id[
      destination_index
    ]
  ) %>%
  select(
    origin_id,
    destination_id,
    distance_m
  ) %>%
  mutate(
    distance_m = case_when(
      is.infinite(distance_m) | is.na(distance_m) ~ penalty_dist,
      TRUE ~ distance_m
    )
  )

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
         trip_attraction_proportion)

#calculate balance factor A and B
zones <- trip_production$KDFPUM
N <- length(zones)

P_vec <- trip_production$trip_production
A_vec <- trip_attraction$trip_attraction_proportion

distance_df <- od_distance %>%
  filter(origin_id %in% zones & destination_id %in% zones) %>%
  select(origin_id, destination_id, distance_m) %>%
  pivot_wider(names_from = destination_id, values_from = distance_m)

dist_matrix <- as.matrix(distance_df[, -1])
rownames(dist_matrix) <- distance_df$origin_id
dist_matrix <- dist_matrix[zones, zones]

#convert distance to km
dist_km <- dist_matrix / 1000

#calculate impedance factor =exp(-beta*d_km)
beta <- 0.2 #intial value 
F_matrix <- exp(-beta * dist_km)

B_vec <- rep(1.0, N)
A_vec_factor <- rep(1.0, N)

max_iter <- 200
tolerance <- 1e-5

for (iter in 1:max_iter) {
  A_vec_factor <- 1 / as.vector(F_matrix %*% (B_vec * A_vec))
  B_vec <- 1 / as.vector(t(F_matrix) %*% (A_vec_factor * P_vec))
  T_ij <- outer(A_vec_factor * P_vec, B_vec * A_vec) * F_matrix
  current_P <- rowSums(T_ij)
  current_A <- colSums(T_ij)
  
  max_error_P <- max(abs(current_P - P_vec))
  max_error_A <- max(abs(current_A - A_vec))
  
  if (iter %% 10 == 0 || iter == 1) {
    cat(sprintf("Iterasi %3d | Max Error P: %.6f | Max Error A: %.6f\n", 
                iter, max_error_P, max_error_A))
  }
  
  if (max_error_P < tolerance && max_error_A < tolerance) {
    cat(sprintf("\n--> KONVERGEN SENSE! Dicapai pada iterasi ke-%d\n", iter))
    break
  }
}

balancing_factor_A <- data.frame(
  KDFPUM = zones,
  P_i = P_vec,
  factor_A = A_vec_factor
)

balancing_factor_B <- data.frame(
  KDFPUM = zones,
  A_j = A_vec,
  factor_B = B_vec
)

rownames(T_ij) <- zones
colnames(T_ij) <- zones

#convert trip Tij into od trip (long format)
od_trips <- as.data.frame(T_ij) %>%
  mutate(origin_id = rownames(T_ij)) %>%
  pivot_longer(
    cols = -origin_id,
    names_to = "destination_id",
    values_to = "trip"
  ) %>%
  select(origin_id, destination_id, trip)

rw_attributes <- jakarta_rw %>%
  st_drop_geometry() %>%
  select(KDFPUM,WADMKK,WADMKC,WADMKD,WADMRW,centroid_x,centroid_y)

od_trip_attributes <- od_trips %>%
  left_join(rw_attributes, by= c("origin_id" = "KDFPUM")) %>%
  select(
    kota_origin      = WADMKK,
    kecamatan_origin = WADMKC,
    kelurahan_origin = WADMKD,
    RW_origin        = WADMRW,
    centroid_x_origin = centroid_x,
    centroid_y_origin = centroid_y,
    origin_id,
    destination_id,
    trip
  )

rw_attributesA <- jakarta_rwA %>%
  st_drop_geometry() %>%
  select(KDFPUM,WADMKK,WADMKC,WADMKD,WADMRW,centroid_x,centroid_y)

od_trip_attributes2 <- od_trip_attributes %>%
  left_join(rw_attributesA, by =c("destination_id" = "KDFPUM")) %>%
  select(
    kota_origin,
    kecamatan_origin,
    kelurahan_origin,
    RW_origin,
    centroid_x_origin,
    centroid_y_origin,
    origin_id,
    destination_id,
    kota_destination = WADMKK,
    kecamatan_destination = WADMKC,
    kelurahan_destination = WADMKD,
    RW_destination        = WADMRW,
    centroid_x_destination = centroid_x,
    centroid_y_destination = centroid_y,
    trip
  )

od_trip_kota <- od_trip_attributes2 %>%
  group_by(kota_origin, kota_destination) %>%
  summarise(
    trip = sum(trip, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(kota_origin, kota_destination)

#simulasi od kota
#create centroid of city
kota_centroids <- jakarta_kota %>%
  group_by(WADMKK) %>%
  summarise(geom = st_union(geom), .groups = "drop") %>%
  st_centroid() %>%
  mutate(
    centroid_x = st_coordinates(.)[, 1],
    centroid_y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(WADMKK, centroid_x, centroid_y)

kota_centroids_sf <- st_as_sf(
  kota_centroids,
  coords = c("centroid_x", "centroid_y"),
  crs = st_crs(jakarta_kota)
)

od_flow_df <- od_trip_kota %>%
  left_join(kota_centroids, by = c("kota_origin" = "WADMKK")) %>%
  rename(x_orig = centroid_x, y_orig = centroid_y) %>%
  left_join(kota_centroids, by = c("kota_destination" = "WADMKK")) %>%
  rename(x_dest = centroid_x, y_dest = centroid_y)

#visualisasi
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. MENAMBAHKAN PARALLEL OFFSET PADA KOORDINAT GARIS LURUS
# ==============================================================================

# Jarak pergeseran garis (semakin besar nilainya, semakin renggang jarak antar garis)
offset_dist <- 500  # Sesuaikan nilai ini jika jarak antar garis terlalu rapat/renggang

od_flow_offset <- od_flow_df %>%
  filter(kota_origin != kota_destination) %>%
  mutate(
    dx = x_dest - x_orig,
    dy = y_dest - y_orig,
    len = sqrt(dx^2 + dy^2),
    perp_x = -dy / len,
    perp_y =  dx / len,
    x_orig_off = x_orig + perp_x * offset_dist,
    y_orig_off = y_orig + perp_y * offset_dist,
    x_dest_off = x_dest + perp_x * offset_dist,
    y_dest_off = y_dest + perp_y * offset_dist
  )

# ==============================================================================
# 2. VISUALISASI DENGAN GGPLOT2 (GARIS LURUS SEJAJAR / PARALLEL STRAIGHT LINES)
# ==============================================================================

ggplot() +
  # A. Layer Peta Dasar Kota
  geom_sf(data = jakarta_kota, fill = "gray95", color = "gray75", size = 0.3) +
  
  # B. Layer Garis Lurus Sejajar (Ketebalan Statis/Sama = 0.8)
  geom_segment(
    data = od_flow_offset,
    aes(
      x = x_orig_off, y = y_orig_off,
      xend = x_dest_off, yend = y_dest_off,
      color = trip  # Hanya warna yang merepresentasikan jumlah trip
    ),
    linewidth = 2, # Ketebalan garis seragam untuk seluruh rute
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    alpha = 0.85,
    lineend = "round"
  ) +
  
  # C. Layer Titik Centroid Kota
  geom_point(
    data = kota_centroids,
    aes(x = centroid_x, y = centroid_y),
    color = "black", size = 3.5
  ) +
  
  # D. Layer Label Nama Kota
  geom_text(
    data = kota_centroids,
    aes(x = centroid_x, y = centroid_y, label = WADMKK),
    vjust = -1.2, fontface = "bold", size = 3.5
  ) +
  
  # E. Skala Warna Saja (Skala Ukuran Dihapus)
  scale_color_viridis_c(
    option = "magma", 
    name = "Jumlah Trip",
    labels = scales::label_comma()
  ) +
  
  # F. Estetika Tema Peta Bersih
  labs(
    title = "Peta Pergerakan Perjalanan Antar-Kota di Jakarta",
    subtitle = "Matriks Asal-Tujuan (OD Flow Map) Garis Lurus Sejajar Berdampingan",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, margin = margin(b = 10)),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. PERSIAPAN DATA (MENGHAPUS INTRA-RW TRIP JIKA DIPERLUKAN)
# ==============================================================================

od_rw_lines <- od_trip_attributes2 %>%
  # Filter trip internal (intra-RW) dan hilangkan trip bernilai 0
  filter(origin_id != destination_id, trip > 0)

# ==============================================================================
# 2. VISUALISASI HEATMAP MATRIKS OD RW DENGAN GGPLOT2
# ==============================================================================

ggplot() +
  # A. Layer Peta Dasar Wilayah Jakarta
  geom_sf(data = jakarta_kota, fill = "gray95", color = "gray80", size = 0.2) +
  
  # B. Layer Garis Lurus OD RW (Warna Berdasarkan Trip)
  geom_segment(
    data = od_rw_lines,
    aes(
      x = centroid_x_origin, 
      y = centroid_y_origin,
      xend = centroid_x_destination, 
      yend = centroid_y_destination,
      color = trip
    ),
    linewidth = 0.2, # Ketebalan tipis agar tumpukan garis tidak menutupi peta
    alpha = 0.15,    # Transparansi agar area padat garis terlihat membentuk intensitas
    lineend = "round"
  ) +
  
  # C. Skala Warna Gradasi
  scale_color_viridis_c(
    option = "magma",
    trans = "log1p", # Mencegah dominasi nilai ekstrem pada visualisasi
    name = "Jumlah Trip",
    labels = scales::label_comma()
  ) +
  
  # D. Tema dan Judul Peta Bersih
  labs(
    title = "Peta Alur Perjalanan (OD Flow Map) Tingkat Sub-Village (RW)",
    subtitle = "Visualisasi Garis Lurus Interaksi Perjalanan Antar-RW di Jakarta",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, margin = margin(b = 10)),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )
