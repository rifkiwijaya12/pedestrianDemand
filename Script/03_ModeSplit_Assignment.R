#mode share
#based on assumption 30% of trip using public transport
library("osmdata")
library("stplanr")
library("sf")
library("dplyr")
library("igraph")

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
  ) %>%
  st_make_valid()

od_trip_PT <- od_trip_attributes2 %>%
  mutate(
    trip = ceiling(trip*0.3)
  )

od_trip_PTSamples <- od_trip_PT[sample(nrow(od_trip_PT), 1), ]

#OD points
origin_sf <- st_as_sf(
  od_trip_PTSamples,
  coords = c("centroid_x_origin", "centroid_y_origin"),
  crs = 32748
)

destination_sf <- st_as_sf(
  od_trip_PTSamples,
  coords = c("centroid_x_destination", "centroid_y_destination"),
  crs = 32748
)

jakarta_roads_4326 <- jakarta_roads %>%
  st_transform(4326)

origin_4326 <- st_transform(
  origin_sf, 4326)

destination_4326 <- st_transform(
  destination_sf, 4326)

#convert road network into graph
graph <- weight_streetnet(
  jakarta_roads_4326,
  wt_profile = "foot",
  type_col = "fclass"
)

#snap origin destination into network
origin_vertex <- match_points_to_verts(
  graph,
  origin_4326
)

destination_vertex <- match_points_to_verts(
  graph,
  destination_4326
)

#routing process
route <- dodgr_paths(
  graph,
  from = origin_vertex,
  to = destination_vertex
)

route_vertices <- route[[1]][[1]]

#convert to linestring

vertices <- dodgr_vertices(graph)

route_vertices_df <- vertices[
  match(
    as.numeric(route_vertices),
    vertices$id
  ),
]

route_matrix <- as.matrix(
  route_vertices_df[, c("x", "y")]
)

route_line <- st_linestring(
  route_matrix
)

route_sf <- st_sf(
  route_id = names(route[[1]]),
  geometry = st_sfc(
    route_line,
    crs = 4326
  )
)

#calculate distance
route_sf_utm <- st_transform(
  route_sf,
  32748
)

route_distance_m <- as.numeric(
  st_length(route_sf_utm)
)

route_distance_m

#visualisation
tmap_mode("view")

tm_shape(jakarta_kota) +
  tm_borders(
    col = "red",
    lwd = 0.7
  ) +
  
  tm_shape(route_sf) +
  tm_lines(
    col = "blue",
    lwd = 3
  ) +
  
  tm_shape(origin_4326) +
  tm_symbols(
    col = "green",
    size = 0.5
  ) +
  
  tm_shape(destination_4326) +
  tm_symbols(
    col = "red",
    size = 0.5
  ) 

#download data gtfs transjakarta
transjakarta_routes <- read.table("D:/Github/pedestrianDemand/Data/file_gtfs/routes.txt", header = TRUE, sep = ",")
transjakarta_stop <- read.table("D:/Github/pedestrianDemand/Data/file_gtfs/stops.txt", header = TRUE, sep = ",")
transjakarta_trip <- read.table("D:/Github/pedestrianDemand/Data/file_gtfs/trips.txt", header = TRUE, sep = ",", quote = "", comment.char = "")
transjakarta_stoptimes <- read.table("D:/Github/pedestrianDemand/Data/file_gtfs/stop_times.txt", header = TRUE, sep = ",", quote = "", comment.char = "")
transjakarta_shapes <- read.table("D:/Github/pedestrianDemand/Data/file_gtfs/shapes.txt", header = TRUE, sep = ",")

#spatial data for GTFS
stops_sf <- transjakarta_stop %>%
  st_as_sf(
    coords = c("stop_lon", "stop_lat"),
    crs = 4326
  )