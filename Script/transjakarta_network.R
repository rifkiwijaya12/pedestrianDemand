#transjakarta network
library(readr)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tmap)


jakarta_kota <- st_read("D:/Github/schoolMobility/Data/batas_wil.gpkg") %>%
  st_zm(drop = TRUE) %>%
  st_make_valid() %>%
  st_transform(32748) %>%
  filter(WADMKK != "Kab. Adm. Kep. Seribu")

jakarta_kota_4326 <- jakarta_kota %>%
  st_transform(4326)

gtfs_dir <- "D:/Github/pedestrianDemand/Data/file_gtfs"

transjakarta_agency <- read_csv(
  file.path(gtfs_dir, "agency.txt"),
  show_col_types = FALSE
)

transjakarta_routes <- read_csv(
  file.path(gtfs_dir, "routes.txt"),
  show_col_types = FALSE
)

transjakarta_shape <- read_csv(
  file.path(gtfs_dir, "shapes.txt"),
  show_col_types = FALSE
)

transjakarta_stop <- read_csv(
  file.path(gtfs_dir, "stops.txt"),
  show_col_types = FALSE
)

transjakarta_stoptimes <- read_csv(
  file.path(gtfs_dir, "stop_times.txt"),
  show_col_types = FALSE
)

transjakarta_trip <- read_csv(
  file.path(gtfs_dir, "trips.txt"),
  show_col_types = FALSE
)

# 1. Shapes: titik-titik pembentuk lintasan
shape_data <- transjakarta_shape %>%
  mutate(
    shape_id = as.character(shape_id),
    shape_pt_sequence = as.numeric(shape_pt_sequence),
    shape_pt_lat = as.numeric(shape_pt_lat),
    shape_pt_lon = as.numeric(shape_pt_lon)
  ) %>%
  filter(
    !is.na(shape_id),
    !is.na(shape_pt_sequence),
    is.finite(shape_pt_lat),
    is.finite(shape_pt_lon),
    between(shape_pt_lat, -90, 90),
    between(shape_pt_lon, -180, 180)
  ) %>%
  arrange(shape_id, shape_pt_sequence)

# 2. Stops: lokasi halte
stop_data <- transjakarta_stop %>%
  mutate(
    stop_id = as.character(stop_id),
    stop_lat = as.numeric(stop_lat),
    stop_lon = as.numeric(stop_lon)
  ) %>%
  filter(
    !is.na(stop_id),
    is.finite(stop_lat),
    is.finite(stop_lon),
    between(stop_lat, -90, 90),
    between(stop_lon, -180, 180)
  ) %>%
  distinct(stop_id, .keep_all = TRUE)

# 3. Stop times: urutan pemberhentian
stoptimes_data <- transjakarta_stoptimes %>%
  mutate(
    trip_id = as.character(trip_id),
    stop_id = as.character(stop_id),
    stop_sequence = as.numeric(stop_sequence)
  )

# 4. Trips: informasi rute dan perjalanan
trip_data <- transjakarta_trip %>%
  mutate(
    trip_id = as.character(trip_id),
    route_id = as.character(route_id),
    shape_id = as.character(shape_id),
    direction_id = as.character(direction_id)
  )

# Kelompokkan titik berdasarkan shape_id
shape_groups <- split(shape_data, shape_data$shape_id)

# Pertahankan shapes yang memiliki minimal dua titik
shape_groups <- shape_groups[
  vapply(shape_groups, nrow, integer(1)) >= 2
]

# Buat LINESTRING dari koordinat longitude, latitude
shape_lines <- lapply(shape_groups, function(x) {
  
  coords <- as.matrix(
    x[, c("shape_pt_lon", "shape_pt_lat")]
  )
  
  st_linestring(coords)
})

# Gabungkan menjadi satu objek sf
shapes_sf <- st_sf(
  shape_id = names(shape_lines),
  geometry = st_sfc(shape_lines, crs = 4326)
)


stops_sf <- st_as_sf(
  stop_data,
  coords = c("stop_lon", "stop_lat"),
  crs = 4326,
  remove = FALSE
)

stops_sf <- st_intersection(
  stops_sf,
  jakarta_kota_4326
)

shapes_sf <- st_intersection(
  shapes_sf,
  jakarta_kota_4326
)
print(stops_sf)
plot(st_geometry(stops_sf))

tmap_mode("plot")

tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(shapes_sf) +
  tm_lines(
    col = "#2878B5",
    lwd = 1,
    alpha = 0.65,
    legend.show = FALSE
  ) +
  tm_shape(stops_sf) +
  tm_dots(
    col = "#D94841",
    size = 0.035,
    alpha = 1,
    legend.show = FALSE
  ) +
  tm_credits(
    paste0(
      "Source: GTFS TransJakarta | "
    ),
    position = c("left", "bottom"),
    size = 0.7
  ) +
  tm_layout(
    frame = FALSE,
    bg.color = "white",
    outer.margins = 0.02
  )+
    tm_shape(jakarta_kota_4326) +
    tm_lines(
      col = "black",
      lwd = 0.5
    )
