#Trip Attraction
library("sf")
library("dplyr")
library("tmap")

#load spatial dataset (zone/RW boundary) from jakartasatu
jakarta_rwA <- st_read("D:/Github/schoolMobility/Data/batas_rw.gpkg") %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(32748) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  filter(WADMKK != "Kab. Adm. Kep. Seribu")
jakarta_kotaA <- st_read("D:/Github/schoolMobility/Data/batas_wil.gpkg") %>%
  st_zm(drop = TRUE) %>%
  st_make_valid() %>%
  st_transform(32748) %>%
  filter(WADMKK != "Kab. Adm. Kep. Seribu")
jakarta_gunalahanA <- st_read("D:/Github/schoolMobility/Data/guna_lahan.gpkg") %>%
  st_make_valid() %>%
  st_transform(32748)

jakarta_commercial <- jakarta_gunalahanA %>%
  filter(D_SUB_PENG %in% c("PERKANTORAN DAN PERDAGANGAN",
                           "PEMERINTAHAN",
                           "LAYANAN PENDIDIKAN",
                           "PELAYANAN KESEHATAN",
                           "JASA",
                           "WISATA",
                           "PERHOTELAN",
                           "KEAGAMAAN",
                           "BUDAYA",
                           "PASAR TRADISIONAL",
                           "PELAYANAN UMUM",
                           "INDUSTRI",
                           "PENYIMPANAN",
                           "PERTANIAN",
                           "PERIKANAN",
                           "PETERNAKAN"
                           ))

jakarta_commercial <- jakarta_commercial %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_sf()

jakarta_commercial_rw <- st_intersection(
  jakarta_rwA,
  jakarta_commercial
)

jakarta_commercial_rw <- jakarta_commercial_rw %>%
  mutate(
    commercial_area_m2 = as.numeric(st_area(geom))
  )

luas_commercial_rw <- jakarta_commercial_rw %>%
  mutate(commercial_area_m2 = as.numeric(st_area(geom))) %>%
  st_drop_geometry() %>%
  group_by(WADMKK, WADMKC, WADMKD, WADMRW) %>%
  summarise(
    total_commercial_area_m2 = sum(commercial_area_m2, na.rm = TRUE),
    jumlah_commercial = n(),
    .groups = "drop"
  )

jakarta_rwA <- jakarta_rwA %>%
  left_join(
    luas_commercial_rw,
    by = c("WADMKK", "WADMKC", "WADMKD", "WADMRW")
  )

jakarta_rwA$total_commercial_area_m2[is.na(jakarta_rwA$total_commercial_area_m2)] <- 0
jakarta_rwA$jumlah_commercial[is.na(jakarta_rwA$jumlah_commercial)] <- 0

total_commercial <- sum(jakarta_rwA$total_commercial_area_m2, na.rm = TRUE)
#total_penduduk_produktif = 9513123
#trip_production = 26279826

jakarta_rwA <- jakarta_rwA %>%
  mutate(
    index_attraction = total_commercial_area_m2 / total_commercial,
    trip_attraction = ceiling(index_attraction * total_penduduk_produktif)
  )

trip_attraction_total <- sum(jakarta_rwA$trip_attraction, na.rm = TRUE)
rasio_PA <- trip_production/trip_attraction_total

jakarta_rwA <- jakarta_rwA %>%
  mutate(
    trip_attraction_proportion = rasio_PA*trip_attraction
  )

trip_attraction_total2 <- sum(jakarta_rwA$trip_attraction_proportion, na.rm = TRUE)

#centroid point for trip attraction
centroid_commercial <- jakarta_commercial_rw %>%
  group_by(WADMKK, WADMKC, WADMKD, WADMRW) %>%
  slice_max(order_by = commercial_area_m2, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  st_centroid()

centroid_coordsA <- centroid_commercial %>%
  mutate(
    centroid_x = st_coordinates(geom)[, 1],
    centroid_y = st_coordinates(geom)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(WADMKK, WADMKC, WADMKD, WADMRW, centroid_x, centroid_y, max_commercial_area_m2 = commercial_area_m2)

jakarta_rwA <- jakarta_rwA %>%
  left_join(
    centroid_coordsA,
    by = c("WADMKK", "WADMKC", "WADMKD", "WADMRW")
  )

#visualisasi
tmap_mode("view")

tm_shape(jakarta_rwA) +
  tm_polygons(
    fill = "trip_attraction_proportion",
    fill.scale = tm_scale_continuous(values = "YlOrRd"),
    fill.legend = tm_legend(title = "Trip Attraction"),
    col = "gray80",
    lwd = 0.3
  ) 
  tm_shape(jakarta_rwA, coords = c("centroid_x", "centroid_y")) +
  tm_dots(
    fill = "blue",
    size = 0.4,
    popup.vars = c(
      "Kota"               = "WADMKK",
      "Kecamatan"          = "WADMKC",
      "Kelurahan"          = "WADMKD",
      "RW"                 = "WADMRW",
      "Trip Production"    = "trip_attraction_proportion"
    )
  )

