#TRIP ATTRACTION
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
key_rw <- c("WADMKK", "WADMKC", "WADMKD", "WADMRW")

commercial_union_rw <- jakarta_commercial_rw %>%
  st_collection_extract("POLYGON") %>%
  group_by(across(all_of(key_rw))) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  st_make_valid()

polyA <- st_geometry(commercial_union_rw)

cenA <- st_centroid(polyA)

insideA <- diag(st_within(cenA, polyA, sparse = FALSE))

npA      <- st_nearest_points(cenA, polyA, pairwise = TRUE)
pt_snapA <- st_cast(npA, "POINT")[seq(2, 2 * length(npA), by = 2)]

xy_cenA   <- st_coordinates(cenA)
xy_snapA  <- st_coordinates(pt_snapA)
xy_finalA <- xy_cenA
xy_finalA[!insideA, ] <- xy_snapA[!insideA, ]

centroid_coordsA <- commercial_union_rw %>%
  st_drop_geometry() %>%
  mutate(
    centroid_x    = xy_finalA[, 1],
    centroid_y    = xy_finalA[, 2],
    cenA_di_dalam = insideA
  )

rw_posA <- jakarta_rwA %>%
  st_point_on_surface() %>%
  mutate(rx = st_coordinates(.)[, 1],
         ry = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(all_of(key_rw), rx, ry)

jakarta_rwA <- jakarta_rwA %>%
  left_join(centroid_coordsA, by = key_rw) %>%
  left_join(rw_posA, by = key_rw) %>%
  mutate(
    ada_atraksi = !is.na(centroid_x),
    centroid_x  = coalesce(centroid_x, rx),
    centroid_y  = coalesce(centroid_y, ry)
  ) %>%
  select(-rx, -ry)

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

