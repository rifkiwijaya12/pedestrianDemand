#TRIP PRODUCTION
#load spatial dataset (zone/RW boundary) from jakartasatu
jakarta_rw <- st_read("D:/Github/schoolMobility/Data/batas_rw.gpkg") %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(32748) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  filter(WADMKK != "Kab. Adm. Kep. Seribu")
jakarta_kota <- st_read("D:/Github/schoolMobility/Data/batas_wil.gpkg") %>%
  st_zm(drop = TRUE) %>%
  st_make_valid() %>%
  st_transform(32748) %>%
  filter(WADMKK != "Kab. Adm. Kep. Seribu")
jakarta_gunalahan <- st_read("D:/Github/schoolMobility/Data/guna_lahan.gpkg") %>%
  st_make_valid() %>%
  st_transform(32748)

#load data penduduk
data_penduduk <- read_excel("D:/Github/pedestrianDemand/Data/Data_Penduduk.xlsx")

usia_produktif <- c("05-09", "10-14", "15-19", "20-24", "25-29", "30-34", 
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")

jakarta_penduduk <- data_penduduk %>%
  filter(periode_data == 2025, 
         kelompok_umur %in% usia_produktif,
         wilayah != "KAB. ADM. KEP. SERIBU") %>%
  group_by(wilayah, kecamatan, kelurahan) %>%
  summarise(penduduk_produktif_kelurahan = sum(jumlah_penduduk, na.rm = TRUE)) %>%
  ungroup()

total_penduduk_produktif <- sum(jakarta_penduduk$penduduk_produktif_kelurahan, na.rm = TRUE)

jakarta_rw <- jakarta_rw %>%
  mutate(
    WADMKK_upper = toupper(WADMKK),
    WADMKC_upper = toupper(WADMKC),
    WADMKD_upper = toupper(WADMKD)
  ) %>%
  left_join(
    jakarta_penduduk,
    by = c(
      "WADMKK_upper" = "wilayah",
      "WADMKC_upper" = "kecamatan",
      "WADMKD_upper" = "kelurahan"
    )
  ) %>%
  select(-WADMKK_upper, -WADMKC_upper, -WADMKD_upper)

#filter gunalahan hanya hunian
jakarta_housing <- jakarta_gunalahan %>%
  filter(D_SUB_PENG %in% c("HUNIAN", "CAMPURAN")) %>%
  st_make_valid() %>%
  st_transform(32748)

jakarta_housing <- jakarta_housing %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_sf()

#intersection jakarta rw and housing
jakarta_housing_rw <- st_intersection(
  jakarta_rw,
  jakarta_housing
)

jakarta_housing_rw <- jakarta_housing_rw %>%
  mutate(
    housing_area_m2 = as.numeric(st_area(geom))
  )

luas_housing_rw <- jakarta_housing_rw %>%
  mutate(housing_area_m2 = as.numeric(st_area(geom))) %>%
  st_drop_geometry() %>%
  group_by(WADMKK, WADMKC, WADMKD, WADMRW) %>%
  summarise(
    total_housing_area_m2 = sum(housing_area_m2, na.rm = TRUE),
    jumlah_housing = n(),
    .groups = "drop"
  )

jakarta_rw <- jakarta_rw %>%
  left_join(
    luas_housing_rw,
    by = c("WADMKK", "WADMKC", "WADMKD", "WADMRW")
  )

jakarta_rw <- jakarta_rw %>%
  group_by(WADMKK, WADMKC, WADMKD) %>%
  mutate(
    total_housing_kelurahan = sum(total_housing_area_m2, na.rm = TRUE),
    penduduk_produktif_rw = ceiling(ifelse(
      total_housing_kelurahan > 0,
      (total_housing_area_m2 / total_housing_kelurahan) * penduduk_produktif_kelurahan,
      0
    ))
  ) %>%
  ungroup()

jakarta_rw <- jakarta_rw %>%
  mutate(
    Index_GL = (total_housing_area_m2 * 0.0001) / LUASWH,
    GL_minus_Med = Index_GL - median(Index_GL, na.rm = TRUE),
    trip_production = ceiling(penduduk_produktif_rw * (2.8+GL_minus_Med))
  ) %>%
  mutate(trip_production = if_else(is.na(trip_production), 0, trip_production))

#centroid point
key_rw <- c("WADMKK", "WADMKC", "WADMKD", "WADMRW")

housing_union_rw <- jakarta_housing_rw %>%
  st_collection_extract("POLYGON") %>%     # buang LINESTRING/POINT hasil intersection
  group_by(across(all_of(key_rw))) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%   # st_union per RW
  st_make_valid()

poly <- st_geometry(housing_union_rw)

cen <- st_centroid(poly)

inside <- diag(st_within(cen, poly, sparse = FALSE))

np      <- st_nearest_points(cen, poly, pairwise = TRUE)
pt_snap <- st_cast(np, "POINT")[seq(2, 2 * length(np), by = 2)]

xy_cen  <- st_coordinates(cen)
xy_snap <- st_coordinates(pt_snap)
xy_final <- xy_cen
xy_final[!inside, ] <- xy_snap[!inside, ]

centroid_coords <- housing_union_rw %>%
  st_drop_geometry() %>%
  mutate(
    centroid_x   = xy_final[, 1],
    centroid_y   = xy_final[, 2],
    cen_di_dalam = inside        # TRUE = centroid asli; FALSE = sudah digeser
  )

rw_pos <- jakarta_rw %>%
  st_point_on_surface() %>%
  mutate(rx = st_coordinates(.)[, 1],
         ry = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(all_of(key_rw), rx, ry)

jakarta_rw <- jakarta_rw %>%
  left_join(centroid_coords, by = key_rw) %>%
  left_join(rw_pos, by = key_rw) %>%
  mutate(
    ada_hunian = !is.na(centroid_x),
    centroid_x = coalesce(centroid_x, rx),
    centroid_y = coalesce(centroid_y, ry)
  ) %>%
  select(-rx, -ry)

trip_production <- sum(jakarta_rw$trip_production, na.rm = TRUE)


jakarta_rw_test <- jakarta_rw %>%
  filter(
    KDFPUM == c(
      "31.73.06.1003.001"
    )
  )
#visualize jakarta_housing
tmap_mode("view")
tm_shape(jakarta_rw)+
  tm_polygons(
    col = "red",
    lwd = 0.3
  )+
tm_shape(jakarta_housing_rw) +
  tm_polygons(
    col = "gray80",
    lwd = 0.3
  ) +
  tm_shape(jakarta_rw,
           coords = c("centroid_x", "centroid_y")) +
  tm_dots(
    fill = "blue",
    size = 0.4,
    popup.vars = c(
      "Kota"               = "WADMKK",
      "Kecamatan"          = "WADMKC",
      "Kelurahan"          = "WADMKD",
      "RW"                 = "WADMRW",
      "Trip Production"    = "trip_production"
    )
  )
