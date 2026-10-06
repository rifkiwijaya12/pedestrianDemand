library("readxl")
library("sf")
library("dplyr")
library("tmap")

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
centroid_housing <- jakarta_housing_rw %>%
  group_by(WADMKK, WADMKC, WADMKD, WADMRW) %>%
  slice_max(order_by = housing_area_m2, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  st_centroid()

centroid_coords <- centroid_housing %>%
  mutate(
    centroid_x = st_coordinates(geom)[, 1],
    centroid_y = st_coordinates(geom)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(WADMKK, WADMKC, WADMKD, WADMRW, centroid_x, centroid_y, max_housing_area_m2 = housing_area_m2)

jakarta_rw <- jakarta_rw %>%
  left_join(
    centroid_coords,
    by = c("WADMKK", "WADMKC", "WADMKD", "WADMRW")
  )
trip_production <- sum(jakarta_rw$trip_production, na.rm = TRUE)

#visualize jakarta_housing
tmap_mode("view")

tm_shape(jakarta_rw) +
  tm_polygons(
    fill = "trip_production",
    fill.scale = tm_scale_continuous(values = "YlOrRd"),
    fill.legend = tm_legend(title = "Trip Production"),
    col = "gray80",
    lwd = 0.3
  ) +
  tm_shape(jakarta_rw, coords = c("centroid_x", "centroid_y")) +
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

