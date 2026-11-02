#jakarta network
jakarta_network <- st_read(
  "D:/Github/schoolMobility/Data/jakarta_network.gpkg"
)

pedestrian_classes <- c(
  "footway", "path", "pedestrian","steps",
  "living_street", "residential"
)

jakarta_pedestrian <- jakarta_network %>%
  filter(fclass %in% pedestrian_classes) %>%
  st_cast("LINESTRING") %>%
  mutate(
    oneway = FALSE,
    length_m = as.numeric(st_length(geom)),
    edge_id = row_number()
  )

jakarta_pedestrian_4326 <- jakarta_pedestrian %>%
  st_transform(4326)



# ==============================================================================
# GAP ANALYSIS: DEMAND PEJALAN KAKI vs KETERSEDIAAN FASILITAS PEDESTRIAN
#
# Input:
#   edges_all               : sf ruas dengan flow_total (hasil merge_pedestrian_flows.R)
#   jakarta_pedestrian_4326 : sf jaringan pedestrian (kolom fclass), EPSG:4326
#   (opsional) od_routes    : OD pendekatan 1 (distance km, centroid UTM 32748)
# ==============================================================================


TOL_M          <- 15      # toleransi jarak ruas demand ke fasilitas (meter) -> uji 10/15/25
DEDICATED      <- c("footway", "path", "pedestrian", "steps")
SHARED_STREET  <- c("living_street", "residential")
TOP_Q          <- 0.80    # kuantil flow untuk "demand tinggi" (top 20%)

# ------------------------------------------------------------------------------
# 1. PROYEKSI KE METER & TITIK TENGAH RUAS DEMAND
# ------------------------------------------------------------------------------
edges_utm <- edges_all %>%
  filter(!is.na(flow_total), flow_total > 0) %>%
  st_transform(32748)
edges_utm$len_m <- as.numeric(st_length(edges_utm))
mid <- st_centroid(st_geometry(edges_utm))     # ruas 2 titik: centroid = titik tengah

ped_utm <- st_transform(jakarta_pedestrian_4326, 32748)
ped_ded <- ped_utm %>% filter(fclass %in% DEDICATED)
ped_shr <- ped_utm %>% filter(fclass %in% SHARED_STREET)

near_dist <- function(pts, lines) {
  g <- st_geometry(lines)                 # ambil sfc, bukan data.frame sf
  stopifnot(length(g) > 0)                # kelas fasilitas tidak boleh kosong
  idx <- st_nearest_feature(pts, g)
  as.numeric(st_distance(pts, g[idx], by_element = TRUE))
}
cat("Jumlah fitur dedicated:", nrow(ped_ded), "| shared:", nrow(ped_shr), "\n")
edges_utm$d_ded <- near_dist(mid, ped_ded)
edges_utm$d_shr <- near_dist(mid, ped_shr)

# ------------------------------------------------------------------------------
# 2. DIAGNOSIS: apakah graph_with_od dibangun dari jaringan pedestrian ini?
#    Jika hampir semua ruas berjarak ~0 m ke fasilitas, perbandingan bersifat
#    tautologis (demand hanya bisa muncul di ruas yang ada di jaringan).
# ------------------------------------------------------------------------------
cat("Ruas demand dgn jarak < 1 m ke jaringan pedestrian (semua kelas):",
    round(100 * mean(pmin(edges_utm$d_ded, edges_utm$d_shr) < 1), 1), "%\n")
print(summary(pmin(edges_utm$d_ded, edges_utm$d_shr)))

# ------------------------------------------------------------------------------
# 3. KLASIFIKASI SUPPLY PER RUAS
# ------------------------------------------------------------------------------
edges_utm <- edges_utm %>%
  mutate(supply = case_when(
    d_ded <= TOL_M ~ "dedicated",             # footway/path/pedestrian/steps
    d_shr <= TOL_M ~ "shared_street",         # residential/living_street/track
    TRUE           ~ "no_facility_in_data"    # bukan berarti pasti tidak ada trotoar
  ))

thr <- quantile(edges_utm$flow_total, TOP_Q, na.rm = TRUE)
edges_utm <- edges_utm %>%
  mutate(priority = case_when(
    flow_total >= thr & supply == "no_facility_in_data" ~ "1. High demand - without facility",
    flow_total >= thr & supply == "shared_street"       ~ "2. High demand - using shared street",
    flow_total >= thr & supply == "dedicated"           ~ "3. High demand - dedicated facility",
    TRUE                                                ~ "4. other demand"
  ))

# ------------------------------------------------------------------------------
# 4. RINGKASAN (ped-km = flow x panjang ruas, ukuran paparan pejalan kaki)
# ------------------------------------------------------------------------------
summ_supply <- edges_utm %>% st_drop_geometry() %>%
  group_by(supply) %>%
  summarise(n_edges = n(), km = sum(len_m) / 1000,
            flow_sum = sum(flow_total), ped_km = sum(flow_total * len_m) / 1000,
            .groups = "drop") %>%
  mutate(pct_edges  = round(100 * n_edges / sum(n_edges), 1),
         pct_ped_km = round(100 * ped_km / sum(ped_km), 1))
print(summ_supply)

summ_priority <- edges_utm %>% st_drop_geometry() %>%
  group_by(priority) %>%
  summarise(n_edges = n(), km = sum(len_m) / 1000,
            ped_km = sum(flow_total * len_m) / 1000, .groups = "drop") %>%
  mutate(pct_ped_km = round(100 * ped_km / sum(ped_km), 1))
print(summ_priority)

# ------------------------------------------------------------------------------
# 5. PETA PRIORITAS
# ------------------------------------------------------------------------------
tmap_mode("view")
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(edges_utm %>% filter(priority != "4. other demand") %>% st_transform(4326)) +
  tm_lines(
    col = "priority",
    col.scale = tm_scale_categorical(values = c("#d7191c", "#B3A300", "#1a9641")),
    lwd = 2,
    lwd.scale = tm_scale_continuous(values.range = c(0.7, 6)),
    col.legend = tm_legend(title = "Priority"),
    lwd.legend = tm_legend_hide()
    ) +
  tm_shape(jakarta_kota_4326) +
  tm_lines(
    col = "blue",
    lwd = 1
  )

tm_basemap("Esri.WorldGrayCanvas") +
tm_shape(edges_utm %>% filter(priority != "4. demand lainnya") %>% st_transform(4326))+
  tm_lines(
    col = "priority",
    col.scale = tm_scale_categorical(values = c("#d7191c", "#fff042", "#1a9641")),
    lwd = 1,
    lwd.scale  = tm_scale_continuous(values.range = c(0.5, 6)),
    lwd.legend = tm_legend_hide()
  )+
  tm_shape(jakarta_kota_4326) +
  tm_lines(
    col = "blue",
    lwd = 2
  )

# ==============================================================================
# DBSCAN PADA RUAS PRIORITAS 1 + VISUALISASI LINGKARAN TIAP KLASTER
#
# Lanjutan dari kode gap analysis Anda. Objek yang dibutuhkan di environment:

PRIORITY_LABEL <- "1. High demand - without facility"

EPS         <- 300                 # m, radius ketetanggaan antar titik tengah ruas
MINPTS      <- 10                  # jumlah ruas minimum dalam radius EPS (termasuk dirinya)
EPS_SENS    <- c(150, 200, 300, 500, 750)
MINPTS_SENS <- c(5, 10, 20)

RADIUS_Q      <- 1                 # kuantil jarak anggota ke pusat: 1 = lingkaran menutup semua anggota;
# 0.9 = abaikan 10% anggota terjauh (kurang sensitif terhadap pencilan)
MIN_RADIUS_M  <- 100               # radius minimum lingkaran
WEIGHT_CENTER <- TRUE              # pusat lingkaran dibobot ped-km (FALSE = pusat geometris biasa)

# ------------------------------------------------------------------------------
# 1. RUAS PRIORITAS 1 & TITIK TENGAH (UTM, meter)
# ------------------------------------------------------------------------------
p1 <- edges_utm %>% filter(priority == PRIORITY_LABEL)
xy <- st_coordinates(st_centroid(st_geometry(p1)))
p1$x <- xy[, 1]; p1$y <- xy[, 2]
cat("Ruas prioritas 1:", nrow(p1), "| median panjang ruas:", round(median(p1$len_m)), "m\n")
# minPts dihitung dalam JUMLAH RUAS: 10 ruas x ~50 m = sekitar 500 m jalan di sekitar titik

# ------------------------------------------------------------------------------
# 2. PEMILIHAN PARAMETER: grafik k-tetangga & tabel sensitivitas
# ------------------------------------------------------------------------------
kNNdistplot(xy, k = MINPTS - 1)    # siku kurva = kandidat EPS
abline(h = EPS, lty = 2)

sens <- rbindlist(lapply(EPS_SENS, function(e) rbindlist(lapply(MINPTS_SENS, function(m) {
  cl  <- dbscan(xy, eps = e, minPts = m)$cluster
  tab <- if (any(cl > 0)) tapply(p1$len_m[cl > 0], cl[cl > 0], sum) else 0
  data.table(eps = e, minPts = m,
             n_klaster         = length(setdiff(unique(cl), 0L)),
             pct_ruas_noise    = round(100 * mean(cl == 0), 1),
             pct_panjang_noise = round(100 * sum(p1$len_m[cl == 0]) / sum(p1$len_m), 1),
             klaster_terbesar_pct_panjang = round(100 * max(tab) / sum(p1$len_m), 1))  # indikator chaining
}))))
print(sens)

# ------------------------------------------------------------------------------
# 3. KLASTER FINAL
# ------------------------------------------------------------------------------
db <- dbscan(xy, eps = EPS, minPts = MINPTS)
p1$cluster <- db$cluster
if (!any(p1$cluster > 0)) stop("Tidak ada klaster; kecilkan MINPTS atau besarkan EPS.")

noise <- p1 %>% st_drop_geometry() %>% filter(cluster == 0)
cat(sprintf("Klaster: %d | noise: %.1f%% ruas, %.1f%% panjang, %.1f%% ped-km\n",
            n_distinct(p1$cluster[p1$cluster > 0]),
            100 * nrow(noise) / nrow(p1),
            100 * sum(noise$len_m) / sum(p1$len_m),
            100 * sum(noise$flow_total * noise$len_m) / sum(p1$flow_total * p1$len_m)))

# ------------------------------------------------------------------------------
# 4. LINGKARAN TIAP KLASTER: pusat (opsional dibobot), radius dari jarak anggota
# ------------------------------------------------------------------------------
mem <- p1 %>% st_drop_geometry() %>% filter(cluster > 0) %>%
  mutate(w = if (WEIGHT_CENTER) flow_total * len_m else 1)

circ <- mem %>% group_by(cluster) %>%
  summarise(n_ruas = n(),
            km     = sum(len_m) / 1000,
            ped_km = sum(flow_total * len_m) / 1000,
            cx = weighted.mean(x, w), cy = weighted.mean(y, w),
            med_len = median(len_m),
            .groups = "drop")

rad <- mem %>%
  left_join(circ %>% select(cluster, cx, cy), by = "cluster") %>%
  mutate(dist_c = sqrt((x - cx)^2 + (y - cy)^2)) %>%
  group_by(cluster) %>%
  summarise(r_members = quantile(dist_c, RADIUS_Q), .groups = "drop")

circ <- circ %>%
  left_join(rad, by = "cluster") %>%
  mutate(radius_m = pmax(r_members + 0.5 * med_len, MIN_RADIUS_M),   # + setengah panjang ruas
         flow_rata2_per_km = round(ped_km / km)) %>%
  arrange(desc(ped_km)) %>%
  mutate(rank = row_number())

pts <- st_as_sf(circ, coords = c("cx", "cy"), crs = 32748, remove = FALSE)
circ_sf <- pts
st_geometry(circ_sf) <- st_sfc(
  Map(function(g, r) st_buffer(g, r, nQuadSegs = 30), st_geometry(pts), pts$radius_m),
  crs = 32748)

print(circ %>% select(rank, cluster, n_ruas, km, ped_km, flow_rata2_per_km, radius_m) %>% head(20))

# ------------------------------------------------------------------------------
# 5. PETA: lingkaran klaster (warna = ped-km), ruas anggota, dan noise
# ------------------------------------------------------------------------------
circ4326    <- st_transform(circ_sf, 4326)
lines_cl    <- st_transform(p1 %>% filter(cluster > 0),  4326)
lines_noise <- st_transform(p1 %>% filter(cluster == 0), 4326)

# Pisahkan 10 klaster dengan rank tertinggi
circ_top10 <- circ4326 %>% filter(rank <= 10)
circ_other <- circ4326 %>% filter(rank > 10)

# Gradasi konsisten untuk SEMUA klaster:
# ped-km rendah = merah cerah
# ped-km tinggi = merah pekat
red_palette <- c(
  "#FB6A4A",
  "#EF3B2C",
  "#CB181D",
  "#A50F15",
  "#67000D"
)


tmap_mode("view")
tm_basemap("Esri.WorldGrayCanvas") +
tm_shape(jakarta_road_4326)+
  tm_lines(
    col = "blue",
    lwd = 1
  ) +
  # Ruas noise
  tm_shape(lines_noise) +
  tm_lines(col = "grey65", lwd = 1) +
  
  # Ruas anggota klaster
  tm_shape(lines_cl) +
  tm_lines(col = "#E53935", lwd = 1.5) +
  
  # Semua lingkaran: quantile ped_km yang sama
  tm_shape(circ4326) +
  tm_polygons(
    fill = "ped_km",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      values = red_palette
    ),
    fill_alpha = 0.40,
    col = "white",
    lwd = 1,
    fill.legend = tm_legend(title = "Ped-km cluster"),
    popup.vars = c(
      "rank", "n_ruas", "km", "ped_km",
      "flow_rata2_per_km", "radius_m"
    )
  ) +
  
  # Garis tepi khusus rank 1–10
  tm_shape(circ_top10) +
  tm_borders(
    col = "#252525",
    lwd = 3
  ) +
  
  # Label khusus rank 1–10
  tm_shape(circ_top10) +
  tm_text(
    "rank",
    size = 0.9,
    col = "#252525"
  )+
  tm_shape(jakarta_kota_4326) +
  tm_lines(
    col = "blue",
    lwd = 1
  )




