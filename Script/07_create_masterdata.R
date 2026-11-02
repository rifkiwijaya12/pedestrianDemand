# ==============================================================================
# 07_export_master.R
# Membangun 1 DATAFRAME MASTER (1 baris = 1 ruas) untuk seluruh visualisasi web app
#
# Objek yang dibutuhkan di environment (dari skrip 00-06):
#   edges_all               : sf ruas + flow_* (skrip 03, kunci: edge_id, a, b)
#   graph_with_od           : graph dodgr (untuk panjang ruas, kunci a/b)
#   jakarta_pedestrian_4326 : jaringan pedestrian OSM (kolom fclass)   (skrip 06)
#   jakarta_kota            : batas kota, EPSG:32748 (NAMOBJ)          (skrip 05)
#
# Output (folder app/data/):
#   app_data.rds  -> list(edges, clusters, summary_priority, summary_kota,
#                         boundary, meta)
#   master_full.gpkg -> arsip lengkap (termasuk jarak ke fasilitas) untuk riset
# ==============================================================================

source("00_setup.R")   # abaikan jika library sudah dimuat

# ------------------------------------------------------------------------------
# 0. PARAMETER (satu tempat, mudah diubah & dicatat di meta)
# ------------------------------------------------------------------------------
TOL_M         <- 15                                    # toleransi ruas ke fasilitas (m)
DEDICATED     <- c("footway", "path", "pedestrian", "steps")
SHARED_STREET <- c("living_street", "residential")

TOP_SCEN      <- c(t10 = 0.90, t20 = 0.80, t30 = 0.70) # kuantil flow_total
CL_SCEN       <- "t20"                                 # cluster hanya untuk top 20%

# Parameter DBSCAN per prioritas (P2 & P3 BELUM diuji: cek dengan kNNdistplot
# dan tabel sensitivitas sebelum dipakai final)
CL_PARAMS <- data.frame(
  priority = c("P1", "P2", "P3"),
  eps      = c(300, 200, 200),   # m
  minpts   = c(10, 10, 10)
)
RADIUS_Q      <- 1
MIN_RADIUS_M  <- 100
WEIGHT_CENTER <- TRUE

OUT_DIR <- "app/data"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. DASAR: ruas ber-demand dari edges_all
# ------------------------------------------------------------------------------
need <- c("edge_id", "a", "b", "flow_walkonly", "flow_acc", "flow_egr",
          "flow_pt", "flow_total")
stopifnot(all(need %in% names(edges_all)), !anyDuplicated(edges_all$edge_id))

master <- edges_all %>%
  filter(!is.na(flow_total), flow_total > 0) %>%
  select(edge_id, a, b, flow_walkonly, flow_acc, flow_egr, flow_pt, flow_total,
         any_of("share_pt"))
n0 <- nrow(master)

# ------------------------------------------------------------------------------
# 2. PANJANG RUAS (dari atribut graph, kunci a-b tak berarah) + PED-KM
# ------------------------------------------------------------------------------
g_len <- as.data.table(as.data.frame(graph_with_od))[
  , .(a = pmin(from_id, to_id), b = pmax(from_id, to_id), d)][
    , .(len_m = mean(d)), by = .(a, b)]

master <- master %>% left_join(as.data.frame(g_len), by = c("a", "b"))
stopifnot(nrow(master) == n0, !anyNA(master$len_m))
master$ped_km <- master$flow_total * master$len_m / 1000

# ------------------------------------------------------------------------------
# 3. TITIK TENGAH (UTM) -> KOTA
# ------------------------------------------------------------------------------
m_utm <- st_transform(master, 32748)
mid   <- st_centroid(st_geometry(m_utm))
mid_xy <- st_coordinates(mid)

kota_utm <- jakarta_kota %>%
  transmute(kota = case_when(
    NAMOBJ == "Kota Adm. Jakarta Pusat"   ~ "Central Jakarta",
    NAMOBJ == "Kota Adm. Jakarta Utara"   ~ "North Jakarta",
    NAMOBJ == "Kota Adm. Jakarta Barat"   ~ "West Jakarta",
    NAMOBJ == "Kota Adm. Jakarta Selatan" ~ "South Jakarta",
    NAMOBJ == "Kota Adm. Jakarta Timur"   ~ "East Jakarta",
    TRUE ~ NAMOBJ))

k_idx <- vapply(st_intersects(mid, kota_utm),
                function(i) if (length(i)) i[1] else NA_integer_, integer(1))
master$kota <- kota_utm$kota[k_idx]
master$kota[is.na(master$kota)] <- "Outside boundary"   # titik tengah di luar batas

# ------------------------------------------------------------------------------
# 4. SUPPLY: jarak titik tengah ke fasilitas dedicated / shared
# ------------------------------------------------------------------------------
ped_utm <- st_transform(jakarta_pedestrian_4326, 32748)
ped_ded <- ped_utm %>% filter(fclass %in% DEDICATED)
ped_shr <- ped_utm %>% filter(fclass %in% SHARED_STREET)

near_dist <- function(pts, lines) {
  g <- st_geometry(lines); stopifnot(length(g) > 0)
  as.numeric(st_distance(pts, g[st_nearest_feature(pts, g)], by_element = TRUE))
}
master$d_ded <- near_dist(mid, ped_ded)
master$d_shr <- near_dist(mid, ped_shr)

master$supply <- case_when(
  master$d_ded <= TOL_M ~ "dedicated",
  master$d_shr <= TOL_M ~ "shared",
  TRUE                  ~ "none_recorded"      # tidak tercatat di data OSM
)

# ------------------------------------------------------------------------------
# 5. PRIORITAS UNTUK TIAP AMBANG (top 10 / 20 / 30%), dari flow_total
#    Dasar kuantil: ruas dengan flow_total > 0 (sama seperti skrip 06)
# ------------------------------------------------------------------------------
thr_tbl <- list()
for (s in names(TOP_SCEN)) {
  thr <- unname(quantile(master$flow_total, TOP_SCEN[[s]], na.rm = TRUE))
  thr_tbl[[s]] <- data.frame(scenario = s, quantile = TOP_SCEN[[s]], flow_threshold = thr)
  high <- master$flow_total >= thr
  master[[paste0("priority_", s)]] <- factor(
    case_when(high & master$supply == "none_recorded" ~ "P1",
              high & master$supply == "shared"        ~ "P2",
              high & master$supply == "dedicated"     ~ "P3",
              TRUE                                    ~ "other"),
    levels = c("P1", "P2", "P3", "other"))
}
thr_tbl <- bind_rows(thr_tbl)

# Pemeriksaan: himpunan bersarang (top10 ⊂ top20 ⊂ top30)
stopifnot(all(master$priority_t10 == "other" | master$priority_t20 != "other"),
          all(master$priority_t20 == "other" | master$priority_t30 != "other"))

# ------------------------------------------------------------------------------
# 6. CLUSTER (DBSCAN) per prioritas pada ambang CL_SCEN
# ------------------------------------------------------------------------------
cluster_one <- function(df, xy, p, eps, minpts) {
  cl <- dbscan(xy, eps = eps, minPts = minpts)$cluster
  if (!any(cl > 0)) return(list(id = rep("noise", nrow(xy)), circ = NULL))
  
  d <- data.frame(cl = cl, x = xy[, 1], y = xy[, 2],
                  len_m = df$len_m, ped_km = df$ped_km)
  d$w <- if (WEIGHT_CENTER) d$ped_km else 1
  mem <- d[d$cl > 0, ]
  
  circ <- mem %>% group_by(cl) %>%
    summarise(n_ruas = n(), km = sum(len_m) / 1000, ped_km = sum(ped_km),
              cx = weighted.mean(x, w), cy = weighted.mean(y, w),
              med_len = median(len_m), .groups = "drop")
  rad <- mem %>% left_join(circ %>% select(cl, cx, cy), by = "cl") %>%
    mutate(dist_c = sqrt((x - cx)^2 + (y - cy)^2)) %>%
    group_by(cl) %>% summarise(r_members = quantile(dist_c, RADIUS_Q), .groups = "drop")
  
  circ <- circ %>% left_join(rad, by = "cl") %>%
    mutate(radius_m = pmax(r_members + 0.5 * med_len, MIN_RADIUS_M),
           ped_km_per_km = round(ped_km / km)) %>%
    arrange(desc(ped_km)) %>%
    mutate(rank = row_number(), priority = p,
           cluster_id = sprintf("%s_%02d", p, rank))
  
  map <- setNames(circ$cluster_id, circ$cl)
  list(id = ifelse(cl > 0, unname(map[as.character(cl)]), "noise"), circ = circ)
}

pcol   <- paste0("priority_", CL_SCEN)
ccol   <- paste0("cluster_", CL_SCEN)
master[[ccol]] <- NA_character_
circ_list <- list()

for (k in seq_len(nrow(CL_PARAMS))) {
  p <- CL_PARAMS$priority[k]
  i <- which(master[[pcol]] == p)
  if (length(i) < CL_PARAMS$minpts[k]) next
  res <- cluster_one(st_drop_geometry(master)[i, ], mid_xy[i, , drop = FALSE],
                     p, CL_PARAMS$eps[k], CL_PARAMS$minpts[k])
  master[[ccol]][i] <- res$id
  circ_list[[p]] <- res$circ
}

circ_all <- bind_rows(circ_list)
pts <- st_as_sf(circ_all, coords = c("cx", "cy"), crs = 32748, remove = FALSE)
clusters <- pts
st_geometry(clusters) <- st_buffer(st_geometry(pts), dist = pts$radius_m, nQuadSegs = 30)
clusters <- clusters %>%
  select(cluster_id, priority, rank, n_ruas, km, ped_km, ped_km_per_km, radius_m) %>%
  st_transform(4326)

# Atribut cluster ikut ke tabel ruas (agar popup ruas bisa menampilkan rank cluster)
master <- master %>%
  left_join(circ_all %>% select(cluster_id, cluster_rank = rank),
            by = setNames("cluster_id", ccol))

# ------------------------------------------------------------------------------
# 7. TABEL RINGKASAN (tanpa st_intersection: memakai kolom kota pada tiap ruas)
# ------------------------------------------------------------------------------
tab <- st_drop_geometry(master)

summary_kota <- tab %>%
  group_by(kota) %>%
  summarise(n_edges = n(), km = sum(len_m) / 1000,
            flow_walkonly = sum(flow_walkonly), flow_pt = sum(flow_pt),
            flow_total = sum(flow_total), ped_km = sum(ped_km), .groups = "drop")

summary_priority <- bind_rows(lapply(names(TOP_SCEN), function(s) {
  tab %>% mutate(priority = .data[[paste0("priority_", s)]]) %>%
    group_by(kota, priority) %>%
    summarise(n_edges = n(), km = sum(len_m) / 1000,
              ped_km = sum(ped_km), flow_total = sum(flow_total), .groups = "drop") %>%
    mutate(scenario = s, .before = 1)
}))

# ------------------------------------------------------------------------------
# 8. VALIDASI
# ------------------------------------------------------------------------------
stopifnot(nrow(master) == n0,
          !anyDuplicated(master$edge_id),
          !anyNA(master$supply),
          abs(sum(master$flow_total) - sum(edges_all$flow_total[edges_all$flow_total > 0])) < 1e-6)
cat("Ruas master:", nrow(master), "| klaster:", nrow(clusters), "\n")
print(table(master$priority_t10)); print(table(master$priority_t20)); print(table(master$priority_t30))
print(table(master[[ccol]], useNA = "ifany") %>% head(15))
print(thr_tbl)

# ------------------------------------------------------------------------------
# 9. EKSPOR
# ------------------------------------------------------------------------------
master_full <- st_transform(master, 4326)           # arsip lengkap

edges_app <- master_full %>%
  select(edge_id, kota,
         flow_walkonly, flow_acc, flow_egr, flow_pt, flow_total,
         len_m, ped_km, supply,
         starts_with("priority_"), starts_with("cluster_")) %>%
  mutate(across(c(flow_walkonly, flow_acc, flow_egr, flow_pt, flow_total), ~ round(.x, 1)),
         len_m = round(len_m, 1), ped_km = round(ped_km, 3))

meta <- list(created = Sys.time(), tol_m = TOL_M, thresholds = thr_tbl,
             cluster_scenario = CL_SCEN, cluster_params = CL_PARAMS,
             dedicated = DEDICATED, shared = SHARED_STREET)

saveRDS(list(edges = edges_app, clusters = clusters,
             summary_priority = summary_priority, summary_kota = summary_kota,
             boundary = st_transform(jakarta_kota, 4326), meta = meta),
        file.path(OUT_DIR, "app_data.rds"))

st_write(master_full, file.path(OUT_DIR, "master_full.gpkg"), delete_dsn = TRUE)
cat("Selesai. Ukuran app_data.rds:",
    round(file.size(file.path(OUT_DIR, "app_data.rds")) / 1e6, 1), "MB\n")
