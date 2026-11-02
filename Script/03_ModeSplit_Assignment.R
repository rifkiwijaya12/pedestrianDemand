#mode share
#OD trip with modal split = pedestrian (distance <=1km)
#OD distance <= 1 km

od_distance_pedestrian <- od_distances_df %>%
  filter(distance <=2) %>%
  filter(distance >0)

od_trip_pedestrian_only <- od_distance_pedestrian %>%
  left_join(
    od_trip_attributes2 %>%
      select(
        kota_origin,
        kecamatan_origin,
        kelurahan_origin,
        RW_origin,
        centroid_x_origin,
        centroid_y_origin,
        origin,
        destination,
        kota_destination,
        kecamatan_destination,
        kelurahan_destination,
        RW_destination,
        centroid_x_destination,
        centroid_y_destination,
        trips
      ),
    by = c(
      "origin_zone" = "origin",
      "destination_zone" = "destination"
    )
  )

#distance decay function
decay_logi <- function(d_m, d50 = 800, beta = log(9) / 400) {
  1 / (1 + exp(beta * (d_m - d50)))
}

od_routes <- od_trip_pedestrian_only %>%
  filter(!is.na(origin_vertex), !is.na(destination_vertex),
         !is.na(trips), trips > 0) %>%
  mutate(
    p_walk     = decay_logi(distance*1000),   # km -> m
    walk_trips = trips * p_walk
  )

od_routes <- od_routes %>%
  filter(walk_trips > 1)

paths_pedestrian <- dodgr_paths(
  graph    = graph_with_od,
  from     = od_routes$origin_vertex,
  to       = od_routes$destination_vertex,
  pairwise = TRUE,
  vertices = FALSE
)

edges_per_route <- lapply(paths_pedestrian, function(p) {
  if (is.null(p) || length(p) == 0) return(integer(0))
  e <- p[[1]]
  if (is.null(e)) integer(0) else as.integer(e)
})

lens <- lengths(edges_per_route)

od_routes$routed <- lens > 0

flows <- data.table(
  edge_idx = unlist(edges_per_route, use.names = FALSE),
  flow     = rep(od_routes$walk_trips, lens)
)[, .(flow = sum(flow)), by = edge_idx]

# Tabel edge + flow
graph_flow <- as.data.frame(graph_with_od[flows$edge_idx, ])
graph_flow$flow <- flows$flow

dt_pedestrian <- as.data.table(graph_flow)[, .(from_id, to_id, flow)]
dt_pedestrian[, `:=`(a = pmin(from_id, to_id), b = pmax(from_id, to_id))]
dt_u_pedestrian <- dt_pedestrian[, .(flow = sum(flow)), by = .(a, b)]

vertices <- dodgr_vertices(graph_with_od)

i_a <- match(dt_u_pedestrian$a, vertices$id)
i_b <- match(dt_u_pedestrian$b, vertices$id)

n <- nrow(dt_u_pedestrian)
coords <- data.frame(
  edge_id = rep(seq_len(n), each = 2),
  x = as.vector(rbind(vertices$x[i_a], vertices$x[i_b])),
  y = as.vector(rbind(vertices$y[i_a], vertices$y[i_b]))
)

edges_pedestrian <- sfheaders::sf_linestring(
  obj = coords, x = "x", y = "y", linestring_id = "edge_id"
) |>
  sf::st_set_crs(4326)

edges_pedestrian$flow <- dt_u_pedestrian$flow

tmap_mode("view")
tm_shape(edges_pedestrian %>% filter(flow > 0)) +
  tm_lines(
    col = "flow",
    col.scale  = tm_scale_intervals(style = "quantile", n = 4, values = "viridis"),
    col.legend = tm_legend(title = "Estimated walk trips"),
    lwd = 1,
    lwd.scale  = tm_scale_continuous(values.range = c(0.5, 4)),
    lwd.legend = tm_legend_hide()
  )



# ==============================================================================
# PENDEKATAN 2: MULTIMODAL (walking - public transport - walking)
# Output: volume pejalan kaki (access + egress) per ruas jalan, peta, dan tabel status
#
# Prasyarat objek di environment (dari proses sebelumnya):
#   - graph_with_od        : graph dodgr pejalan kaki (TIDAK contracted, urutan baris tetap)
#   - od_distances_df      : origin_zone, origin_vertex, destination_zone, destination_vertex, distance (km)
#   - od_trip_attributes2  : atribut OD + trips
# ==============================================================================

library(dplyr)
library(data.table)
library(Matrix)
library(dodgr)
library(sf)
library(sfheaders)
library(tmap)

# ------------------------------------------------------------------------------
# 0. PARAMETER
# ------------------------------------------------------------------------------
SAMPLE_MODE <- TRUE         # TRUE: pakai filter sampel Anda; FALSE: seluruh OD > 2 km
PT_SHARE    <- 0.2           # pangsa PT (asumsi)
R_WALK      <- 800           # batas jalan kaki access/egress (meter)
R_FAR       <- 5000          # radius cari halte untuk sisi non-jalan-kaki (asumsi, uji sensitivitas)
USE_DECAY   <- TRUE         # FALSE: walk_trips = pt_trips (sesuai keputusan sementara)
SERVICE_ID  <- "SH"          # NULL = semua service_id
TRANSFER_MODE <- "cluster"   # "strict" = stop_id sama; "cluster" = nama sama & <= 50 m
CLUSTER_M   <- 100

decay_logi <- function(d_m, d50 = 0.800, beta = log(9) / 400) {
  1 / (1 + exp(beta * (d_m - d50)))
}

# ------------------------------------------------------------------------------
# 1. OD MULTIMODAL
# ------------------------------------------------------------------------------
od_distance_PT <- if (SAMPLE_MODE) {
  od_distances_df %>% filter(distance >= 5.065916, distance < 5.065918)
} else {
  od_distances_df %>% filter(distance > 2)      # > 2 km, tidak tumpang tindih dgn pendekatan 1
}

od_trip_multimodal <- od_distance_PT %>%
  left_join(
    od_trip_attributes2 %>%
      select(kota_origin, kecamatan_origin, kelurahan_origin, RW_origin,
             centroid_x_origin, centroid_y_origin, origin, destination,
             kota_destination, kecamatan_destination, kelurahan_destination,
             RW_destination, centroid_x_destination, centroid_y_destination, trips),
    by = c("origin_zone" = "origin", "destination_zone" = "destination")
  ) %>%
  mutate(pt_trips = PT_SHARE * trips) %>%
  mutate(distance=1000*distance)

od_trip_multimodal <- od_trip_multimodal %>%
  filter(pt_trips > 1)

od <- as.data.table(od_trip_multimodal)[
  !is.na(origin_vertex) & !is.na(destination_vertex) & !is.na(pt_trips) & pt_trips > 0,
  .(origin_zone, destination_zone, origin_vertex, destination_vertex, pt_trips)]


# ------------------------------------------------------------------------------
# 2. BACA & BERSIHKAN GTFS
# ------------------------------------------------------------------------------
gtfs_dir <- "D:/Github/pedestrianDemand/Data/file_gtfs"

stops <- fread(file.path(gtfs_dir, "stops.txt"),
               colClasses = list(character = "stop_id"), encoding = "UTF-8")
st    <- fread(file.path(gtfs_dir, "stop_times.txt"),
               select = c("trip_id", "stop_id", "stop_sequence"),
               colClasses = list(character = c("trip_id", "stop_id")))
tr    <- fread(file.path(gtfs_dir, "trips.txt"),
               select = c("trip_id", "route_id", "service_id"),
               colClasses = list(character = c("trip_id", "route_id", "service_id")))

# samakan format ID
stops[, stop_id := trimws(stop_id)]
st[, `:=`(stop_id = trimws(stop_id), trip_id = trimws(trip_id))]
tr[, `:=`(trip_id = trimws(trip_id), service_id = trimws(service_id))]

# stops: koordinat valid & unik
stops <- stops[!is.na(stop_lat) & !is.na(stop_lon)]
stops <- stops[!duplicated(stop_id)]

# stop_times: hanya halte yang ada
n_before <- nrow(st)
st[, stop_sequence := as.integer(stop_sequence)]
st <- st[!is.na(stop_sequence) & stop_id %in% stops$stop_id]

# trips: filter service
if (!is.null(SERVICE_ID)) {
  if (!SERVICE_ID %in% tr$service_id) warning("SERVICE_ID tidak ditemukan; cek unique(tr$service_id)")
  tr <- tr[service_id == SERVICE_ID]
}
st <- st[tr[, .(trip_id)], on = "trip_id", nomatch = 0]
setorder(st, trip_id, stop_sequence)
st <- st[, if (.N >= 2) .SD, by = trip_id]

# pola rute unik (satu trip wakil per urutan halte)
sig <- st[, .(sig = paste(stop_id, collapse = "|")), by = trip_id]
rep_trips <- sig[!duplicated(sig), trip_id]
ps <- st[trip_id %in% rep_trips, .(trip_id, stop_id, seq = stop_sequence)]


# pasangan halte terhubung langsung (A sebelum B pada pola yang sama)
direct <- ps[ps, on = "trip_id", allow.cartesian = TRUE][
  seq < i.seq, .(from = stop_id, to = i.stop_id)] |> unique()


# stops hanya yang dilalui rute; urutan baris = indeks matriks
stops <- stops[stop_id %in% unique(ps$stop_id)]
ns <- nrow(stops)

D0 <- sparseMatrix(i = match(direct$from, stops$stop_id),
                   j = match(direct$to,   stops$stop_id),
                   x = 1, dims = c(ns, ns))
diag(D0) <- 0
D0 <- drop0(D0)

# ------------------------------------------------------------------------------
# 3. SNAP HALTE KE GRAPH, MATRIKS TRANSFER (halte yang sama, tanpa jalan kaki)
# ------------------------------------------------------------------------------
v <- dodgr_vertices(graph_with_od)

stops[, vertex := v$id[match_points_to_verts(v, cbind(stop_lon, stop_lat), connected = TRUE)]]

xy <- st_coordinates(st_transform(
  st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE), 32748))
stops[, `:=`(x = xy[, 1], y = xy[, 2])]

if (TRANSFER_MODE == "strict") {
  Tm <- Diagonal(ns)
} else {
  stops_utm <- st_as_sf(stops, coords = c("x", "y"), crs = 32748, remove = FALSE)
  nbc <- st_is_within_distance(stops_utm, stops_utm, dist = CLUSTER_M)
  same <- lapply(seq_along(nbc), function(i) {
    j <- nbc[[i]]; j[stops$stop_name[j] == stops$stop_name[i]]
  })
  Tm <- sparseMatrix(i = rep(seq_along(same), lengths(same)), j = unlist(same),
                     x = 1, dims = c(ns, ns))
}
D0T <- D0 %*% Tm        # halte naik -> halte tempat pindah rute

# ------------------------------------------------------------------------------
# 4. JARAK JARINGAN ZONA -> HALTE (<= R_FAR), flag walkable (<= R_WALK)
# ------------------------------------------------------------------------------
near_stops <- function(vs, radius, chunk = 100) {
  vv  <- v[match(vs, v$id), ]
  ord <- order(round(vv$x, 2), vv$y)           # kelompokkan secara spasial
  vs  <- vs[ord]; vv <- vv[ord, ]
  grp <- ceiling(seq_along(vs) / chunk)
  buf <- radius / 111000 + 0.005               # derajat (pra-filter bbox)
  
  res <- lapply(split(seq_along(vs), grp), function(ii) {
    xr <- range(vv$x[ii]) + c(-buf, buf)
    yr <- range(vv$y[ii]) + c(-buf, buf)
    sel <- which(stops$stop_lon >= xr[1] & stops$stop_lon <= xr[2] &
                   stops$stop_lat >= yr[1] & stops$stop_lat <= yr[2])
    if (!length(sel)) return(NULL)
    sv <- unique(stops$vertex[sel])
    dm <- dodgr_dists(graph_with_od, from = vs[ii], to = sv)   # meter
    idx <- which(!is.na(dm) & dm <= radius, arr.ind = TRUE)
    if (!nrow(idx)) return(NULL)
    data.table(vertex = vs[ii][idx[, 1]], stop_vertex = sv[idx[, 2]], d = dm[idx])
  })
  out <- rbindlist(res)
  out <- merge(out, stops[, .(stop_i = .I, stop_vertex = vertex)],
               by = "stop_vertex", allow.cartesian = TRUE)   # 1 vertex bisa >1 stop_id
  out[, walk := d <= R_WALK]
  setkey(out, vertex)
  out
}

acc_dt <- near_stops(unique(od$origin_vertex),      R_FAR)
egr_dt <- near_stops(unique(od$destination_vertex), R_FAR)   # graph dianggap simetris

# ------------------------------------------------------------------------------
# 5. ROUTING PT: pilih halte naik/turun (maks. 1 transfer di halte yang sama)
# ------------------------------------------------------------------------------
# side = "both": minimalkan egress, lalu ntr, lalu access
# side = "acc" : minimalkan access (egress via moda lain), lalu ntr
# side = "egr" : minimalkan egress (access via moda lain), lalu ntr
try_pair <- function(A, E, side) {
  if (nrow(A) == 0 || nrow(E) == 0) return(NULL)
  C0 <- as.matrix(D0[A$stop_i, E$stop_i, drop = FALSE]) > 0
  C1 <- as.matrix(D0T[A$stop_i, , drop = FALSE] %*% D0[, E$stop_i, drop = FALSE]) > 0
  ok <- which(C0 | C1, arr.ind = TRUE)
  if (nrow(ok) == 0) return(NULL)
  p <- data.table(a = ok[, 1], e = ok[, 2])
  p <- p[A$stop_i[a] != E$stop_i[e]]                 # naik dan turun bukan halte yang sama
  if (nrow(p) == 0) return(NULL)
  p[, `:=`(ntr = fifelse(C0[cbind(a, e)], 0L, 1L),
           d_a = A$d[a], d_e = E$d[e])]
  setorderv(p, switch(side,
                      both = c("d_e", "ntr", "d_a"),
                      acc  = c("d_a", "ntr"),
                      egr  = c("d_e", "ntr")))
  b <- p[1]
  list(board = A$stop_i[b$a], alight = E$stop_i[b$e], ntr = b$ntr,
       d_a = b$d_a, d_e = b$d_e)
}

solve_od <- function(ov, dv) {
  A_all <- acc_dt[.(ov), nomatch = 0]
  E_all <- egr_dt[.(dv), nomatch = 0]
  if (nrow(A_all) == 0 || nrow(E_all) == 0) return(list(status = "no_stop"))
  A_w <- A_all[walk == TRUE]
  E_w <- E_all[walk == TRUE]
  
  r <- try_pair(A_w, E_w, "both")
  if (!is.null(r)) return(c(list(status = "both_walk"), r))
  
  ra <- try_pair(A_w, E_all, "acc")   # access jalan kaki, egress moda lain
  re <- try_pair(A_all, E_w, "egr")   # egress jalan kaki, access moda lain
  if (!is.null(ra) && !is.null(re)) {
    if (ra$d_a <= re$d_e) re <- NULL else ra <- NULL
  }
  if (!is.null(ra)) return(c(list(status = "access_only"), ra))
  if (!is.null(re)) return(c(list(status = "egress_only"), re))
  list(status = "not_connected")
}

# routing per pasangan vertex unik (banyak OD berbagi vertex yang sama)
uv <- unique(od[, .(origin_vertex, destination_vertex)])


t_route <- system.time({
  res_v <- rbindlist(lapply(seq_len(nrow(uv)), function(i) {
    if (i %% 1000 == 0) cat("  ", i, "/", nrow(uv), "\n")
    c(list(origin_vertex = uv$origin_vertex[i],
           destination_vertex = uv$destination_vertex[i]),
      solve_od(uv$origin_vertex[i], uv$destination_vertex[i]))
  }), fill = TRUE)
})
print(t_route)

res <- merge(od, res_v, by = c("origin_vertex", "destination_vertex"))

# tabel status
status_tbl <- res[, .(n_od = .N, trips = sum(pt_trips)), by = status][
  , `:=`(pct_od = round(100 * n_od / sum(n_od), 1),
         pct_trips = round(100 * trips / sum(trips), 1))][order(-n_od)]


# ------------------------------------------------------------------------------
# 6. LEG JALAN KAKI (access & egress) -> walk_trips
#    sementara walk_trips = pt_trips (USE_DECAY = FALSE)
# ------------------------------------------------------------------------------
w <- function(d) if (USE_DECAY) decay_logi(d) else 1

acc_ok <- res[status %in% c("both_walk", "access_only")]
egr_ok <- res[status %in% c("both_walk", "egress_only")]

legs <- rbindlist(list(
  acc_ok[, .(from = origin_vertex, to = stops$vertex[board],
             type = "access", flow = pt_trips * w(d_a))],
  egr_ok[, .(from = stops$vertex[alight], to = destination_vertex,
             type = "egress", flow = pt_trips * w(d_e))]
))
legs <- legs[from != to]

pairs <- legs[, .(flow_acc = sum(flow[type == "access"]),
                  flow_egr = sum(flow[type == "egress"])), by = .(from, to)]


# ------------------------------------------------------------------------------
# 7. ASSIGN LEG KE GRAPH PEJALAN KAKI (indeks edge, tanpa membuat geometri per rute)
# ------------------------------------------------------------------------------
paths_multimodal <- dodgr_paths(graph = graph_with_od,
                       from = pairs$from, to = pairs$to,
                       pairwise = TRUE, vertices = FALSE)

edges_per_leg <- lapply(paths_multimodal, function(p) {
  if (is.null(p) || length(p) == 0) return(integer(0))
  e <- p[[1]]
  if (is.null(e)) integer(0) else as.integer(e)
})
lens <- lengths(edges_per_leg)
cat("Leg tanpa path:", sum(lens == 0), "dari", length(lens), "\n")

flows <- data.table(
  edge_idx = unlist(edges_per_leg, use.names = FALSE),
  flow_acc = rep(pairs$flow_acc, lens),
  flow_egr = rep(pairs$flow_egr, lens)
)[, .(flow_acc = sum(flow_acc), flow_egr = sum(flow_egr)), by = edge_idx]
flows[, flow_total := flow_acc + flow_egr]

# ------------------------------------------------------------------------------
# 8. GABUNGKAN DUA ARAH DI TABEL BIASA, GEOMETRI DIBUAT SEKALI
# ------------------------------------------------------------------------------
g_df <- as.data.frame(graph_with_od)
dt_multimodal <- as.data.table(g_df[flows$edge_idx, c("from_id", "to_id")])
dt_multimodal <- cbind(dt_multimodal, flows[, .(flow_acc, flow_egr, flow_total)])
dt_multimodal[, `:=`(a = pmin(from_id, to_id), b = pmax(from_id, to_id))]
dt_u_multimodal <- dt_multimodal[, .(flow_acc = sum(flow_acc), flow_egr = sum(flow_egr),
               flow_total = sum(flow_total)), by = .(a, b)]

ia <- match(dt_u_multimodal$a, v$id)
ib <- match(dt_u_multimodal$b, v$id)
n  <- nrow(dt_u_multimodal)
coords <- data.frame(
  edge_id = rep(seq_len(n), each = 2),
  x = as.vector(rbind(v$x[ia], v$x[ib])),
  y = as.vector(rbind(v$y[ia], v$y[ib]))
)

edges_ped_pt <- sfheaders::sf_linestring(obj = coords, x = "x", y = "y",
                                         linestring_id = "edge_id") |>
  sf::st_set_crs(4326)
edges_ped_pt$flow_acc   <- dt_u_multimodal$flow_acc
edges_ped_pt$flow_egr   <- dt_u_multimodal$flow_egr
edges_ped_pt$flow_total <- dt_u_multimodal$flow_total
edges_ped_pt$vertex_a   <- dt_u_multimodal$a
edges_ped_pt$vertex_b   <- dt_u_multimodal$b

# ------------------------------------------------------------------------------
# 9. VISUALISASI: flow pejalan kaki per ruas
# ------------------------------------------------------------------------------
tmap_mode("view")

tm_shape(edges_ped_pt %>% filter(flow_total > 0)) +
  tm_lines(
    col = "flow_total",
    col.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      values = "viridis"
    ),
    col.legend = tm_legend(
      title = "Walk trips (access + egress)"
    ),
    lwd = 2,
    lwd.scale = tm_scale_continuous(
      values.range = c(0.5, 6)
    ),
    lwd.legend = tm_legend_hide()
  )


#PENGGABUNGAN PENDEKATAN 1 DAN 2

# ==============================================================================
# GABUNGKAN FLOW PENDEKATAN 1 (walk-only) DAN PENDEKATAN 2 (access/egress PT)
# Kunci ruas: (a, b) = pmin/pmax(from_id, to_id) pada graph_with_od yang SAMA
# ==============================================================================


# ------------------------------------------------------------------------------
# A. SIMPAN HASIL TIAP PENDEKATAN (jalankan tepat setelah dt_u terbentuk)
#    Objek flows, dt, dt_u, n, coords, lens ditimpa oleh pendekatan 2,
#    jadi simpan hasil pendekatan 1 lebih dulu.

vol1 <- copy(dt_u_pedestrian)[, .(a, b, flow_walkonly = flow)]
vol2 <- copy(dt_u_multimodal)[, .(a, b, flow_acc, flow_egr)]

# ------------------------------------------------------------------------------
# B. CEK KONSISTENSI GRAPH / VERTEX
# ------------------------------------------------------------------------------
stopifnot(is.character(vol1$a), is.character(vol1$b),
          is.character(vol2$a), is.character(vol2$b))

g <- as.data.table(as.data.frame(graph_with_od))[, .(from_id, to_id)]
g_keys <- unique(g[, .(a = pmin(from_id, to_id), b = pmax(from_id, to_id))])

n_miss <- function(x) nrow(x[!g_keys, on = c("a", "b")])
cat("Ruas pendekatan 1 yang tidak ada di graph :", n_miss(vol1), "\n")
cat("Ruas pendekatan 2 yang tidak ada di graph :", n_miss(vol2), "\n")
# keduanya harus 0. Jika > 0, graph berbeda antar sesi/proses.

# ------------------------------------------------------------------------------
# C. GABUNGKAN PER RUAS
# ------------------------------------------------------------------------------
vol_all <- merge(vol1, vol2, by = c("a", "b"), all = TRUE)
for (j in c("flow_walkonly", "flow_acc", "flow_egr"))
  set(vol_all, which(is.na(vol_all[[j]])), j, 0)

vol_all[, `:=`(flow_pt    = flow_acc + flow_egr,
               flow_total = flow_walkonly + flow_acc + flow_egr)]
vol_all[, share_pt := fifelse(flow_total > 0, flow_pt / flow_total, NA_real_)]

cat("Jumlah ruas: walk-only =", nrow(vol1), "| multimodal =", nrow(vol2),
    "| gabungan =", nrow(vol_all), "| beririsan =",
    nrow(vol_all[flow_walkonly > 0 & flow_pt > 0]), "\n")

# ------------------------------------------------------------------------------
# D. GEOMETRI SEKALI (dari vertex graph)
# ------------------------------------------------------------------------------
v  <- dodgr_vertices(graph_with_od)
ia <- match(vol_all$a, v$id)
ib <- match(vol_all$b, v$id)
n  <- nrow(vol_all)

coords <- data.frame(
  edge_id = rep(seq_len(n), each = 2),
  x = as.vector(rbind(v$x[ia], v$x[ib])),
  y = as.vector(rbind(v$y[ia], v$y[ib]))
)

edges_all <- sfheaders::sf_linestring(obj = coords, x = "x", y = "y",
                                      linestring_id = "edge_id") |>
  sf::st_set_crs(4326)
edges_all <- cbind(edges_all, as.data.frame(vol_all))   # urutan baris sama



# ------------------------------------------------------------------------------
# E. PETA: ruas yang membawa 80% demand, lebar & warna menurut flow_total
# ------------------------------------------------------------------------------
e <- edges_all %>%
  filter(!is.na(flow_total), flow_total > 0) %>%
  arrange(desc(flow_total)) %>%
  mutate(cum = cumsum(flow_total) / sum(flow_total, na.rm = TRUE))

tmap_mode("view")
tm_shape(edges_all) +
  tm_lines(
    col = "flow_total",
    col.scale  = tm_scale_intervals(style = "quantile", n = 6, values = "viridis"),
    col.legend = tm_legend(title = "Estimated walk trips"),
    lwd = 1,
    lwd.scale  = tm_scale_continuous(values.range = c(0.5, 6)),
    lwd.legend = tm_legend_hide()
  )

tm_shape(edges_all) +
  tm_lines(
    col = "flow_total",
    col.scale = tm_scale_intervals(
      style = "quantile",
      n = 5,
      values = "viridis"
    ),
    col.legend = tm_legend(
      title = "Walk trips (access + egress)"
    ),
    lwd = 2,
    lwd.scale = tm_scale_continuous(
      values.range = c(0.5, 6)
    ),
    lwd.legend = tm_legend_hide()
  )



#optional: public transport route flow
# ==============================================================================
# EKSTRAKSI PERJALANAN PUBLIC TRANSPORT (MULTIMODAL: walk - PT - walk)
#
# Jalankan SETELAH blok pendekatan 2 pada 03_ModeSplit_Assignment.R sampai objek `res` terbentuk.
# Objek yang dibutuhkan di environment:
#   res, stops, st, Tm, od_trip_multimodal, v, graph_with_od, gtfs_dir, w()
#   (opsional) sig  : tabel tanda pola rute dari blok GTFS (trip_id, sig); dibuat ulang jika tidak ada
#
# Keluaran:
#   od_mm       : semua OD multimodal + titik centroid, vertex hasil snap, jarak snap, status
#   od_it       : OD terlayani + halte access/egress, rute 1, halte & rute transit, rute 2, flow
#   route_flow  : flow per rute (penumpang naik per rute)
#   stop_sum    : naik/turun/transfer per halte + flow jalan kaki access/egress
#   pt_seg_sf   : beban per segmen antar-halte per rute (garis lurus antar-halte)
#   plot_itinerary(origin_zone, destination_zone) : peta itinerary satu OD
#
# Definisi flow:
#   pt_flow          = pt_trips (penumpang PT pada OD tsb; tidak dikenai decay)
#   walk_flow_access = pt_trips * w(d_access)  (hanya jika sisi access berjarak <= R_WALK)
#   walk_flow_egress = pt_trips * w(d_egress)
# ==============================================================================

SERVED <- c("both_walk", "access_only", "egress_only")
stopifnot(exists("res"), exists("stops"), exists("Tm"), exists("st"), exists("v"))

# ------------------------------------------------------------------------------
# 1. OD MULTIMODAL + TITIK HASIL SNAP KE VERTEX
# ------------------------------------------------------------------------------
od_unique <- unique(
  as.data.table(od_trip_multimodal)[, .(
    origin_zone, destination_zone,
    kota_origin, kecamatan_origin, kelurahan_origin,
    kota_destination, kecamatan_destination, kelurahan_destination,
    centroid_x_origin, centroid_y_origin,            # UTM 32748
    centroid_x_destination, centroid_y_destination,
    distance_m = distance, trips)],
  by = c("origin_zone", "destination_zone"))

od_mm <- merge(res, od_unique, by = c("origin_zone", "destination_zone"),
               all.x = TRUE, sort = FALSE)

vx <- function(ids, what) v[[what]][match(ids, v$id)]
od_mm[, `:=`(origin_vertex_lon      = vx(origin_vertex, "x"),
             origin_vertex_lat      = vx(origin_vertex, "y"),
             destination_vertex_lon = vx(destination_vertex, "x"),
             destination_vertex_lat = vx(destination_vertex, "y"))]

snap_dist <- function(lon, lat, cx, cy) {            # jarak centroid -> vertex hasil snap (m)
  p <- st_coordinates(st_transform(
    st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326), 32748))
  sqrt((cx - p[, 1])^2 + (cy - p[, 2])^2)
}
od_mm[, `:=`(
  snap_origin_m      = snap_dist(origin_vertex_lon, origin_vertex_lat,
                                 centroid_x_origin, centroid_y_origin),
  snap_destination_m = snap_dist(destination_vertex_lon, destination_vertex_lat,
                                 centroid_x_destination, centroid_y_destination))]
cat("Jarak snap origin (m):\n");      print(summary(od_mm$snap_origin_m))
cat("Jarak snap destination (m):\n"); print(summary(od_mm$snap_destination_m))

# ------------------------------------------------------------------------------
# 2. HALTE ACCESS / EGRESS PER OD + FLOW JALAN KAKI
# ------------------------------------------------------------------------------
od_mm[, `:=`(
  board_stop_id     = stops$stop_id[board],   board_stop_name  = stops$stop_name[board],
  board_stop_lat    = stops$stop_lat[board],  board_stop_lon   = stops$stop_lon[board],
  board_stop_vertex = stops$vertex[board],
  alight_stop_id     = stops$stop_id[alight],  alight_stop_name = stops$stop_name[alight],
  alight_stop_lat    = stops$stop_lat[alight], alight_stop_lon  = stops$stop_lon[alight],
  alight_stop_vertex = stops$vertex[alight])]

od_mm[, `:=`(
  walk_access = status %in% c("both_walk", "access_only"),
  walk_egress = status %in% c("both_walk", "egress_only"))]
od_mm[, `:=`(
  walk_flow_access = fifelse(walk_access, pt_trips * w(d_a), 0),
  walk_flow_egress = fifelse(walk_egress, pt_trips * w(d_e), 0),
  pt_flow          = fifelse(status %in% SERVED, pt_trips, 0))]

# ------------------------------------------------------------------------------
# 3. POLA RUTE (per route_id) UNTUK MENGENALI RUTE YANG DIPAKAI
#    Pola dibedakan menurut route_id + urutan halte, sehingga dua rute yang
#    melalui urutan halte identik tetap terbaca sebagai rute berbeda.
#    Keterhubungan (D0) tidak berubah karena urutan halte yang sama.
# ------------------------------------------------------------------------------
tr_full <- fread(file.path(gtfs_dir, "trips.txt"),
                 select = c("trip_id", "route_id", "direction_id", "trip_headsign"),
                 colClasses = list(character = c("trip_id", "route_id", "trip_headsign")))
# jika terjadi galat parsing, hapus "trip_headsign" dari select
tr_full[, `:=`(trip_id = trimws(trip_id), route_id = trimws(route_id))]

routes <- fread(file.path(gtfs_dir, "routes.txt"),
                select = c("route_id", "route_short_name", "route_long_name", "route_desc"),
                colClasses = list(character = c("route_id", "route_short_name",
                                                "route_long_name", "route_desc")))
routes[, route_id := trimws(route_id)]

if (!exists("sig")) sig <- st[, .(sig = paste(stop_id, collapse = "|")), by = trip_id]
sig2 <- merge(sig, tr_full[, .(trip_id, route_id)], by = "trip_id")
rep2 <- sig2[!duplicated(paste(route_id, sig)), trip_id]

pat <- st[trip_id %in% rep2, .(pid = trip_id, stop_id, seq = stop_sequence)]
pat[, stop_i := match(stop_id, stops$stop_id)]
pat <- pat[!is.na(stop_i)]
setorder(pat, pid, seq)
pat[, ord := seq_len(.N), by = pid]
cat("Pola rute (route x urutan halte):", uniqueN(pat$pid), "\n")

pat_info <- merge(tr_full[trip_id %in% rep2, .(pid = trip_id, route_id, direction_id, trip_headsign)],
                  routes, by = "route_id", all.x = TRUE)
setkey(pat_info, pid)

pat_stop <- copy(pat); setkey(pat_stop, stop_i)         # cari pola yang melewati suatu halte
pat_pid  <- copy(pat); setkey(pat_pid, pid)             # ambil seluruh halte suatu pola
patk     <- copy(pat); setkey(patk, pid, ord)           # ambil halte pada urutan tertentu

# ------------------------------------------------------------------------------
# 4. ITINERARY PER PASANGAN (halte naik, halte turun) YANG TERPILIH
#    Aturan pemilihan rute (asumsi, bukan jadwal): jumlah henti terkecil.
#    Langsung (ntr = 0): pola dengan henti paling sedikit dari naik ke turun.
#    1 transfer (ntr = 1): pasangan (halte transfer turun x, halte transfer naik y)
#    pada klaster halte yang sama (Tm), dengan total henti kedua leg terkecil.
# ------------------------------------------------------------------------------
get_itinerary <- function(sa, se, ntr) {
  if (is.na(sa) || is.na(se)) return(NULL)
  pa <- pat_stop[.(sa), .(pid, ord_a = ord)]
  pe <- pat_stop[.(se), .(pid, ord_e = ord)]
  
  if (ntr == 0) {
    dr <- merge(pa, pe, by = "pid")[ord_a < ord_e]
    if (!nrow(dr)) return(NULL)
    dr[, hops := ord_e - ord_a]
    setorder(dr, hops)
    b   <- dr[1]
    alt <- paste(sort(unique(pat_info[.(unique(dr$pid)), route_short_name])), collapse = "|")
    return(list(board = sa, alight = se, ntr = 0L,
                pid1 = b$pid, from1 = b$ord_a, to1 = b$ord_e,
                xfer_alight = NA_integer_, xfer_board = NA_integer_,
                pid2 = NA_character_, from2 = NA_integer_, to2 = NA_integer_,
                hops = b$hops, n_cand = nrow(dr), routes_alt = alt))
  }
  
  s1 <- merge(pat_pid[.(unique(pa$pid)), .(pid, x = stop_i, ord)], pa,
              by = "pid", allow.cartesian = TRUE)[
                ord > ord_a, .(pid1 = pid, ord_a, ord_x = ord, x, h1 = ord - ord_a)]
  s2 <- merge(pat_pid[.(unique(pe$pid)), .(pid, y = stop_i, ord_y = ord)], pe,
              by = "pid", allow.cartesian = TRUE)[
                ord_y < ord_e, .(pid2 = pid, ord_y, y, ord_e, h2 = ord_e - ord_y)]
  if (!nrow(s1) || !nrow(s2)) return(NULL)
  
  ux <- unique(s1$x); uy <- unique(s2$y)
  Mt <- as.matrix(Tm[ux, uy, drop = FALSE]) > 0          # halte transfer: klaster halte yang sama
  k  <- which(Mt, arr.ind = TRUE)
  if (!nrow(k)) return(NULL)
  xy   <- data.table(x = ux[k[, 1]], y = uy[k[, 2]])
  cand <- merge(merge(s1, xy, by = "x", allow.cartesian = TRUE),
                s2, by = "y", allow.cartesian = TRUE)
  cand[, tot := h1 + h2]
  b <- cand[which.min(tot)]
  
  list(board = sa, alight = se, ntr = 1L,
       pid1 = b$pid1, from1 = b$ord_a, to1 = b$ord_x,
       xfer_alight = b$x, xfer_board = b$y,
       pid2 = b$pid2, from2 = b$ord_y, to2 = b$ord_e,
       hops = b$tot, n_cand = nrow(cand), routes_alt = NA_character_)
}

ub <- unique(res[status %in% SERVED, .(board, alight, ntr)])      # itinerary hanya bergantung pada pasangan halte
cat("Pasangan halte unik untuk diekstraksi:", nrow(ub), "\n")

itin <- rbindlist(lapply(seq_len(nrow(ub)), function(i) {
  if (i %% 2000 == 0) cat("  ", i, "/", nrow(ub), "\n")
  get_itinerary(ub$board[i], ub$alight[i], ub$ntr[i])
}), fill = TRUE)

route_cols <- function(dt, pidcol, sfx) {
  m <- match(dt[[pidcol]], pat_info$pid)
  nm <- paste0(c("route_id", "route_short", "route_long", "route_dir", "route_headsign"), sfx)
  dt[, (nm) := list(pat_info$route_id[m], pat_info$route_short_name[m],
                    pat_info$route_long_name[m], pat_info$direction_id[m],
                    pat_info$trip_headsign[m])]
}
route_cols(itin, "pid1", "1")
route_cols(itin, "pid2", "2")

itin[, `:=`(
  xfer_alight_stop_id   = stops$stop_id[xfer_alight],   xfer_alight_stop_name = stops$stop_name[xfer_alight],
  xfer_board_stop_id    = stops$stop_id[xfer_board],    xfer_board_stop_name  = stops$stop_name[xfer_board])]

od_it <- merge(od_mm, itin, by = c("board", "alight", "ntr"), all.x = TRUE, sort = FALSE)
od_pt <- od_it[status %in% SERVED]                                  # khusus OD yang terlayani PT

cat("Itinerary tidak ditemukan untuk", sum(is.na(od_pt$pid1)), "dari", nrow(od_pt), "OD terlayani\n")
print(od_pt[, .(n_od = .N, pt_trips = round(sum(pt_trips), 1)), by = ntr])

# ------------------------------------------------------------------------------
# 5. FLOW PUBLIC TRANSPORT: per rute, per halte, per segmen antar-halte
# ------------------------------------------------------------------------------
legs_pt <- rbindlist(list(
  od_pt[!is.na(pid1),                 .(origin_zone, destination_zone, leg = 1L,
                                        pid = pid1, o_from = from1, o_to = to1, flow = pt_flow)],
  od_pt[ntr == 1 & !is.na(pid2),      .(origin_zone, destination_zone, leg = 2L,
                                        pid = pid2, o_from = from2, o_to = to2, flow = pt_flow)]))
legs_pt[, `:=`(route_id = pat_info$route_id[match(pid, pat_info$pid)], hops = o_to - o_from)]

# 5a. per rute
route_flow <- legs_pt[, .(pax_total            = sum(flow),
                          pax_leg_pertama      = sum(flow[leg == 1]),
                          pax_setelah_transfer = sum(flow[leg == 2]),
                          n_od = .N, pax_hops = sum(flow * hops)), by = route_id]
route_flow <- merge(route_flow, unique(routes, by = "route_id"), by = "route_id", all.x = TRUE)
setorder(route_flow, -pax_total)
print(head(route_flow, 20))

# 5b. per halte (naik, turun, transfer) + flow jalan kaki access/egress
ev <- rbindlist(list(
  od_pt[!is.na(board), .(stop_i = board,  event = "board",           flow = pt_flow)],
  od_pt[!is.na(alight), .(stop_i = alight, event = "alight",          flow = pt_flow)],
  od_pt[ntr == 1, .(stop_i = xfer_board,  event = "transfer_board",   flow = pt_flow)],
  od_pt[ntr == 1, .(stop_i = xfer_alight, event = "transfer_alight",  flow = pt_flow)]))
stop_sum <- dcast(ev[, .(flow = sum(flow)), by = .(stop_i, event)],
                  stop_i ~ event, value.var = "flow", fill = 0)
walk_a <- od_pt[, .(walk_access_flow = sum(walk_flow_access)), by = .(stop_i = board)]
walk_e <- od_pt[, .(walk_egress_flow = sum(walk_flow_egress)), by = .(stop_i = alight)]
stop_sum <- Reduce(function(x, y) merge(x, y, by = "stop_i", all = TRUE),
                   list(stop_sum, walk_a, walk_e))
setnafill(stop_sum, fill = 0, cols = setdiff(names(stop_sum), "stop_i"))
stop_sum[, `:=`(stop_id = stops$stop_id[stop_i], stop_name = stops$stop_name[stop_i],
                stop_lat = stops$stop_lat[stop_i], stop_lon = stops$stop_lon[stop_i])]
stop_sum_sf <- st_as_sf(stop_sum, coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)

# 5c. per segmen antar-halte pada tiap rute (beban penumpang)
hp  <- legs_pt$o_to - legs_pt$o_from
idx <- rep(seq_len(nrow(legs_pt)), hp)
seg_pt <- data.table(pid  = legs_pt$pid[idx],
                     flow = legs_pt$flow[idx],
                     ord  = legs_pt$o_from[idx] + sequence(hp) - 1L)
seg_pt[, `:=`(from_i = patk[.(seg_pt$pid, seg_pt$ord),     stop_i],
              to_i   = patk[.(seg_pt$pid, seg_pt$ord + 1L), stop_i],
              route_id = pat_info$route_id[match(pid, pat_info$pid)])]
seg_load <- seg_pt[, .(flow = sum(flow)), by = .(route_id, from_i, to_i)]
seg_load <- merge(seg_load, unique(routes[, .(route_id, route_short_name, route_long_name)],
                                   by = "route_id"), by = "route_id", all.x = TRUE)

n_seg <- nrow(seg_load)
cc <- data.frame(
  id = rep(seq_len(n_seg), each = 2),
  x  = as.vector(rbind(stops$stop_lon[seg_load$from_i], stops$stop_lon[seg_load$to_i])),
  y  = as.vector(rbind(stops$stop_lat[seg_load$from_i], stops$stop_lat[seg_load$to_i])))
pt_seg_sf <- sfheaders::sf_linestring(cc, x = "x", y = "y", linestring_id = "id") |>
  sf::st_set_crs(4326)
pt_seg_sf <- cbind(pt_seg_sf, as.data.frame(seg_load))      # garis lurus antar-halte (bukan shapes)

# ------------------------------------------------------------------------------
# 6. SIMPAN
# ------------------------------------------------------------------------------
saveRDS(od_it,       "pt_od_itinerary.rds")
saveRDS(route_flow,  "pt_route_flow.rds")
saveRDS(stop_sum_sf, "pt_stop_summary.rds")
saveRDS(pt_seg_sf,   "pt_segment_load.rds")
fwrite(od_it,      "pt_od_itinerary.csv")
fwrite(route_flow, "pt_route_flow.csv")

# ------------------------------------------------------------------------------
# 7. PETA BEBAN PT PER SEGMEN & HALTE
# ------------------------------------------------------------------------------
tmap_mode("view")
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(pt_seg_sf) +
  tm_lines(col = "flow",
           lwd = "flow", lwd.scale = tm_scale_continuous(values.range = c(0.5, 6)),
           col.legend = tm_legend(title = "Penumpang PT per segmen"),
           lwd.legend = tm_legend_hide())

# ------------------------------------------------------------------------------
# 8. PETA ITINERARY SATU OD (untuk pemeriksaan): access - rute 1 - transfer - rute 2 - egress
# ------------------------------------------------------------------------------
plot_itinerary <- function(oz, dz) {
  r <- od_it[origin_zone == oz & destination_zone == dz]
  stopifnot(nrow(r) >= 1); r <- r[1]
  stopifnot(r$status %in% SERVED, !is.na(r$pid1))
  
  # jalan kaki (rute di graph pejalan kaki)
  wp <- rbindlist(list(
    if (isTRUE(r$walk_access)) data.table(type = "access", from = r$origin_vertex, to = r$board_stop_vertex),
    if (isTRUE(r$walk_egress)) data.table(type = "egress", from = r$alight_stop_vertex, to = r$destination_vertex)))
  wl <- NULL
  if (!is.null(wp) && nrow(wp)) {
    pth <- dodgr_paths(graph_with_od, from = wp$from, to = wp$to, pairwise = TRUE, vertices = FALSE)
    e <- lapply(pth, function(p) {
      if (is.null(p) || length(p) == 0) return(integer(0))
      x <- p[[1]]; if (is.null(x)) integer(0) else as.integer(x)
    })
    gdf <- as.data.frame(graph_with_od)
    ee  <- data.table(type = rep(wp$type, lengths(e)), idx = unlist(e))
    if (nrow(ee)) {
      ia <- match(gdf$from_id[ee$idx], v$id); ib <- match(gdf$to_id[ee$idx], v$id)
      cw <- data.frame(id = rep(seq_len(nrow(ee)), each = 2),
                       x = as.vector(rbind(v$x[ia], v$x[ib])), y = as.vector(rbind(v$y[ia], v$y[ib])))
      wl <- sfheaders::sf_linestring(cw, x = "x", y = "y", linestring_id = "id") |> st_set_crs(4326)
      # sf_linestring membawa kolom 'id'; buat ulang hanya dengan type, label, geometry
      wl <- st_sf(type = ee$type, label = ee$type, geometry = st_geometry(wl))
    }
  }
  
  # leg PT (garis lurus antar-halte menurut urutan pola)
  leg_line <- function(pid_in, o1, o2, type, label) {
    s <- pat_pid[.(pid_in)]; s <- s[ord >= o1 & ord <= o2]   # jangan pakai nama 'pid' (nama kolom)
    st_sf(type = type, label = label,
          geometry = st_sfc(st_linestring(cbind(stops$stop_lon[s$stop_i], stops$stop_lat[s$stop_i])),
                            crs = 4326))
  }
  pl <- list(leg_line(r$pid1, r$from1, r$to1, "pt1", paste0("Rute 1: ", r$route_short1)))
  if (r$ntr == 1) pl[[2]] <- leg_line(r$pid2, r$from2, r$to2, "pt2", paste0("Rute 2: ", r$route_short2))
  parts <- c(list(wl)[!is.null(wl)], pl)
  lines_all <- do.call(rbind, lapply(parts, function(x) x[, c("type", "label")]))   # kolom harus identik
  lines_all$type <- factor(lines_all$type, levels = c("access", "pt1", "pt2", "egress"))
  
  pts <- data.frame(
    role = c("naik", "turun", if (r$ntr == 1) c("transfer turun", "transfer naik")),
    label = c(r$board_stop_name, r$alight_stop_name,
              if (r$ntr == 1) c(r$xfer_alight_stop_name, r$xfer_board_stop_name)),
    lon = c(r$board_stop_lon, r$alight_stop_lon,
            if (r$ntr == 1) c(stops$stop_lon[r$xfer_alight], stops$stop_lon[r$xfer_board])),
    lat = c(r$board_stop_lat, r$alight_stop_lat,
            if (r$ntr == 1) c(stops$stop_lat[r$xfer_alight], stops$stop_lat[r$xfer_board])))
  pts <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
  
  tmap_mode("view")
  tm_basemap("Esri.WorldGrayCanvas") +
    tm_shape(lines_all) +
    tm_lines(col = "type", lwd = 4,
             col.scale = tm_scale_categorical(
               values = c("#2ca02c", "#1f77b4", "#ff7f0e", "#d62728"),
               levels = c("access", "pt1", "pt2", "egress")),
             col.legend = tm_legend(title = "Leg")) +
    tm_shape(pts) + tm_dots(fill = "black", size = 0.8) + tm_text("label", size = 0.7)
}
ex <- od_pt[ntr == 1][1]; plot_itinerary(ex$origin_zone, ex$destination_zone)
