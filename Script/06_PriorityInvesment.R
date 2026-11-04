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

# GAP ANALYSIS: Pedestrian Demand vs Infrastructure availability
# parameter
TOL_M          <- 15      # tolerance to capture pedestrian infrastructure in meter
DEDICATED      <- c("footway", "path", "pedestrian", "steps")
SHARED_STREET  <- c("living_street", "residential")
TOP_Q          <- 0.80    # (top 20%), capture only high demand 

# Projecting data into meters and calculating the midpoint of a segment
edges_utm <- edges_all %>%
  filter(!is.na(flow_total), flow_total > 0) %>%
  st_transform(32748)
edges_utm$len_m <- as.numeric(st_length(edges_utm))
mid <- st_centroid(st_geometry(edges_utm))

ped_utm <- st_transform(jakarta_pedestrian_4326, 32748)
ped_ded <- ped_utm %>% filter(fclass %in% DEDICATED)
ped_shr <- ped_utm %>% filter(fclass %in% SHARED_STREET)

near_dist <- function(pts, lines) {
  g <- st_geometry(lines)                 
  stopifnot(length(g) > 0)                
  idx <- st_nearest_feature(pts, g)
  as.numeric(st_distance(pts, g[idx], by_element = TRUE))
}

edges_utm$d_ded <- near_dist(mid, ped_ded)
edges_utm$d_shr <- near_dist(mid, ped_shr)


# segment classification
edges_utm <- edges_utm %>%
  mutate(supply = case_when(
    d_ded <= TOL_M ~ "dedicated",             # footway/path/pedestrian/steps
    d_shr <= TOL_M ~ "shared_street",         # residential/living_street/track
    TRUE           ~ "no_facility_in_data"    # no facility captured in OSM
  ))

thr <- quantile(edges_utm$flow_total, TOP_Q, na.rm = TRUE)
edges_utm <- edges_utm %>%
  mutate(priority = case_when(
    flow_total >= thr & supply == "no_facility_in_data" ~ "1. High demand - without facility",
    flow_total >= thr & supply == "shared_street"       ~ "2. High demand - using shared street",
    flow_total >= thr & supply == "dedicated"           ~ "3. High demand - dedicated facility",
    TRUE                                                ~ "4. other demand"
  ))


# measure pedestrian exposure (ped-km = flow x segment length)
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


# Investment priority map
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


# Clustering analysis for priority 1 , 2, and 3

PRIORITY_LABEL <- "1. High demand - without facility"

EPS         <- 300 
MINPTS      <- 10
EPS_SENS    <- c(150, 200, 300, 500, 750)
MINPTS_SENS <- c(5, 10, 20)

RADIUS_Q      <- 1                 
MIN_RADIUS_M  <- 100
WEIGHT_CENTER <- TRUE

# Segment priority 1

p1 <- edges_utm %>% filter(priority == PRIORITY_LABEL)
xy <- st_coordinates(st_centroid(st_geometry(p1)))
p1$x <- xy[, 1]; p1$y <- xy[, 2]

# DBSCAN clustering analysis 
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

# Final Cluster in Priority 1
db <- dbscan(xy, eps = EPS, minPts = MINPTS)
p1$cluster <- db$cluster
if (!any(p1$cluster > 0)) stop("Tidak ada klaster; kecilkan MINPTS atau besarkan EPS.")

noise <- p1 %>% st_drop_geometry() %>% filter(cluster == 0)
cat(sprintf("Klaster: %d | noise: %.1f%% ruas, %.1f%% panjang, %.1f%% ped-km\n",
            n_distinct(p1$cluster[p1$cluster > 0]),
            100 * nrow(noise) / nrow(p1),
            100 * sum(noise$len_m) / sum(p1$len_m),
            100 * sum(noise$flow_total * noise$len_m) / sum(p1$flow_total * p1$len_m)))

# create circle for visualisation

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

# map visualisation for cluster priority 1
circ4326    <- st_transform(circ_sf, 4326)
lines_cl    <- st_transform(p1 %>% filter(cluster > 0),  4326)
lines_noise <- st_transform(p1 %>% filter(cluster == 0), 4326)

circ_top10 <- circ4326 %>% filter(rank <= 10)
circ_other <- circ4326 %>% filter(rank > 10)

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
      "average_flow_per_km", "radius_m"
    )
  ) +
  
  # spesicial treatment for top 10
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




