#visualisation


jakarta_kota <- st_read("D:/Github/schoolMobility/Data/batas_wil.gpkg") %>%
  st_zm(drop = TRUE) %>%
  st_make_valid() %>%
  st_transform(32748) %>%
  filter(WADMKK != "Kab. Adm. Kep. Seribu")


jakarta_kota_4326 <- jakarta_kota %>%
  st_transform(4326)


label_kota <- jakarta_kota_4326 %>%
  st_point_on_surface() %>%
  mutate(
    nama_kota = case_when(
      NAMOBJ == "Kota Adm. Jakarta Pusat"   ~ "Central Jakarta",
      NAMOBJ == "Kota Adm. Jakarta Utara"   ~ "North Jakarta",
      NAMOBJ == "Kota Adm. Jakarta Barat"   ~ "West Jakarta",
      NAMOBJ == "Kota Adm. Jakarta Selatan" ~ "South Jakarta",
      NAMOBJ == "Kota Adm. Jakarta Timur"   ~ "East Jakarta",
      TRUE ~ NAMOBJ
    )
  )

# Visualisasi study area
tmap_mode("plot")
tm_basemap("Esri.WorldGrayCanvas") +
  
  tm_shape(jakarta_kota_4326) +
  tm_lines(
    col = "red",
    lwd = 2
  ) +
  
  # Menambahkan nama kota di dalam wilayahnya
  tm_shape(label_kota) +
  tm_text(
    text = "nama_kota",
    size = 0.8,
    col = "black",
    fontface = "bold"
  )

# Fungsi untuk menghasilkan peta
make_demand_map <- function(data, flow_var, map_title) {
  
  tm_basemap("Esri.WorldGrayCanvas") +
    
    tm_shape(data %>% filter(.data[[flow_var]] > 0)) +
    tm_lines(
      col = flow_var,
      col.scale = shared_scale,
      col.legend = tm_legend(
        title = "Pedestrian flow"
      ),
      lwd = 2
    ) +
    
    tm_shape(jakarta_kota_4326) +
    tm_lines(
      col = "red",
      lwd = 1.5
    ) +
    
    tm_layout(
      title = map_title,
      title.size = 1,
      title.fontfamily = "sans",
      title.fontface = "bold",
      title.color = "black",
      title.position = c("left", "top")
    )
}

# Peta 1: Short-distance demand
map_short <- make_demand_map(
  edges_pedestrian,
  "flow",
  "A. Short-distance demand"
)

# Peta 2: Multimodal access/egress to PT
map_pt <- make_demand_map(
  edges_ped_pt,
  "flow_total",
  "B. Multimodal PT demand"
)

# Peta 3: Total demand
map_all <- make_demand_map(
  edges_all,
  "flow_total",
  "C. Total pedestrian demand"
)

# Susun secara vertikal: 3 baris x 1 kolom
tmap_arrange(
  map_short,
  map_pt,
  map_all,
  nrow = 1,
  ncol = 3,
  sync = FALSE
)



#flow each city
# A. dari geometri (meter, geodesik untuk koordinat lon/lat)
edges_all$len_geom_m <- as.numeric(st_length(edges_all))

# B. dari atribut graph (d, meter), dicocokkan lewat kunci ruas tak berarah (a, b)
g <- as.data.table(as.data.frame(graph_with_od))[
  , .(a = pmin(from_id, to_id), b = pmax(from_id, to_id), d)][
    , .(len_m = mean(d)), by = .(a, b)]          # dua arah / edge paralel dirata-rata

edges_all <- edges_all %>% left_join(as.data.frame(g), by = c("a", "b"))

# pemeriksaan
sum(is.na(edges_all$len_m))                                   # harus 0
summary(edges_all$len_m)                                      # median wajar: puluhan sampai ratusan meter
summary(edges_all$len_m / edges_all$len_geom_m)               # sekitar 1,0-1,1 jika graph tidak di-contract

# ukuran paparan pejalan kaki
edges_all$ped_km <- edges_all$flow_total * edges_all$len_m / 1000

tmap_mode("plot")
tm_shape(edges_all) +
  tm_lines(
    col = "blue",
    lwd = 2
  ) +
  tm_shape(jakarta_kota_4326) +
  tm_lines(
    col = "red",
    lwd = 1.5
  )


jakarta_pusat <- jakarta_kota_4326 %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Pusat")

jakarta_barat <- jakarta_kota_4326 %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Barat")

jakarta_selatan <- jakarta_kota_4326 %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Selatan")

jakarta_timur <- jakarta_kota_4326 %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Timur")

jakarta_utara <- jakarta_kota_4326 %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Utara")

demand_jakarta_pusat <- st_intersection(
  edges_all,
  jakarta_pusat
)

demand_jakarta_barat <- st_intersection(
  edges_all,
  jakarta_barat
)

demand_jakarta_selatan <- st_intersection(
  edges_all,
  jakarta_selatan
)

demand_jakarta_timur <- st_intersection(
  edges_all,
  jakarta_timur
)

demand_jakarta_utara <- st_intersection(
  edges_all,
  jakarta_utara
)

summary_demand <- function(data, nama_wilayah) {
  data %>%
    st_drop_geometry() %>%
    summarise(
      `Total pedestrian demand (estimation)` = sum(flow_total, na.rm = TRUE),
      `Flow pedestrian (short distance)` = sum(flow_walkonly, na.rm = TRUE),
      `Flow pedestrian (PT)` = sum(flow_pt, na.rm = TRUE),
      `Total length (m)` = sum(len_m, na.rm = TRUE),
      `Pedestrian.km` = sum(ped_km, na.rm = TRUE)
    ) %>%
    mutate(
      `Wilayah` = nama_wilayah,
      .before = 1
    )
}

# Summary masing-masing wilayah
summary_jakarta <- bind_rows(
  summary_demand(demand_jakarta_pusat, "Central Jakarta"),
  summary_demand(demand_jakarta_barat, "West jakarta"),
  summary_demand(demand_jakarta_selatan, "South Jakarta"),
  summary_demand(demand_jakarta_timur, "East Jakarta"),
  summary_demand(demand_jakarta_utara, "North Jakarta")
  ) 

#TOP 5 pedestrian demand
tmap_mode("view")
  
tm_shape(edges_all %>% filter(edges_all$flow_total >= 20000, edges_all$flow_total <= 40000)) +
  tm_lines(
    col = "blue",
    lwd = 3
  ) +
  tm_shape(jakarta_kota_4326) +
  tm_lines(
    col = "red",
    lwd = 1.5
  )

edges_excel <- edges_all %>%
  st_drop_geometry()
write_xlsx(edges_excel, "edges_excel.xlsx")

#composition of pedestrian demand
plot_data <- summary_jakarta %>%
  mutate(
    `Total pedestrian demand (estimation)` =
      `Flow pedestrian (PT)` +
      `Flow pedestrian (short distance)`
  ) %>%
  pivot_longer(
    cols = c(
      `Flow pedestrian (PT)`,
      `Flow pedestrian (short distance)`
    ),
    names_to = "Demand type",
    values_to = "Flow"
  ) %>%
  mutate(
    `Demand type` = factor(
      `Demand type`,
      levels = c(
        "Flow pedestrian (PT)",
        "Flow pedestrian (short distance)"
      )
    ),
    Wilayah = factor(
      Wilayah,
      levels = summary_jakarta$Wilayah
    )
  )

# Total demand untuk label di atas batang
total_labels <- summary_jakarta %>%
  mutate(
    Total = `Total pedestrian demand (estimation)`,
    Wilayah = factor(
      Wilayah,
      levels = summary_jakarta$Wilayah
    )
  )

# Membuat stacked bar chart
ggplot(
  plot_data,
  aes(x = Wilayah, y = Flow, fill = `Demand type`)
) +
  geom_col(
    width = 0.7,
    position = position_stack(reverse = TRUE)
  ) +
  
  # Label total demand di atas setiap batang
  geom_text(
    data = total_labels,
    aes(
      x = Wilayah,
      y = Total,
      label = comma(Total, accuracy = 1)
    ),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = c(
      "Flow pedestrian (PT)" = "#31688E",
      "Flow pedestrian (short distance)" = "#F4A259"
    ),
    breaks = c(
      "Flow pedestrian (PT)",
      "Flow pedestrian (short distance)"
    ),
    labels = c(
      "PT access/egress",
      "Short-distance trips"
    )
  ) +
  
  scale_y_continuous(
    labels = label_number(
      big.mark = ",",
      accuracy = 1
    ),
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  labs(
    title = "Pedestrian Demand Composition by Jakarta Region",
    x = NULL,
    y = "Estimated Pedestrian demand (trip)",
    fill = "Demand type"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Visualisasi jakarta pedestrian network

jakarta_network <- st_read(
  "D:/Github/schoolMobility/Data/jakarta_network.gpkg"
)

dedicated_classes <- c("footway", "path", "pedestrian", "steps")
shared_classes <- c("living_street", "residential")


jakarta_dedicated <- jakarta_network %>%
  filter(fclass %in% dedicated_classes) %>%
  st_cast("LINESTRING") %>%
  mutate(
    oneway = FALSE,
    length_m = as.numeric(st_length(geom)),
    edge_id = row_number()
  )

jakarta_dedicated_4326 <- jakarta_dedicated %>%
  st_transform(4326)


jakarta_shared <- jakarta_network %>%
  filter(fclass %in% shared_classes) %>%
  st_cast("LINESTRING") %>%
  mutate(
    oneway = FALSE,
    length_m = as.numeric(st_length(geom)),
    edge_id = row_number()
  )

jakarta_shared_4326 <- jakarta_shared %>%
  st_transform(4326)

jakarta_network_4326 <- jakarta_network %>%
  st_transform(4326)

tmap_mode("view")
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(jakarta_shared_4326)+
  tm_lines(
    col = "#1AAD4E",
    lwd = 1
  ) +
  tm_shape(jakarta_dedicated_4326)+
  tm_lines(
    col = "blue",
    lwd = 1
  ) 
  tm_shape(jakarta_road_4326)+
  tm_lines(
    col = "blue",
    lwd = 1
  ) +
  tm_shape(jakarta_kota_4326) +
  tm_lines(
    col = "red",
    lwd = 2
  ) +
  
  # Menambahkan nama kota di dalam wilayahnya
  tm_shape(label_kota) +
  tm_text(
    text = "nama_kota",
    size = 0.8,
    col = "black",
    fontface = "bold"
  )

jakarta_pusat_32748 <- jakarta_kota %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Pusat")

jakarta_barat_32748 <- jakarta_kota %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Barat")

jakarta_selatan_32748 <- jakarta_kota %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Selatan")

jakarta_timur_32748 <- jakarta_kota %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Timur")

jakarta_utara_32748 <- jakarta_kota %>%
  filter(NAMOBJ == "Kota Adm. Jakarta Utara")

priority_jakarta_pusat <- st_intersection(
  edges_utm,
  jakarta_pusat_32748
)

priority_jakarta_barat <- st_intersection(
  edges_utm,
  jakarta_barat_32748
)

priority_jakarta_selatan <- st_intersection(
  edges_utm,
  jakarta_selatan_32748
)

priority_jakarta_timur <- st_intersection(
  edges_utm,
  jakarta_timur_32748
)

priority_jakarta_utara <- st_intersection(
  edges_utm,
  jakarta_utara_32748
)

summary_priority <- function(data, nama_wilayah) {
  data %>%
    st_drop_geometry() %>%
    group_by(priority) %>%
    summarise(
      jumlah_edge         = n(),
      total_flow_walkonly = sum(flow_walkonly, na.rm = TRUE),
      total_flow_acc      = sum(flow_acc, na.rm = TRUE),
      total_flow_egr      = sum(flow_egr, na.rm = TRUE),
      total_flow_pt       = sum(flow_pt, na.rm = TRUE),
      total_len_m         = sum(len_m, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Wilayah = nama_wilayah,
      .before = 1
    ) %>%
    select(
      Wilayah,
      priority,
      jumlah_edge,
      total_flow_walkonly,
      total_flow_acc,
      total_flow_egr,
      total_flow_pt,
      total_len_m
    )
}

# Summary masing-masing wilayah
summary_jakarta_priority <- bind_rows(
  summary_priority(priority_jakarta_pusat, "Central Jakarta"),
  summary_priority(priority_jakarta_barat, "West Jakarta"),
  summary_priority(priority_jakarta_selatan, "South Jakarta"),
  summary_priority(priority_jakarta_timur, "East Jakarta"),
  summary_priority(priority_jakarta_utara, "North Jakarta")
)
