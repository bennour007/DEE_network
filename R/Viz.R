

pacman::p_load(
  tidyverse,
  sf, 
  # tmap,
  ggraph,
  rnaturalearth
)




# --- 1. country centroids as an sf points layer (the NODES) ---
world <- ne_countries(scale = "medium", returnclass = "sf")
cent <- world %>%
  select(iso = iso_a3_eh, geometry) %>%          # _eh = more complete iso3
  st_centroid() %>%
  filter(!is.na(iso), iso != "-99")

coords <- cent %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# --- 2. edges = trade flows, one slice, top-3 suppliers per importer ---
edges <- trade_hq %>%
  filter(year == 2021) %>%
  group_by(iso_d, iso_o,  tech_group) %>%
  summarise(value = sum(trade_value_pred), .groups = "drop") %>%
  group_by(iso_d, tech_group) %>%
  slice_max(value, n = 4) %>%
  ungroup() %>%
  inner_join(coords, by = c("iso_o" = "iso")) %>% rename(x0 = lon, y0 = lat) %>%
  inner_join(coords, by = c("iso_d" = "iso")) %>% rename(x1 = lon, y1 = lat)

# --- 3. turn each edge into an sf LINESTRING ---
edge_lines <- edges %>%
  rowwise() %>%
  mutate(geometry = st_sfc(st_linestring(matrix(c(x0, x1, y0, y1), 2, 2)),
                           crs = 4326)) %>%
  st_as_sf()

# # --- 4. draw: basemap + edges (width = value) + nodes ---
# tm_shape(world) +
#   tm_polygons(col = "grey95", border.col = "white") +
#   tm_shape(edge_lines) +
#   tm_lines(lwd = "value", col = "steelblue", alpha = 0.3, scale = 3, legend.lwd.show = FALSE) +
#   tm_shape(cent) +
#   tm_dots(col = "grey40", size = 0.05) +
#   tm_layout(frame = FALSE, main.title = "Top digital-infrastructure suppliers, 2021")


pacman::p_load(showtext,sysfonts)

# --- load a professional font from Google Fonts ---
  font_add_google("Montserrat")        # clean modern sans; great for slides
# alternatives: "Source Sans 3", "Roboto", "Lato", "IBM Plex Sans"
showtext_auto()                           # render all text with showtext
showtext_opts(dpi = 300)                  # match your export dpi (important!)


ggplot() +
  geom_sf(data = world, fill = "grey95", color = "white", linewidth = 0.2) +
  geom_segment(data = edges,
               aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = value, color = tech_group),
               # color = "steelblue", 
               alpha = 0.3) +
  geom_point(data = coords, aes(lon, lat), size = 0.1, color = "grey50", alpha = 0.3) +
  scale_linewidth(range = c(0.1, 1.5), guide = "none") +
  coord_sf(crs = 4326, expand = FALSE) +     # same CRS as the data → lines land right
  # theme_void() +
  facet_wrap(~ tech_group) +
  labs(title = "Top 4 digital-infrastructure suppliers, 2021") +
  theme_void(base_family = "Montserrat") +      # <- font applied to the whole theme
  theme(
    plot.title   = element_text(family = "Montserrat", face = "bold", size = 18,
                                hjust = 0, margin = margin(b = 10)),
    strip.text   = element_text(family = "Montserrat", face = "bold", size = 12),
    legend.position = "bottom",
    legend.text  = element_text(family = "Montserrat", size = 10),
    plot.margin  = margin(15, 15, 15, 15)
  ) -> trade_map


ggsave(filename = "trade_map.png", plot = trade_map, width = 12, height = 7, dpi = 300, bg = "white")


tidy_obj <- edges %>% 
  rename(
    to = iso_d,  from = iso_o
  ) %>% 
  tidygraph::as_tbl_graph()



tidy_obj %>% 
  ggraph(layout = "linear") +
  geom_edge_arc(
    aes(color = tech_group)
  ) +
  theme_void() +
  geom_node_arc_bar()
  # geom_sf(data = world, fill = "grey95", color = "white", linewidth = 0.2) +
  geom_segment(data = edges,
               aes(x = x0, y = y0, xend = x1, yend = y1, linewidth = value, color = tech_group),
               # color = "steelblue", 
               alpha = 0.3) +
  geom_point(data = coords, aes(lon, lat), size = 0.1, color = "grey50", alpha = 0.3) +
  scale_linewidth(range = c(0.1, 1.5), guide = "none") +
  coord_sf(crs = 4326, expand = FALSE) +     # same CRS as the data → lines land right
  theme_void() +
  facet_wrap(~ tech_group) +
  labs(title = "Top digital-infrastructure suppliers, 2021") +
  theme(
    legend.position = "bottom"
  )

