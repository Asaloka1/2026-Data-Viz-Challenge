# Figure 2: Long-Haul Rice Milling Route
# Carolinas and Arkansas

# Packages and map options
library(tidyverse)
library(sf)
library(tigris)
library(maps)
library(gridExtra)
library(grid)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# Confirm that both project data files are available
required_files <- c("final_rice_farming_layer_2024.csv", "final_rice_milling_infrastructure_2024.csv")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste0("Missing required file(s):\n", paste(missing_files, collapse = "\n"),
              "\n\nWorking directory:\n", getwd()))
}

# Import and standardize county-level establishment files
rice_farms <- read_csv(required_files[1], show_col_types = FALSE) %>%
  mutate(area_fips = str_pad(as.character(area_fips), 5, "left", "0"))

rice_mills <- read_csv(required_files[2], show_col_types = FALSE) %>%
  mutate(area_fips = str_pad(as.character(area_fips), 5, "left", "0"))

rice_data <- full_join(rice_farms, rice_mills, by = "area_fips") %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# Add locations identified through Extension discussions
extension_data <- tibble(
  area_fips = c("37137", "45043", "45079"),
  rice_farm_estabs_ext = c(1, 1, 1),
  rice_mill_estabs_ext = c(1, 0, 1),
  is_extension = TRUE
)

rice_data <- rice_data %>%
  full_join(extension_data, by = "area_fips") %>%
  mutate(
    is_extension = replace_na(is_extension, FALSE),
    rice_farm_estabs = if_else(is_extension, replace_na(rice_farm_estabs_ext, 0), replace_na(rice_farm_estabs, 0)),
    rice_mill_estabs = if_else(is_extension, replace_na(rice_mill_estabs_ext, 0), replace_na(rice_mill_estabs, 0))
  ) %>%
  select(-rice_farm_estabs_ext, -rice_mill_estabs_ext)

# Download 2024 county and state boundaries for the three study states
counties_sf <- tigris::counties(state = c("05", "37", "45"), cb = TRUE, year = 2024, class = "sf")

states_sf <- tigris::states(cb = TRUE, year = 2024, class = "sf") %>%
  filter(GEOID %in% c("05", "37", "45")) %>%
  st_transform(4326)

rice_map <- counties_sf %>%
  left_join(rice_data, by = c("GEOID" = "area_fips")) %>%
  mutate(
    rice_farm_estabs = replace_na(rice_farm_estabs, 0),
    rice_mill_estabs = replace_na(rice_mill_estabs, 0),
    is_extension = replace_na(is_extension, FALSE)
  ) %>%
  st_transform(4326)

# Create county representative points for milling-establishment symbols
suppressWarnings({
  mill_points <- rice_map %>%
    st_point_on_surface() %>%
    select(GEOID, STATEFP, rice_mill_estabs, is_extension) %>%
    filter(rice_mill_estabs > 0)
})

official_mills <- mill_points %>% filter(!is_extension)
extension_mills <- mill_points %>% filter(is_extension)

# Build a Southeast state layer for geographic context
southeast_states <- c("arkansas", "louisiana", "mississippi", "alabama", "tennessee", "kentucky", "georgia", "north carolina", "south carolina", "virginia")
state_labels <- tibble(ID = southeast_states, abb = c("AR", "LA", "MS", "AL", "TN", "KY", "GA", "NC", "SC", "VA"))

southeast_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  st_set_crs(4326) %>%
  filter(ID %in% southeast_states) %>%
  left_join(state_labels, by = "ID")

# Calculate state-label positions and route anchors
suppressWarnings({
  state_anchor_points <- southeast_map %>% st_point_on_surface()
})

anchor_coordinates <- st_coordinates(state_anchor_points)
state_anchor_points <- state_anchor_points %>%
  mutate(lon = anchor_coordinates[, 1], lat = anchor_coordinates[, 2])

ar_anchor <- filter(state_anchor_points, abb == "AR")
nc_anchor <- filter(state_anchor_points, abb == "NC")
sc_anchor <- filter(state_anchor_points, abb == "SC")

ar_lon <- ar_anchor$lon[1]; ar_lat <- ar_anchor$lat[1]
nc_lon <- nc_anchor$lon[1]; nc_lat <- nc_anchor$lat[1]
sc_lon <- sc_anchor$lon[1]; sc_lat <- sc_anchor$lat[1]

# Draw the Southeast map, establishment patterns, and long-haul routes
distance_map <- ggplot() +
  geom_sf(data = southeast_map, fill = "#f0f0f0", color = "white", linewidth = 0.55) +
  geom_sf(data = filter(southeast_map, abb == "AR"), fill = "#d9d9d9", color = "#616161", linewidth = 1.1) +
  geom_sf(data = filter(southeast_map, abb %in% c("NC", "SC")), fill = "#d9d9d9", color = "#616161", linewidth = 1.1) +
  geom_sf(data = rice_map, fill = "#d9d9d9", color = "white", linewidth = 0.27) +
  geom_sf(data = filter(rice_map, rice_farm_estabs > 0, !is_extension), aes(fill = rice_farm_estabs), color = "white", linewidth = 0.28, alpha = 0.95) +
  scale_fill_gradient(low = "#fff59d", high = "#e65100", limits = c(0, 24), oob = scales::squish, guide = "none") +
  geom_sf(data = filter(rice_map, is_extension), fill = "#FFDAB9", color = "white", linewidth = 0.28) +
  geom_sf(data = states_sf, fill = NA, color = "#616161", linewidth = 0.7) +
  geom_sf(data = official_mills, aes(size = rice_mill_estabs), color = "#4a148c", shape = 16, alpha = 0.85) +
  geom_sf(data = extension_mills, aes(size = rice_mill_estabs), color = "#00c853", shape = 19, alpha = 0.95) +
  scale_size_continuous(range = c(5, 11), limits = c(1, 4), guide = "none") +
  geom_curve(aes(x = ar_lon + 0.85, y = ar_lat + 0.30, xend = nc_lon - 0.90, yend = nc_lat + 0.18), curvature = -0.17, linewidth = 2.2, color = "#0010f5", arrow = arrow(type = "closed", ends = "both", length = unit(0.34, "cm"))) +
  geom_curve(aes(x = ar_lon + 0.85, y = ar_lat - 0.30, xend = sc_lon - 0.55, yend = sc_lat - 0.18), curvature = 0.15, linewidth = 2.2, color = "#0010f5", arrow = arrow(type = "closed", ends = "both", length = unit(0.34, "cm"))) +
  annotate("label", x = -84.85, y = 36.25, label = "845 miles", size = 5.5, fontface = "bold", color = "#0010f5", fill = "white", label.size = 0.7, label.padding = unit(0.25, "lines")) +
  annotate("label", x = -87.85, y = 33.10, label = "750 miles", size = 5.5, fontface = "bold", color = "#0010f5", fill = "white", label.size = 0.7, label.padding = unit(0.25, "lines")) +
  geom_text(data = filter(state_anchor_points, !abb %in% c("AR", "NC", "SC")), aes(lon, lat, label = abb), size = 4, fontface = "bold", color = "#8a99a6") +
  annotate("text", x = ar_lon - 0.15, y = ar_lat + 0.10, label = "AR", size = 6, fontface = "bold", color = "#212121") +
  annotate("text", x = nc_lon + 0.25, y = nc_lat + 0.15, label = "NC", size = 6, fontface = "bold", color = "#212121") +
  annotate("text", x = sc_lon + 0.42, y = sc_lat - 0.30, label = "SC", size = 6, fontface = "bold", color = "#212121") +
  annotate("label", x = -84.80, y = 33.65, label = "Limited local milling may require \ntravel to larger processing centers.", size = 3.8, fontface = "bold", lineheight = 1.05, color = "#263238", fill = "#FFFFFF", label.size = 0.55, label.padding = unit(0.25, "lines")) +
  coord_sf(xlim = c(-94.80, -75.10), ylim = c(30.00, 37.85), expand = FALSE, datum = NA) +
  labs(title = "Figure 2. Long-Haul Rice Milling Route", subtitle = paste0("Surrounding states provide geographic context while the arrows show ", "the distance between rice farming and rice milling establishments in the Carolinas and Arkansas.")) +
  theme_void() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "#1f2933", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11.5, hjust = 0.5, color = "#52606d", margin = margin(b = 10)),
    panel.border = element_rect(fill = NA, color = "#7b8794", linewidth = 0.8),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.margin = margin(8, 12, 8, 12)
  )

# Farming-establishment legend
farming_legend_data <- tibble(
  x = c(1.00, 1.75, 2.50, 3.25, 4.00), y = 1,
  category = factor(c("1–4", "5–14", "15–23", "24", "Extension*"), levels = c("1–4", "5–14", "15–23", "24", "Extension*"))
)

farming_legend <- ggplot(farming_legend_data, aes(x, y)) +
  geom_tile(aes(fill = category), width = 0.72, height = 0.56, color = "white", linewidth = 0.35) +
  scale_fill_manual(values = c("1–4" = "#fff59d", "5–14" = "#ffb74d", "15–23" = "#f57c00", "24" = "#e65100", "Extension*" = "#FFDAB9"), guide = "none") +
  geom_text(aes(label = category), size = 2.7, fontface = "bold", color = "#212121") +
  labs(title = "RICE FARMING ESTABLISHMENTS") +
  xlim(0.55, 4.45) + ylim(0.60, 1.40) + theme_void() +
  theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5, color = "#212121", margin = margin(b = 1)), plot.margin = margin(0, 2, 0, 2))

# Milling-establishment legend
mill_legend_data <- tibble(x = c(1.00, 2.05, 3.30), y = 0.90, mill_count = c(1, 2, 4))

processing_legend <- ggplot() +
  geom_point(data = mill_legend_data, aes(x, y, size = mill_count), shape = 16, color = "#4a148c", alpha = 0.85) +
  scale_size_continuous(range = c(4, 9), limits = c(1, 4), guide = "none") +
  geom_text(data = mill_legend_data, aes(x, y = 0.38, label = mill_count), size = 2.8, fontface = "bold", color = "#212121") +
  annotate("point", x = 4.75, y = 0.90, shape = 19, size = 6, color = "#00c853", alpha = 0.95) +
  annotate("text", x = 4.75, y = 0.38, label = "Extension*", size = 2.6, fontface = "bold", color = "#212121") +
  annotate("text", x = 2.15, y = 1.55, label = "RICE MILL ESTABLISHMENTS", size = 3.2, fontface = "bold", color = "#212121") +
  annotate("text", x = 4.75, y = 1.55, label = "EXTENSION-IDENTIFIED FACILITY", size = 2.8, fontface = "bold", color = "#212121") +
  xlim(0.35, 5.55) + ylim(0.15, 1.80) + theme_void() +
  theme(plot.margin = margin(0, 2, 0, 2))

# Combine the legend, note, and map
distance_legend <- arrangeGrob(farming_legend, processing_legend, ncol = 2, widths = c(1.10, 1))
figure_note <- textGrob("*Extension-identified locations provide context for activity that may not be visible in the 2024 Local Food Data Warehouse.", x = 0.01, hjust = 0, gp = gpar(fontsize = 8.5, fontface = "italic", col = "#000000"))
figure2_final <- arrangeGrob(distance_map, distance_legend, figure_note, ncol = 1, heights = c(5.00, 0.66, 0.18))

grid.newpage()
grid.draw(figure2_final)

# Export high-resolution PNG and PDF files
ggsave("Figure2_Long_Haul_Rice_Milling_Route.png", figure2_final, width = 16, height = 9, units = "in", dpi = 600, bg = "white", limitsize = FALSE)
ggsave("Figure2_Long_Haul_Rice_Milling_Route.pdf", figure2_final, width = 16, height = 9, units = "in", bg = "white", limitsize = FALSE)

cat("Figure 2 created successfully in:", getwd(), "\n")
