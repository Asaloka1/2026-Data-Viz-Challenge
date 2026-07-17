setwd("C:/Users/Moi/Documents/AAEA/bls")

# Figure 1: Rice Production and Milling Infrastructure
# Arkansas and the Carolinas

# Packages and map options
library(tidyverse)
library(sf)
library(tigris)
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

# Import county-level farming and milling files and standardize county FIPS codes
rice_farms <- read_csv(required_files[1], show_col_types = FALSE) %>%
  mutate(area_fips = str_pad(as.character(area_fips), 5, "left", "0"))

rice_mills <- read_csv(required_files[2], show_col_types = FALSE) %>%
  mutate(area_fips = str_pad(as.character(area_fips), 5, "left", "0"))

# Merge both files and replace missing numeric values with zero
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

# Download 2024 county and state boundaries for Arkansas and the Carolinas
counties_sf <- tigris::counties(state = c("05", "37", "45"), cb = TRUE, year = 2024, class = "sf")

states_sf <- tigris::states(cb = TRUE, year = 2024, class = "sf") %>%
  filter(GEOID %in% c("05", "37", "45"))

# Join establishment data to county geometry
rice_map <- counties_sf %>%
  left_join(rice_data, by = c("GEOID" = "area_fips")) %>%
  mutate(
    rice_farm_estabs = replace_na(rice_farm_estabs, 0),
    rice_mill_estabs = replace_na(rice_mill_estabs, 0),
    is_extension = replace_na(is_extension, FALSE)
  ) %>%
  st_transform(4326)

states_sf <- st_transform(states_sf, 4326)

# Use county interior points to display milling-establishment symbols
suppressWarnings({
  mill_points <- rice_map %>%
    st_point_on_surface() %>%
    select(GEOID, STATEFP, rice_mill_estabs, is_extension) %>%
    filter(rice_mill_estabs > 0)
})

official_mills <- mill_points %>% filter(!is_extension)
extension_mills <- mill_points %>% filter(is_extension)

# Create regional layers for the two map panels
arkansas_counties <- rice_map %>% filter(STATEFP == "05")
carolina_counties <- rice_map %>% filter(STATEFP %in% c("37", "45"))
arkansas_state <- states_sf %>% filter(GEOID == "05")
carolina_states <- states_sf %>% filter(GEOID %in% c("37", "45"))
arkansas_mills <- official_mills %>% filter(STATEFP == "05")
carolina_official_mills <- official_mills %>% filter(STATEFP %in% c("37", "45"))
carolina_extension_mills <- extension_mills %>% filter(STATEFP %in% c("37", "45"))

# Shared panel formatting
map_theme <- function() {
  theme_void() +
    theme(
      plot.title = element_text(size = 17, face = "bold", hjust = 0.5, color = "#1f2933", margin = margin(b = 3)),
      plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "#52606d", margin = margin(b = 8)),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none",
      plot.margin = margin(12, 18, 12, 18)
    )
}

# Arkansas panel
plot_arkansas <- ggplot() +
  geom_sf(data = arkansas_counties, fill = "#f0f0f0", color = "white", linewidth = 0.25) +
  geom_sf(data = filter(arkansas_counties, rice_farm_estabs > 0), aes(fill = rice_farm_estabs), color = "white", linewidth = 0.30) +
  scale_fill_gradient(low = "#fff59d", high = "#e65100", limits = c(0, 24), oob = scales::squish, guide = "none") +
  geom_sf(data = arkansas_state, fill = NA, color = "#616161", linewidth = 0.9) +
  geom_sf(data = arkansas_mills, aes(size = rice_mill_estabs), color = "#4a148c", shape = 16, alpha = 0.85) +
  scale_size_continuous(range = c(4, 10), limits = c(1, 4), guide = "none") +
  geom_sf_text(data = filter(arkansas_counties, GEOID == "05111"), aes(label = "Poinsett"), size = 3.2, fontface = "bold", color = "#212121", nudge_x = -0.10, nudge_y = 0.08) +
  geom_sf_text(data = filter(arkansas_counties, GEOID == "05001"), aes(label = "Arkansas"), size = 3.2, fontface = "bold", color = "#212121", nudge_x = 0.12, nudge_y = 0.18) +
  coord_sf(xlim = c(-94.70, -89.55), ylim = c(32.85, 36.55), expand = FALSE, datum = NA) +
  labs(title = "Arkansas Delta", subtitle = "Centralized rice production and industrial milling hubs") +
  map_theme()

# Carolinas panel
plot_carolinas <- ggplot() +
  geom_sf(data = carolina_counties, fill = "#f0f0f0", color = "white", linewidth = 0.25) +
  geom_sf(data = filter(carolina_counties, rice_farm_estabs > 0, !is_extension), aes(fill = rice_farm_estabs), color = "white", linewidth = 0.30) +
  scale_fill_gradient(low = "#fff59d", high = "#e65100", limits = c(0, 24), oob = scales::squish, guide = "none") +
  geom_sf(data = filter(carolina_counties, is_extension), fill = "#FFDAB9", color = "white", linewidth = 0.30) +
  geom_sf(data = carolina_states, fill = NA, color = "#616161", linewidth = 0.9) +
  geom_sf(data = carolina_official_mills, aes(size = rice_mill_estabs), color = "#4a148c", shape = 16, alpha = 0.85) +
  geom_sf(data = carolina_extension_mills, aes(size = rice_mill_estabs), color = "#00c853", shape = 19, alpha = 0.95) +
  scale_size_continuous(range = c(4, 10), limits = c(1, 4), guide = "none") +
  geom_sf_label(data = filter(carolina_counties, GEOID == "37137"), aes(label = "Pamlico"), size = 3.1, fontface = "bold", fill = "white", color = "#212121", label.size = 0.25, nudge_x = -0.35, nudge_y = -0.17, label.padding = unit(0.12, "lines")) +
  geom_sf_label(data = filter(carolina_counties, GEOID == "45043"), aes(label = "Georgetown"), size = 3.1, fontface = "bold", fill = "white", color = "#212121", label.size = 0.25, nudge_x = -0.25, nudge_y = -0.20, label.padding = unit(0.12, "lines")) +
  geom_sf_label(data = filter(carolina_counties, GEOID == "45079"), aes(label = "Richland"), size = 3.1, fontface = "bold", fill = "white", color = "#212121", label.size = 0.25, nudge_x = -0.35, nudge_y = 0.14, label.padding = unit(0.12, "lines")) +
  coord_sf(xlim = c(-84.40, -75.20), ylim = c(31.90, 36.70), expand = FALSE, datum = NA) +
  labs(title = "The Carolinas", subtitle = "Heirloom rice production and limited regional milling infrastructure") +
  map_theme()

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

# Zero-establishment legend
zero_legend <- ggplot() +
  annotate("rect", xmin = 0.65, xmax = 1.35, ymin = 0.72, ymax = 1.28, fill = "#f0f0f0", color = "#bdbdbd", linewidth = 0.45) +
  annotate("text", x = 1, y = 1, label = "0", size = 3, fontface = "bold", color = "#616161") +
  annotate("text", x = 1, y = 1.58, label = "ZERO DATA", size = 3.2, fontface = "bold", color = "#212121") +
  annotate("text", x = 1, y = 0.38, label = "No recorded rice farms", size = 2.7, color = "#616161") +
  xlim(0.35, 1.65) + ylim(0.15, 1.80) + theme_void() +
  theme(plot.margin = margin(0, 2, 0, 2))

# Assemble the shared legend and place both maps inside one border
figure_legend <- arrangeGrob(farming_legend, processing_legend, zero_legend, ncol = 3, widths = c(1.15, 1, 0.42))
maps_inside <- arrangeGrob(plot_arkansas, plot_carolinas, ncol = 2, widths = c(1, 1.25), padding = unit(0, "line"))

maps_with_border <- grobTree(
  rectGrob(x = 0.5, y = 0.5, width = unit(1, "npc") - unit(1.5, "mm"), height = unit(1, "npc") - unit(1.5, "mm"), gp = gpar(fill = "white", col = "#7b8794", lwd = 1.2)),
  editGrob(maps_inside, vp = viewport(x = 0.5, y = 0.5, width = unit(1, "npc") - unit(7, "mm"), height = unit(1, "npc") - unit(7, "mm")))
)

# Add the figure heading and data note
figure_title <- textGrob("Figure 1. Rice Farming and Milling Establishments", gp = gpar(fontsize = 22, fontface = "bold", col = "#1f2933"))
figure_subtitle <- textGrob("Arkansas has a dense network of industrial milling hubs, while Carolina heirloom rice production operates with fewer local processing facilities.", gp = gpar(fontsize = 11.5, col = "#52606d"))
figure_note <- textGrob("*Extension-identified locations provide context for activity that may not be visible in the 2024 Local Food Data Warehouse.", x = 0.01, hjust = 0, gp = gpar(fontsize = 8.5, fontface = "italic", col = "#000000"))

figure1_final <- arrangeGrob(figure_title, figure_subtitle, maps_with_border, figure_legend, figure_note, ncol = 1, heights = c(0.24, 0.16, 4.80, 0.68, 0.18), padding = unit(0.15, "line"))

grid.newpage()
grid.draw(figure1_final)

# Export high-resolution PNG and PDF files
ggsave("Figure1_Rice_Production_and_Milling_Infrastructure.png", figure1_final, width = 16, height = 9, units = "in", dpi = 600, bg = "white", limitsize = FALSE)
ggsave("Figure1_Rice_Production_and_Milling_Infrastructure.pdf", figure1_final, width = 16, height = 9, units = "in", bg = "white", limitsize = FALSE)

cat("Figure 1 created successfully in:", getwd(), "\n")
