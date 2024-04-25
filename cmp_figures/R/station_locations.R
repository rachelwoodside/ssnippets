# April 25, 2024

library(adcp)
library(here)
library(RColorBrewer)
library(rnaturalearth)
library(readr)
library(sf)
library(sensorstrings)
library(dplyr)
library(ggplot2)
#library(ggspatial)

# creates nuce basemap of NS and surrounding land 
# (must have rnaturalearth and ggplot2 loaded)
source(here("functions/ns_base_map.R"))

theme_set(theme_light())

# station locations -------------------------------------------------------

strings_raw <- read_csv(
  here("data/2024-03-14_station_locations.csv"),
  show_col_types = FALSE) %>%
  mutate(variable = "Water Quality"
  ) %>%
  select(station, variable, latitude, longitude)

adcp_raw <- adcp_read_nsdfa_metadata(
  here("data/2023-11-27 - NSDFA Tracking Sheet.xlsx")
) %>%
  mutate(variable = if_else(
    is.na(Waves_Ensemble_Interval_s), "Current", "Current & Wave"
  )) %>%
  select(station = Station_Name, variable, latitude = Depl_Lat, longitude = Depl_Lon) %>%
  na.omit()

stations <- rbind(strings_raw, adcp_raw) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4617) %>% 
  mutate(
    variable = ordered(
      variable, levels = c("Water Quality", "Current & Wave", "Current"))
  )

# All Stations: Full Slide ---------------------------------------------------------------------

ns_base_map() +
  geom_sf(
    data = stations, aes(fill = variable),
    pch = 21, size = 3.5, alpha = 1
  ) +
  scale_fill_manual("", values = brewer.pal(3, "Dark2")) +
  guides(fill = guide_legend(keyheight = 1.75)) +
  theme(
    legend.position = c(0.9, 0.15),
    legend.title = element_blank(),
    legend.box.background = element_rect(color = "grey20"),
    legend.text = element_text(size = 16)
  )

# full slide figure
ggsave(
  filename = here("figures/2024-04-09_station_locations_wide.png"),
  device = "png",
  width = 32, height = 15, units = "cm",
  dpi = 600
)


# All Stations: Word Doc --------------------------------------------------

ns_base_map(x_min = -67, x_max = -59, y_min = 43.3, y_max = 47.2) +
  geom_sf(
    data = stations, aes(fill = variable),
    pch = 21, size = 3.5, alpha = 1
  ) +
  scale_fill_manual("", values = brewer.pal(3, "Dark2")) +
  guides(fill = guide_legend(keyheight = 1.75)) +
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_blank(),
    legend.box.background = element_rect(color = "grey20"),
    legend.text = element_text(size = 12)
  )

ggsave(
  filename = here("figures/2024-04-25_station_locations_word.png"),
  device = "png",
  width = 20, height = 15, units = "cm",
  dpi = 600
)

# All Stations: Word Doc --------------------------------------------------

ns_base_map(
  x_min = -67, x_max = -59, y_min = 43.3, y_max = 47.2,
  linewidth = 0.25
  ) +
  geom_sf(
    data = stations, aes(fill = variable),
    pch = 21, size = 2, alpha = 1
  ) +
  scale_fill_manual("", values = brewer.pal(3, "Dark2")) +
 # guides(fill = guide_legend(keyheight = 0.75)) +
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_blank(),
    legend.box.background = element_rect(color = NA),
    legend.text = element_text(size = 10)
  )

ggsave(
  filename = here("figures/2024-04-25_station_locations_word_small.png"),
  device = "png",
  width = 11, height = 9, units = "cm",
  dpi = 600
)


