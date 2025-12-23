# JUne 6, 2025

library(adcp)
library(here)
library(ggview)
library(RColorBrewer)
library(rnaturalearth)
library(readr)
library(sf)
library(sensorstrings)
library(dplyr)
library(ggplot2)


# creates nice basemap of NS and surrounding land 
# (must have rnaturalearth and ggplot2 loaded)
source(here("functions/ns_base_map.R"))

theme_set(theme_light())

# detection -------------------------------------------------------

dat_raw <- read_csv(here("data/nsdfa_qualified_detections_2024.csv"))

dat <- dat_raw %>% 
  select(station, latitude, longitude, scientificname) %>% 
  mutate(
    scientificname = case_when(
      scientificname == "Scomber scombrus" ~ "Atlantic Mackerel",
      scientificname == "Prionace glauca" ~ "Blue Shark",
      scientificname == "Salmo salar" ~ "Atlantic Salmon",
      TRUE ~ "Unqualifed")
  ) %>% 
  group_by(station, scientificname) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct(station, scientificname, .keep_all = TRUE) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4617) 


# All Stations: Full Slide ---------------------------------------------------------------------
w = 32
h = 15

dat <- dat %>% 
  filter(scientificname == "Atlantic Mackerel")

ns_base_map() +
  geom_sf(
    data = dat, aes(size = n), pch = 21, alpha = 0.75, fill = "#23A8B8"
  ) +
  scale_size("Number of Detections", range = c(0, 20)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  canvas(width = w, height = h, units = "cm")

# full slide figure
ggsave(
  filename = here("figures/2025-06-06_otn_mackerel_detections.png"),
  device = "png",
  width = w, height = h, units = "cm",
  dpi = 600
)



# All Detections: Full Slide ---------------------------------------------------------------------

# ns_base_map() +
#   geom_sf(
#     data = dat, aes(fill = scientificname, size = n),
#     pch = 21, alpha = 1
#   ) +
#   scale_fill_manual("Species", values = c(brewer.pal(3, "Dark2"), "grey")) +
#   guides(fill = guide_legend(keyheight = 1.75)) +
#   theme(
#     legend.position.inside = c(0.9, 0.15),
#     legend.title = element_blank(),
#     legend.box.background = element_rect(color = "grey20"),
#     legend.text = element_text(size = 16)
#   ) + 
#   facet_wrap(~scientificname)





