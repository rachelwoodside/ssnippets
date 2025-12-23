# October 17, 2025

library(here)
library(RColorBrewer)
library(readxl)
library(rnaturalearth)
library(sf)
library(sensorstrings)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggview)
#library(ggspatial)

# creates nice basemap of NS and surrounding land 
# (must have rnaturalearth and ggplot2 loaded)
source(here("functions/ns_base_map.R"))

theme_set(theme_light())

# station locations -------------------------------------------------------

strings_raw <- read_excel(
  here("data/water_quality_deployment_tracking.xlsx"),
  sheet = "station_waterbody_county"
) 

st_locations <- strings_raw %>% 
  filter(status == "active") %>% 
  mutate(
    latitude_ddm = str_remove(latitude_ddm, " N"),
    longitude_ddm = str_remove(longitude_ddm, " W"),
    latitude = if_else(
      is.na(latitude), ss_coords_from_ddm_to_dd(latitude_ddm), latitude
    ),
    longitude = if_else(
      is.na(longitude), ss_coords_from_ddm_to_dd(longitude_ddm), longitude
    )
  ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4617) 

# All Stations: Word Doc ---------------------------------------------------------------------

w_out <- 20
h_out <- 15

ns_base_map(x_min = -67, x_max = -59, y_min = 43.3, y_max = 47.2) +
  geom_sf(
    data = st_locations, fill = "#1B9E77",
    pch = 21, size = 3.5, alpha = 1
  ) +
  canvas(width = w_out, height = h_out, units = "cm")

ggsave(
  filename = here("figures/2025-10-17_station_locations_word.png"),
  device = "png",
  width = w_out, height = h_out, units = "cm",
  dpi = 600
)

