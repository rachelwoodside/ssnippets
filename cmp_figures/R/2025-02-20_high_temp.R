library(dplyr)
library(here)
library(ggplot2)
library(htmlwidgets)
library(leaflet)
library(lubridate)
library(qaqcmar)
library(readr)
library(sensorstrings)
library(summaryplots)

source(here("functions/add_map_layers.R"))

theme_set(theme_light())

# import data -------------------------------------------------------------

dat_raw <- ss_import_data()

dat <- dat_raw %>% 
  filter(!is.na(temperature_degree_c), qc_flag_temperature_degree_c < 4) %>% 
  select(county, station, timestamp_utc, 
         sensor_depth_at_low_tide_m, sensor_type, sensor_serial_number, 
         temperature_degree_c)

rm(dat_raw)
gc()


# filter data -------------------------------------------------------------

dat_temp <- dat %>% 
  filter(temperature_degree_c >= 25)

dat_summary <- dat_temp %>% 
  mutate(year_utc = year(timestamp_utc)) %>% 
  group_by(county, station, sensor_depth_at_low_tide_m) %>% 
  summarise(n = n())


# map ---------------------------------------------------------------------
st_locations <- read_csv(
  here("data/2024-12-06_station_locations.csv"), show_col_types = FALSE
)

#county_pal <- get_county_colour_palette(length(unique(st_locations$county)))

st_locations <- st_locations %>% 
  inner_join(dat_summary, join_by(county, station)) %>% 
  mutate(
    popup = paste(county, station, n, sep = "</br>")
  ) %>% 
  ss_convert_depth_to_ordered_factor()

depth_pal <- colorFactor(
  palette = viridis(10, direction = -1),
  domain = c("0", "0.5", "0.8", "1", "1.5", "1.6", "2", "3", "3.5", "5.5")
)

st_depth <- split(st_locations, st_locations$sensor_depth_at_low_tide_m)

m <- add_map_layers(
  st_depth, 
  ~depth_pal(sensor_depth_at_low_tide_m),
  popup = ~popup, size = ~0.01*n
)  %>%
  addLegend(
    "bottomright", pal = depth_pal, 
    values = c("0", "0.5", "0.8", "1", "1.5", "1.6", "2", "3", "3.5", "5.5"),
    title = "Sensor Depth",
    opacity = 0.75
  ) %>% 
  addScaleBar(
    position = "bottomleft", 
    options = scaleBarOptions(imperial = FALSE)
  )

saveWidget(m, here("figures/mussel_heat_stress_temps.html"))


# bar plot ----------------------------------------------------------------

ggplot(st_locations, aes(n, station, fill = sensor_depth_at_low_tide_m)) +
  geom_col() +
  scale_fill_manual("Depth (m)", values = viridis(10, direction = -1)) +
  scale_y_discrete(name = "", limits = rev) +
  scale_x_continuous("Number of Observations >= 25 C")

ggsave(
  here("figures/mussel_heat_stress_barplot.png"), 
  device = "png",width = 15, height = 20, units = "cm")
