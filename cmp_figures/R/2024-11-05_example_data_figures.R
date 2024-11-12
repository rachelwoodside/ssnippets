library(adcp)
library(dplyr)
library(ggplot2)
library(ggview)
library(here)
library(lubridate)
library(qaqcmar)
library(sensorstrings)
library(viridis)
library(waves)


# water quality -----------------------------------------------------------
w <- 32
h <- 12

dat <- readRDS(here("data/shut_in_island.RDS"))

dat_temp <- dat %>% 
  filter(qc_flag_temperature_degree_c != 4) %>% 
  select(timestamp_utc, temperature_degree_c,
         sensor_depth_at_low_tide_m, sensor_serial_number, sensor_type)

p1 <- ss_ggplot_variables(dat_temp) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous("Temperature (\u00B0C)") +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme(
    strip.text = element_blank(),
    text = element_text(size = 18)
  )

p1 + canvas(width = w, height = h, units = "cm")

ggsave(
  p1,
  filename = here("figures/2024-11-05_shut-in_island_temp.png"),
  device = "png",
  width = w, height = h, units = "cm",
  dpi = 600
)

###########################
p1_thresh <- ss_ggplot_variables(
  dat_temp,
  superchill = TRUE
  ) +
  annotate(
    "rect",
    xmin = as_datetime(-Inf), xmax = as_datetime(Inf),
    ymin = 18,  ymax = Inf,
    fill = "#FB9A99", alpha = 1
  ) +
  geom_point(size = 0.25) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous("Temperature (\u00B0C)") +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme(
    strip.text = element_blank(),
    text = element_text(size = 18)
  )

p1_thresh + canvas(width = w, height = h, units = "cm")

ggsave(
  p1_thresh,
  filename = here("figures/2024-11-05_shut-in_island_temp_thresh.png"),
  device = "png",
  width = w, height = h, units = "cm",
  dpi = 600
)

##############################
p1_2022 <- dat_temp %>% 
  filter(
    timestamp_utc >= as_datetime("2022-01-01"),
    timestamp_utc < as_datetime("2023-01-01")
    ) %>% 
  ss_ggplot_variables() +
  annotate(
    "rect",
    xmin = as_datetime("2022-09-23 20:00:00"),
    xmax = as_datetime("2022-09-25"),
    ymin = -Inf, ymax = Inf,
    fill =  "#EDA247"
  ) +
  geom_point(size = 0.25) +
  scale_x_datetime(
    date_breaks = "2 month",
    date_minor_breaks = "1 month", 
    date_labels = "%Y-%m-%d"
    ) +
  scale_y_continuous("Temperature (\u00B0C)") +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme(
    strip.text = element_blank(),
    text = element_text(size = 18)
  )

p1_2022 + canvas(width = w, height = h, units = "cm")


ggsave(
  p1_2022,
  filename = here("figures/2024-11-05_shut-in_island_temp_2022_fiona.png"),
  device = "png",
  width = w, height = h, units = "cm",
  dpi = 600
)

ggsave(
  p1_2022,
  filename = here("figures/2024-11-05_shut-in_island_temp_2022.png"),
  device = "png",
  width = w, height = h, units = "cm",
  dpi = 600
)



# current -----------------------------------------------------------------

current <- readRDS(here("data/halifax_current_2023-06-02.RDS")) %>% 
  filter(station == "Shut-In Island") %>% 
  mutate(sea_water_speed_cm_s = round(sea_water_speed_m_s * 100, digits = 2)) %>% 
  select(-sea_water_speed_m_s) #

current_summary <- current %>% 
  group_by(bin_height_above_sea_floor_m) %>% 
  summarise(
    n_obs = n(),
    average_speed_cm_s = mean(sea_water_speed_cm_s),
    average_speed_cm_s = round(average_speed_cm_s, digits = 2),
    average_direction_degree = mean(sea_water_to_direction_degree)
  ) %>% 
  ungroup() %>% 
  filter(n_obs == 12920)

p2 <- ggplot(
  current_summary, 
  aes(average_speed_cm_s, bin_height_above_sea_floor_m, 
      col = average_speed_cm_s)
  ) +
  geom_point(size = 2) +
  scale_x_continuous(name = "Average Sea Water Speed (cm / s)") +
  scale_y_continuous("Height Above Sea Floor (m)") +
 # scale_color_viridis(option = "F", direction = -1) +
  scale_colour_gradient(low = "#F7C9AAFF", high =  "#921C5BFF") +
  theme(
    text = element_text(size = 18),
    legend.position = "none"
  ) 

p2 + canvas(width = w, height = h, units = "cm")

ggsave(
  p2,
  filename = here("figures/2024-11-05_shut-in_island_current_speed.png"),
  device = "png",
  width = w, height = h, units = "cm",
  dpi = 600
)


p <- current %>% 
  filter(bin_height_above_sea_floor_m == 37.58) %>% 
  adcp_plot_current_speed_time()

p + canvas(width = w, height = h, units = "cm")

current %>% 
  filter(bin_height_above_sea_floor_m == 5.58) %>% 
ggplot( aes(timestamp_utc, sea_water_speed_cm_s)) +
  geom_line() +
  scale_y_continuous("Current Speed (cm / s)") +
  scale_x_datetime("Date")# +
  labs(
    title = unique(dat$station),
    caption = paste0(
      unique(dat$bin_height_above_sea_floor_m), " m above sea floor",
      "\n",
      "(approx ", round(mean(unique(dat$sensor_depth_below_surface_m)), digits = 1),
      " m below the surface)"
    )
  ) +
  theme_light()

# Waves -------------------------------------------------------------------
w <- 32
h <- 12

wave_raw <- readRDS(here("data/2020-07-03_Shut-In_Island_HL008.rds")) %>% 
  wv_assign_short_variable_names()

p3 <- wv_plot_ts(wave_raw, vars = "significant_height_m") +
  theme(
    legend.position = "none",
    text = element_text(size = 18),
    strip.text = element_blank()
  )

p3 + canvas(width = w, height = h, units = "cm")


ggsave(
  p3,
  filename = here("figures/2024-11-05_shut-in_island_waves.png"),
  device = "png",
  width = w, height = h, units = "cm",
  dpi = 600
)


# combine -----------------------------------------------------------------

# p1 / (p2 + p3)





