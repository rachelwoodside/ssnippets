library(adcp)
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(patchwork)
library(qaqcmar)
library(sensorstrings)
library(viridis)
library(wesanderson)


# water quality -----------------------------------------------------------

dat <- readRDS(here("data/shut_in_island.RDS"))

dat_temp <- dat %>% 
  filter(qc_flag_temperature_degree_c != 4) %>% 
  select(timestamp_utc, temperature_degree_c,
         sensor_depth_at_low_tide_m, sensor_serial_number, sensor_type)

p1 <- ss_plot_variables(dat_temp)

ggsave(
  p1,
  filename = here("figures/2024-04-25_shut-in_island_temperature.png"),
  device = "png",
  width = 20, height = 8, units = "cm",
  dpi = 600
)


# dat_all <- dat %>% 
#   qc_pivot_longer(qc_tests = "qc") %>% 
#   qc_filter_summary_flags()
# 
# ss_plot_variables(dat_all)  


# current -----------------------------------------------------------------

current <- readRDS(here("data/halifax_current_2023-06-02.RDS")) %>% 
  filter(station == "Shut-In Island") %>% 
  mutate(sea_water_speed_cm_s = round(sea_water_speed_m_s * 100, digits = 2)) %>% 
  select(-sea_water_speed_m_s) %>% 
  group_by(bin_height_above_sea_floor_m) %>% 
  summarise(
    n_obs = n(),
    average_speed_cm_s = mean(sea_water_speed_cm_s),
    average_speed_cm_s = round(average_speed_cm_s, digits = 2),
    average_direction_degree = mean(sea_water_to_direction_degree)
  ) %>% 
  ungroup() %>% 
  filter(n_obs == 12920)

p2 <- ggplot(current, aes(average_speed_cm_s, bin_height_above_sea_floor_m)) +
  geom_point(col = "grey30") +
  scale_x_continuous(name = "Average Sea Water Speed (cm / s)") +
  scale_y_continuous("Height Above Sea Floor (m)") +
 # scale_colour_gradient(low = "#F7C9AAFF", high =  "#921C5BFF") +
  theme(
    text = element_text(size = 11),
    legend.position = "none"
  ) 

ggsave(
  p2,
  filename = here("figures/2024-04-25_shut-in_island_current_speed_bw.png"),
  device = "png",
  width = 9, height = 8, units = "cm",
  dpi = 600
)

# Waves -------------------------------------------------------------------

wave_raw <- read_csv(here("data/20210728_Halifax_County_Wave_Data.csv"), 
                    show_col_types = FALSE)

wave <- wave_raw %>% 
  filter(Waterbody == "St. Margarets Bay") %>% 
  select(DateTime, Hs, Tp) %>% 
  mutate(Date = as_datetime(paste0(DateTime, ":00"))) 

p3 <- ggplot(wave, aes(Date, Hs, color = NULL)) +
  geom_bar(
    stat = "identity", position ="identity", size = 0.5, fill = "grey30"
  ) +
  scale_x_datetime("Date", date_labels = "%Y-%m-%d") +
  scale_y_continuous(
    "Wave Height (m)", 
    limits = c(0, 4), 
    breaks = seq(0, 4, 1)
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 11),
    #  panel.border = element_rect(color = "black", fill = NA)
  )

ggsave(
  p3,
  filename = here("figures/shut-in_island_waves_bw.png"),
  device = "png",
  width = 9, height = 8, units = "cm",
  dpi = 600
)


# combine -----------------------------------------------------------------

# p1 / (p2 + p3)





