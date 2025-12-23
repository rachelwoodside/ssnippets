library(dplyr)
library(ggplot2)
library(ggview)
library(here)
library(lubridate)
library(purrr)
library(readr)
library(sensorstrings)
library(tidyr)

theme_set(theme_light())
w_out <- 32
h_out <- 13

# moose point -----------------------------------------------------

files <- list.files(
  "R:/data_branches/water_quality/processed_data/qc_data/guysborough",
  pattern = "moose_point",
  full.names = TRUE
)

moose_raw <- map_df(files, readRDS)

moose <- moose_raw %>% 
  select(
    station, 
    timestamp_utc, 
    sensor_depth_at_low_tide_m, 
    temperature_degree_c,
    qc_flag_temperature_degree_c
  ) %>% 
  filter(
    sensor_depth_at_low_tide_m == 10, 
    qc_flag_temperature_degree_c < 4,
    timestamp_utc >= as_datetime("2018-06-17")
    ) %>% 
  mutate(
    season = if_else(timestamp_utc < as_datetime("2019-02-19 21:30:00"), TRUE, FALSE)
  )
  
ggplot(moose, aes(timestamp_utc, temperature_degree_c, col = season)) + 
  # annotate("rect",
  #          xmin = as_datetime(-Inf), xmax = as_datetime (Inf),
  #          ymin = 18, ymax = Inf,
  #          fill = "#FB9A99") +
  annotate("rect",
           xmin = as_datetime(-Inf), xmax = as_datetime (Inf),
           ymin = -Inf, ymax = -0.7,
           fill = "#A6CEE3") +
  geom_line(col = "#1B9E77") +
 # scale_color_manual(values = c("TRUE" = "#1B9E77", "FALSE" = "grey40")) +
  scale_y_continuous("Temperature (\u00B0C)", limits = c(-2.5, 21)) +
  scale_x_datetime(
    "Date", 
    limits = c(min(moose$timestamp_utc), min(moose$timestamp_utc) + days(540)),
    date_labels = "%Y-%m-%d", date_breaks = "3 month"
  ) +
  theme(
    text = element_text(size = 20),
    axis.ticks.length=unit(.25, "cm"),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black")
  ) +
  canvas(width = w_out, height = h_out, units = "cm", dpi = 600)


ggsave(
  filename = here("figures/2025-06-03_moose_point_2.png"),
  device = "png",
  width = w_out, height = h_out, units = "cm",
  dpi = 600
)





# shut-in -----------------------------------------------------------------

files <- list.files(
  "R:/data_branches/water_quality/processed_data/qc_data/halifax",
  pattern = "shut-in",
  full.names = TRUE
)

shut_raw <- map_df(files, readRDS)

shut <- shut_raw %>% 
  select(
    station, 
    timestamp_utc, 
    sensor_depth_at_low_tide_m, 
    temperature_degree_c,
    qc_flag_temperature_degree_c
  ) %>% 
  filter(sensor_depth_at_low_tide_m == 5, qc_flag_temperature_degree_c < 4) %>% 
  mutate(
    year_utc = year(timestamp_utc),
    month_utc = month(timestamp_utc),
    day_utc = day(timestamp_utc),
    hour_utc = hour(timestamp_utc),
    min_utc = minute(timestamp_utc),
    sec_utc = second(timestamp_utc),
    year_plot = 2025
  ) %>% 
  filter(
    month_utc %in% c(1, 2, 3),
    year_utc < 2024, year_utc > 2018
  ) %>% 
  mutate(timestamp_plot = make_datetime(
    year_plot, month_utc, day_utc, hour_utc, min_utc, sec_utc),
    year_utc = factor(year_utc)
  )


w_out <- 33
h_out <- 14.5


ggplot(shut, aes(timestamp_plot, temperature_degree_c, col = year_utc)) + 
  annotate("rect",
           xmin = as_datetime(-Inf), xmax = as_datetime (Inf),
           ymin = -Inf, ymax = -0.7,
           fill = "#A6CEE3") +
  geom_line(size = 1) +
  scale_y_continuous("Temperature (\u00B0C)", limits = c(-2, 6)) +
  scale_x_datetime("Date") +
  scale_color_brewer("Year", palette = "Dark2") +
  guides(
    colour = guide_legend(keyheight = 2.5, override.aes = list(size = 2))
  ) +
  theme(
    text = element_text(size = 20),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black"),
    legend.title = element_text(hjust = 0.5),
    # legend.position = "none"
  ) +
  canvas(width = w_out, height = h_out, units = "cm", dpi = 600)

ggsave(
  filename = here("figures/2025-01-27_shut-in_legend.png"),
  device = "png",
  width = w_out, height = h_out, units = "cm",
  dpi = 600
)
