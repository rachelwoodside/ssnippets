library(dplyr)
library(ggplot2)
library(ggview)
library(here)
library(lubridate)
library(qaqcmar)


width_out <- 32
height_out <- 12

theme_legend <- theme(
  text = element_text(size = 18),
  strip.text = element_text(size = 18),
  legend.position = "inside",
  legend.position.inside = c(0.83, 0.68),
  legend.background = element_rect(colour = "grey40"),
  legend.text = element_text(size = 18)
)


# read in data ------------------------------------------------------------

dat <- readRDS(here("data/shut_in_island.RDS")) %>% 
  filter(
    deployment_range == "2019-Dec-01 to 2020-Nov-08",
    sensor_depth_at_low_tide_m == 5
  )


p <- qc_plot_flags(
  dat, qc_tests = "qc",
  vars = "dissolved_oxygen_percent_saturation",
  flag_title = FALSE
) 

# general qc --------------------------------------------------------------

# all green points
p1 <- p[[1]]$qc +
  geom_point(col = "chartreuse4") +
  scale_y_continuous("Dissolved Oxygen (% saturation)") +
  scale_x_datetime("", date_labels = "%Y-%m-%d", date_minor_breaks = "1 month") +
  theme(
    text = element_text(size = 18),
    strip.text = element_text(size = 18),
    legend.position = "none"
  )

p1 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p1,
  filename = here("figures/2024-10-31_do_green.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)

# all green points + red background
p2 <- p$dissolved_oxygen_percent_saturation$qc +
  annotate(
    "rect",
      xmin = as_datetime(-Inf),
      xmax = as_datetime(Inf),
      ymin = 150, ymax = Inf,
    alpha = 0.5, fill = "#DB4325"
  ) +
  annotate(
    "rect",
    xmin = as_datetime("2020-02-15"),
    xmax = as_datetime("2020-05-05"),
    ymin = -Inf, ymax = Inf,
    alpha = 0.5, fill = "#EDA247"
  ) +
  geom_point(col = "chartreuse4") +
  scale_y_continuous("Dissolved Oxygen (% saturation)") +
  scale_x_datetime("", date_labels = "%Y-%m-%d", date_minor_breaks = "1 month") +
  theme(
    text = element_text(size = 18),
    strip.text = element_text(size = 18),
    legend.position = "none"
  )

p2 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p2,
  filename = here("figures/2024-10-31_do_green_red_orange_rect.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)


# qc flags
p3 <- p$dissolved_oxygen_percent_saturation$qc +
  scale_y_continuous("Dissolved Oxygen (% saturation)") +
  scale_x_datetime("", date_labels = "%Y-%m-%d", date_minor_breaks = "1 month") +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  legend_theme

p3 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p3,
  filename = here("figures/2024-10-31_do_biofouling.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)


# grossrange - do ---------------------------------------------------------

p <- qc_plot_flags(
  dat, 
  qc_tests = c("grossrange", "climatology", "spike", "rolling_sd"),
  vars = "dissolved_oxygen_percent_saturation",
  flag_title = FALSE
) 

p4 <- p$dissolved_oxygen_percent_saturation$grossrange +
  scale_y_continuous("Dissolved Oxygen (% saturation)") +
  scale_x_datetime("", date_labels = "%Y-%m-%d", date_minor_breaks = "1 month") +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme_legend

p4 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p4,
  filename = here("figures/2024-10-31_do_grossrange.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)

# rolling sd - do ----------------------------------------------------------
p5 <- p$dissolved_oxygen_percent_saturation$rolling_sd +
  scale_y_continuous("Dissolved Oxygen (% saturation)") +
  scale_x_datetime("", date_labels = "%Y-%m-%d", date_minor_breaks = "1 month") +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme_legend

p5 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p5,
  filename = here("figures/2024-10-31_do_rolling_sd.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)


# climatology - temp ------------------------------------------------------

dat2 <- readRDS(here("data/tilley_point_2022-11-03.rds")) %>% 
  filter(
    timestamp_utc >= as_datetime("2022-11-15"),
    sensor_serial_number == 20308050
  )

p_temp <- qc_plot_flags(dat2, qc_tests = "climatology", flag_title = FALSE)

p6 <- p_temp$temperature_degree_c$climatology +
  annotate(
    "rect",
    xmin = as_datetime("2023-02-01"),
    xmax = as_datetime("2023-03-01"),
    ymin = 4.29, ymax = Inf,
    alpha = 0.25, fill = "#EDA247"
  ) +
  annotate(
    "rect",
    xmin = as_datetime("2023-02-01"),
    xmax = as_datetime("2023-03-01"),
    ymin = -Inf, ymax = 4.29,
    alpha = 0.25, fill = "chartreuse4"
  ) +
  geom_point() +
  scale_y_continuous("Temperature (\u00B0C)") +
  scale_x_datetime(
    "", 
    date_labels = "%Y-%m-%d", 
    breaks = "1 month",
    date_minor_breaks = "1 month"
  ) +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme_legend

p6 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p6,
  filename = here("figures/2024-10-31_temp_climatology.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)

# human in loop - temp ----------------------------------------------------

dat3 <- readRDS(here("data/white_island_2018-10-26.rds")) %>% 
  filter(
    timestamp_utc > as_datetime("2019-01-15"),
    timestamp_utc < as_datetime("2020-01-15"),
    sensor_serial_number == 10755218
  )

p_hil <- qc_plot_flags(
  dat3, 
  qc_tests = c("grossrange", 
               "climatology", 
               "spike", 
               "rolling_sd",
               "human_in_loop"),
  flag_title = FALSE)

p7 <- p_hil$temperature_degree_c$human_in_loop +
  geom_point() +
  scale_y_continuous("Temperature (\u00B0C)") +
  scale_x_datetime("", date_labels = "%Y-%m-%d", date_minor_breaks = "1 month") +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme_legend

p7 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p7,
  filename = here("figures/2024-10-31_temp_hil.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)

p8 <- p_hil$temperature_degree_c$human_in_loop +
  geom_point(col = "chartreuse4") +
  scale_y_continuous("Temperature (\u00B0C)") +
  scale_x_datetime("", date_labels = "%Y-%m-%d", date_minor_breaks = "1 month") +
  theme_legend +
  theme(legend.position = "none")

p8 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p8,
  filename = here("figures/2024-10-31_temp_hil_green.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)


# spike - temperature -----------------------------------------------------

dat4 <- readRDS(here("data/wine_harbour_2022-11-04.rds")) %>% 
  filter(
    timestamp_utc > as_datetime("2023-04-07"),
    timestamp_utc < as_datetime("2023-04-08 13:00:00"),
    sensor_serial_number == 20687285
   # sensor_serial_number == 10817409
  )

p_spike <- qc_plot_flags(dat4, qc_tests = "spike", flag_title = FALSE)

p9 <- p_spike$temperature_degree_c$spike +
  geom_point(size = 2) +
  scale_y_continuous("Temperature (\u00B0C)") +
  scale_x_datetime(
    "", 
    date_labels = "%Y-%m-%d %H:%M", 
    date_minor_breaks = "2 hour"
    ) +
  guides(
    colour = guide_legend(keyheight = 2, override.aes = list(size = 4))
  ) +
  theme_legend

p9 +
  canvas(width_out, height_out, units = "cm")

ggsave(
  p9,
  filename = here("figures/2024-10-31_temp_spike.png"),
  device = "png",
  width = width_out, height = height_out, units = "cm",
  dpi = 600
)

