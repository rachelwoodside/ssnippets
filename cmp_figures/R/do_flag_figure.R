library(dplyr)
library(ggplot2)
library(ggview)
library(here)
library(lubridate)
library(qaqcmar)

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


# 2024-06-14 -----------------------------------------------------------------

p1 <- p[[1]]$qc +
  scale_y_continuous("Dissolved Oxygen (% saturation)") +
  scale_x_datetime("") +
  guides(colour = guide_legend(keyheight = 1.75, override.aes = list(size = 4))) +
  theme(
    text = element_text(size = 18),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.7),
    legend.background = element_rect(colour = "grey40")
  )

p1

ggsave(
  p1,
  filename = here("figures/2024-06-14_do_biofouling.png"),
  device = "png",
  width = 32, height = 12, units = "cm",
  dpi = 600
)


ggsave(
  p1,
  filename = here("figures/2024-06-14_do_biofouling_small.png"),
  device = "png",
  width = 23, height = 13, units = "cm",
  dpi = 600
)
