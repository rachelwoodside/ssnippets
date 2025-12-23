# June 3, 2022

library(cowplot)       # to inset map figure
library(dplyr)         # data manipulation 
library(ggplot2)       # figures
library(ggpubr)        # to arrange figures
library(ggspatial)     # for north arrow and scale bar
library(glue)          # used in map figure
library(here)          # relative file paths
library(lubridate)     # dates
library(patchwork)
library(RColorBrewer)  # for TGC model figure
library(rnaturalearth) # for map of North America
library(rnaturalearthhires) # for map of NS
library(readr)         # export table
library(sf)            # static map 
library(strings)       # convert_depth_to_ordered_factor() function
library(tgc)           # season functions
library(tidyr)         # to format the sensitivity results
library(viridis)       # colour palette

# max size
#width = 17, height = 22.5, units = "cm"


# Import results ----------------------------------------------------------
results <- readRDS(here("results/model_results.rds"))

dat_raw <- results$dat_raw
dat_seasons <- results$dat_seasons
dat_filt <- results$dat_filt
gap_table <- results$gap_table
dd <- results$dd
tgc_table <- results$tgc_table


# Plot params -------------------------------------------------------------
theme_set(theme_light())
colour_pal <- viridis(5, direction = -1)


# Figure 4 ----------------------------------------------------------------
ylims <- c(-1, 21)
text_size <- 14
title_text_size <- 14

# Beaver Point
beaver <- filter(dat_seasons, STATION == "Beaver Point") 

beaver_filt <- filter_out_heat_stress_events(beaver)

p4_B <- beaver %>% 
  select(-SEASON) %>% 
  plot_filtered_data(
    beaver_filt,
    legend_position = "right",
    colour_palette = colour_pal,
    ylims = ylims,
    date_axis_name = NULL,
    date_breaks_major = "2 month",
    date_labels_format = "%Y-%m-%d"
  ) +
  theme(text = element_text(size = 11))

p4_B

ggsave(
  p4_B,
  filename = "C:/Users/Danielle Dempsey/Desktop/RProjects/ssnippets/cmp_figures/figures/2025-07_21_beaver_point_tgc.png",
  device = "png",
  width = 20, height = 8, units = "cm",
  dpi = 600
)


# Figure 6 ----------------------------------------------------------------
point_size <- 4

p6_B  <- tgc_table %>% 
  filter(STATION == "Beaver Point") %>% 
  mutate(
    TGC = case_when(
      TGC == 0.25 ~ "Remedial\n(0.25)",
      TGC == 0.30 ~ "Average\n(0.30)",
      TGC == 0.35 ~ "Elite\n(0.35)"
    ),
    TGC = ordered(
       TGC, levels = c("Remedial\n(0.25)", "Average\n(0.30)", "Elite\n(0.35)" )
     )
  ) %>% 
ggplot(
   aes(x = TGC, y = TGC_INITIAL_WEIGHT, fill = DEPTH)
) +
  geom_point(pch = 21, size = point_size, alpha = 0.75) +
  scale_fill_manual("Depth (m)", values = colour_pal, drop = TRUE) +
  scale_x_discrete("TGC Value") +
  scale_y_continuous("Stocking Weight (kg)") +
  theme(
    strip.background = element_rect(fill = "white", colour = "darkgrey"),
    strip.text = element_text(colour = "grey30", size = 6)
  )



ggsave(
  filename = "2025-07-21_tgc_results.png",
  path = "C:/Users/Danielle Dempsey/Desktop/RProjects/ssnippets/cmp_figures/figures",,
  device = "png",
  width = 10, height = 8, units = "cm",
  dpi = 600
)


