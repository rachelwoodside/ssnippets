# October 6, 2023

# library(rnaturalearth)
# library(ggplot2)

ns_base_map <- function(
    x_min = -69, x_max = -57, y_min = 43.5, y_max = 47,
    linewidth = 0.5
    ) {

  theme_set(theme_light())
  
  # north america - large scale for main map
  can <- rnaturalearth::ne_countries(
    continent = "north america", returnclass = "sf", scale = "large"
  )

  # crop to ns and map
  ggplot() +
    geom_sf(data = can, size = 1, linewidth = linewidth) +
    scale_x_continuous(limits =  c(x_min, x_max)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),
      panel.grid = element_blank()
    )
  
}
