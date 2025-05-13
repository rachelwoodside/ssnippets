# December 16, 2024

# multiple layers to leaflet map
## map_dat: list where each element is one layer to add
## map_pal: colour palette for layers. Can be a function
## popup: column of map_dat to use as popup or label

add_map_layers <- function(map_dat, map_pal = NULL, popup = NULL, size = NULL) {
  
  m <- leaflet(map_dat) %>%
    addProviderTiles(providers$CartoDB.Positron) 
  
  names(map_dat) %>%
    purrr::walk(function(x) {
      m <<- m %>%
        addCircleMarkers(
          data = map_dat[[x]],
          group = x,
          
          fillColor = map_pal,
          popup = popup,
          lng = ~longitude, lat = ~latitude,
          
          weight = 1,
          color = "black",
          fillOpacity = 0.75,
          radius = size,
        )
    }) 
  
  m
}
