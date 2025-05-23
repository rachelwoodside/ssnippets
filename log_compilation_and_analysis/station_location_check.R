# April 17, 2024

# reads in the stacked logs and applies station location checks

library(dplyr)
library(googlesheets4)
library(here)
library(purrr)
library(sensorstrings)
library(sf)


# not_in_area_info <- c(
#   "Broad 1 Dirt Road 1",
#   "Medway 1 Bangs Falls 1",
#   "Medway 2 Big Cove 1",
#   "Medway 3 Greenfield 1",
#   "Mersey 1 River Road Offshoot 1",
#   "Mersey 2 River Road 2",
#   "Mersey 4 Fish Hatchery Road 1"
#   )

dat_all <- readRDS(here("stacked_logs_2024-04-17.rds"))

dat <- dat_all %>%
  select(
    station = location_description,
    deployment_date = deployment,
    retrieval_date = retrieval,
    latitude = logger_latitude,
    longitude = logger_longitude,
    retrieval_latitude,
    retrieval_longitude
  ) %>%
  distinct()# %>%
  #filter(!(station %in% not_in_area_info))

# link to the "STRING TRACKING" google sheet
googlesheets4::gs4_deauth()

link <- "http://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"

# read in the "Area Info" tab of the STRING TRACKING sheet
station_coords <- googlesheets4::read_sheet(link, sheet = "Area Info2") %>%
  select(station, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude))

# NS shapefile
ns <- read_sf(
  "R:/data_branches/water_quality/processing_resources/ns_coast/ns_coast.shp") %>%
  na.omit()


# Apply Checks ------------------------------------------------------------

location_check <- list()

for(i in seq_along(1:nrow(dat))) {

  log_i <- dat[i, ]
  coords_i <- filter(station_coords, station == log_i$station)

  if(nrow(coords_i)  > 0) {
    rad_check <- ss_check_station_radius(log_i, station_coords = coords_i)
  } else rad_check <- NA

  if(!is.na(log_i$retrieval_latitude) & !is.na(log_i$retrieval_longitude)) {
    drift_check <- ss_check_station_drift(log_i)
  } else  drift_check <- NA

  # this check is slow
  ocean_check <- ss_check_station_in_ocean(log_i, coast_shp = ns)

  location_check[[i]] <- data.frame(log_i, rad_check, drift_check, ocean_check)
  #location_check[[i]] <- data.frame(log_i, rad_check, drift_check)

  if(i %% 20 == 0) print(i)
}

location_check <- location_check %>%
  list_rbind()


saveRDS(
  location_check,
  here("2024-04-17_station_location_check.rds")
)


# drift analysis  ----------------------------------------------------------------
library(ggplot2)

drift_dist <- list()

for(i in seq_along(1:nrow(dat))) {
  
  log_i <- dat[i, ]
  
  if(!is.na(log_i$retrieval_latitude) & !is.na(log_i$retrieval_longitude)) {
    drift_check <- ss_check_station_drift(log_i, return_drift = TRUE)
  } else  drift_check <- NA
  
  drift_dist[[i]] <- data.frame(log_i, drift_check)
  
  if(i %% 20 == 0) print(i)
}

drift_dist <- drift_dist %>%
  list_rbind()


x <- drift_dist %>% 
  filter(!is.na(drift_check)) %>% 
  summarise(
    min = min(drift_check),
    max = max(drift_check),
    mean = mean(drift_check),
    med = median(drift_check)
  )


ggplot(drift_dist, aes(drift_check)) +
  geom_histogram()



# Look into ---------------------------------------------------------------

dat <- readRDS(here("2024-04-17_station_location_check.rds"))

st_checks <- dat %>% 
  filter(
    rad_check == FALSE |
      drift_check == FALSE |
      ocean_check == FALSE
  )


rad_check <- dat %>% filter(rad_check == FALSE)
drift_check <- dat %>% filter(drift_check == FALSE)
ocean_check <- dat %>% filter(ocean_check == FALSE)




