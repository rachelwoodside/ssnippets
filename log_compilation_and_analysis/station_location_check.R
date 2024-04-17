# April 17, 2024

# reads in the stacked logs and applies station location checks

library(dplyr)
library(googlesheets4)
library(here)
library(purrr)
library(sensorstrings)
library(sf)


not_in_area_info <- c(
  "Broad 1 Dirt Road 1",
  "Medway 1 Bangs Falls 1",
  "Medway 2 Big Cove 1",
  "Medway 3 Greenfield 1",
  "Mersey 1 River Road Offshoot 1",
  "Mersey 2 River Road 2",
  "Mersey 4 Fish Hatchery Road 1"
  )

dat_all <- readRDS(
  here("log_compilation_and_analysis/stacked_logs_2024-04-15.rds")
)

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
  distinct() %>%
  filter(!(station %in% not_in_area_info))

# link to the "STRING TRACKING" google sheet
googlesheets4::gs4_deauth()

link <- "http://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"

# read in the "Area Info" tab of the STRING TRACKING sheet
station_coords <- googlesheets4::read_sheet(link, sheet = "Area Info") %>%
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

  if(i %% 20 == 0) print(i)
}

location_check <- location_check %>%
  list_rbind()


saveRDS(
  location_check,
  here("log_compilation_and_analysis/station_location_checks.rds")
)



