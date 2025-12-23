# DATE: 2024-10-08
# NAME:
# sensorstrings VERSION: 1.0.3
# NOTES:

# Interim compile template until database is finalized

# Template for compiling & formatting data extracted from a sensor string deployment
# Exports compiled data as an rds file in the specified folder

# Must be connected to Perennia vpn

# SECTION 1: Generate File Path
# Update station and depl_date with the station name and deployment date of interest

# SECTION 2: Compile & View

# SECTION 3: Check station location Check station coordinates are within 500 m
# of official coordinates and are in the ocean.

# SECTION 4: Identify Trim Dates
# use the trimdates app to zoom in on the start and end of the deployment
# record the timestamp of the first and last observations

# SECTION 5: Trim Data
# if all sensors start recording good data around the same time, use this code

# SECTION 6: Export Data
# export compiled data as an rds file in the county/new folder


library(dplyr)
library(lubridate)
library(sensorstrings)

# SECTION 1: **Generate File Path** -------------------------------------------------------------

station <- "birchy head"
depl_date <- "2023-11-10"

path <- ss_import_path(station, depl_date)

# SECTION 2: Compile & View -----------------------------------------------------------------

# create log from metadata
ss_create_log_from_metadata(
  station = station,
  deployment_date = depl_date,
  path_export = path
)

dat_raw <- ss_compile_deployment_data(path)

ss_ggplot_variables(dat_raw)


# SECTION 3: Check Station Location ---------------------------------------

st_location <- data.frame(
  station = tools::toTitleCase(station),
  latitude = unique(dat_raw$latitude),
  longitude = unique(dat_raw$longitude)
)

ss_leaflet_station_map(st_location)

ss_check_station_radius(st_location)
ss_check_station_in_ocean(st_location) # slow because reads in shape file
# ss_check_station_drift(st_location) # need retrieval coords


# SECTION 4: Identify Trim Dates --------------------------------------------------------------
# if the app viewer is blank, click "Open in Browser"
# close the app between deployment start and deployment end

# deployment start
ss_open_trimdates_app(dat_raw, filter_to = "start", period = "2 days")

# enter timestamp for first GOOD observation:
depl_start <- as_datetime("2023-11-10 15:00:00")

# deployment end
ss_open_trimdates_app(dat_raw, filter_to = "end", period = "2 days")

# enter timestamp for last GOOD observation:
depl_end <- as_datetime('2024-05-02 15:34:33')


# SECTION 5: Trim Data --------------------------------------------------------------

dat <- dat_raw %>%
  filter(timestamp_utc >= depl_start, timestamp_utc <= depl_end)

# review and adjust if necessary
ss_open_trimdates_app(dat, filter_to = "start", period = "2 days")
ss_open_trimdates_app(dat, filter_to = "end", period = "2 days")

ss_ggplot_variables(dat)


# SECTION 6: Export Data ------------------------------------------------------------------

export_path <- ss_export_path(dat, ext = "rds")

saveRDS(dat, export_path)

