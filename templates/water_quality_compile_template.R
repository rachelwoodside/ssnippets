# DATE: 2023-09-07
# NAME: NLT
# sensorstrings VERSION: 0.4.6
# NOTES:

# Template for compiling & formatting data extracted from a sensor string deployment
# Exports compiled data as an rds file in the specified folder

# Must be connected to Perennia vpn

# SECTION 1: Generate File Path
# Update station and depl_date with the station name and deployment date of interest

# SECTION 2: Compile & View

# SECTION 3: Identify Trim Dates
# use the trimdates app to zoom in on the start and end of the deployment
# record the timestamp of the first and last observations

# SECTION 4: Trim Data
# if all sensors start recording good data around the same time, use this code
# if there is a large discrepancy, see APPENDIX

# SECTION 5: Export Data
# export compiled data as an rds file in the county/new folder

# APPENDIX
# use this code if there is a large discrepancy between sensor start or end times
# can copy/paste timestamps from the trimdates app BUT take care that they
# are in the correct order (ie., so they line up with the appropriate sensor)

library(dplyr)
library(lubridate)
library(sensorstrings)

# SECTION 1: **Generate File Path** -------------------------------------------------------------

station <- "tickle island 1"
depl_date <- "2020-02-24"

path <- ss_import_path(station, depl_date)

# SECTION 2: Compile & View -----------------------------------------------------------------

dat_raw <- ss_compile_deployment_data(path)

ss_ggplot_variables(dat_raw)

# SECTION 3: Identify Trim Dates --------------------------------------------------------------
# if the app viewer is blank, click "Open in Browser"
# close the app between deployment start and deployment end

# deployment start
ss_open_trimdates_app(dat_raw, filter_to = "start", period = "2 days")

# enter timestamp for first GOOD observation:
depl_start <- as_datetime("2018-09-13 14:13:00")

# deployment end
ss_open_trimdates_app(dat_raw, filter_to = "end", period = "2 days")

# enter timestamp for last GOOD observation:
depl_end <- as_datetime('2019-04-12 18:09:00')


# SECTION 4: Trim Data --------------------------------------------------------------

dat <- dat_raw %>%
  filter(timestamp_utc >= depl_start, timestamp_utc <= depl_end)

# review and adjust if necessary
ss_open_trimdates_app(dat, filter_to = "start", period = "2 days")
ss_open_trimdates_app(dat, filter_to = "end", period = "2 days")

ss_ggplot_variables(dat)

# SECTION 5: Export Data ------------------------------------------------------------------

export_path <- ss_export_path(dat, ext = "rds")

dat %>%
  saveRDS(export_path)

# APPENDIX: additional trim -----------------------------------------------------------

# use this if there is a substantial difference between start times for different sensors
# Make sure the order of the timestamps matches the sensor serial number

# trim_dates <- dat_raw %>%
#   ss_pivot_longer() %>%
#   distinct(variable, sensor_type, sensor_serial_number) %>%
#   arrange(variable) %>%
#   # print trim_dates up to here to ensure timestamps are entered in the correct order
#   mutate(
#     depl_start = as_datetime(
#       c("2019-05-31 03:45:00 UTC",
#         "2019-05-31 00:06:00 UTC",
#         "2019-05-31 01:00:00 UTC",
#         "2019-05-31 01:42:00 UTC",
#         "2019-05-31 00:02:00 UTC",
#         "2019-05-31 00:02:00 UTC")
#     ),
#     depl_end = as_datetime(
#       c("2019-10-19 14:56:00 UTC",
#         "2019-10-19 13:55:00 UTC",
#         "2019-10-19 05:00:00 UTC",
#         "2019-10-18 16:53:00 UTC",
#         "2019-10-19 01:00:00 UTC",
#         "2019-05-31 00:02:00 UTC")
#     )
#   )
#
# dat <- dat_raw %>%
#   ss_pivot_longer() %>%
#   left_join(trim_dates, by = c("variable", "sensor_type", "sensor_serial_number")) %>%
#   filter(timestamp_utc >= depl_start & timestamp_utc <= depl_end) %>%
#   select(-c(depl_start, depl_end)) %>%
#   ss_pivot_wider()
#




