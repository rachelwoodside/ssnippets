# Should we start a CMAR header standard?
# Date, Name, 1 - 2 sentences about what the script does?

# Library imports --------------------------------------------------------------
library(dplyr)
library(here)
library(readxl)
library(writexl)

# Read in data -----------------------------------------------------------------
# Clean logs
clean_logs_filename <- here("clean_stacked_logs_2024-05-23.rds")
clean_logs_df <- readRDS(clean_logs_filename)

# Log format file
log_format_filename <- here("2024-03-26_new_log_format.xlsx")
log_format_df <- read_excel(path = log_format_filename,
                            sheet = "log_example",
                            col_names = TRUE)
log_col_names <- colnames(log_format_df)

# Update to standard 2024 log format -------------------------------------------
# Rename columns to match deployment tracking sheet/metadata
clean_logs_df <- clean_logs_df %>%
  rename(waterbody = deployment_waterbody) %>%
  rename(station = location_description) %>%
  # lease -- unchanged
  # status -- unchanged
  rename(deployment_date = deployment) %>%
  rename(retrieval_date = retrieval) %>%
  select(!duration) %>%
  rename(deployment_latitude = logger_latitude) %>%
  rename(deployment_longitude = logger_longitude) %>%
  rename(sensor_type = logger_model) %>%
  rename(sensor_serial_number = serial) %>%
  rename(sensor_depth_m = sensor_depth) %>%
  rename(sounding_m = sounding) %>%
  # datum -- unchanged
  select(!mount_type) %>%
  # acoustic_release -- unchanged
  select(!surface_buoy) %>%
  # notes -- unchanged
  select(!deployment_waypoint) %>%
  select(!retrieval_waypoint) %>%
  # retrieval_latitude and retrieval_longitude -- unchanged
  select(!sensor_voltage_deployed) %>%
  select(!sensor_voltage_retrieved) %>%
  select(!vessel_sounder_offset_transponder_depth) %>%
  select(!verified_measurement_below_origin_first_sensor_under_float) %>%
  rename(tide_correction_m = tide_correction) %>%
  rename(deployment_tide_direction = rising_or_falling) %>%
  rename(vr2ar_lug_height_above_seafloor_m = height_of_vr_2_ar_base_off_bottom) %>%
  # photos_taken -- unchanged
  # anchor_type -- unchanged
  rename(primary_buoy_type = float_type) %>%
  rename(deployment_time_utc = deployment_time) %>%
  rename(retrieval_time_utc = retrieval_time) %>%
  select(!dist_to_shore) %>%
  select(!substrate) %>%
  select(!mooring_type) %>%
  select(!lease_maybe) %>%
  select(!latitude_maybe) %>%
  select(!longitude_maybe) %>%
  select(!depth_maybe) %>%
  select(!`15`) %>%
  rename(secondary_buoy_type = secondary_float_type) %>%
  rename(string_configuration = configuration) %>%
# deployment_attendant and retrieval_attendant -- unchanged
# anchor_weight_in_kg -- unchanged
  mutate(bottom_buoy_type = NA) %>%
  mutate(biofouling_prevention = "none")

# Drop unneccessary columns and reorder to match log format
clean_logs_df <- clean_logs_df %>%
  select(all_of(log_col_names))

# Check that all column names are present
setdiff(log_col_names, colnames(clean_logs_df))

# Generate file with standard 2024 log format ----------------------------------
# Filter for currently deployed logs
currently_deployed_clean <- clean_logs_df %>% filter(status == "deployed")

# Generate tables for database import ------------------------------------------
# TODO