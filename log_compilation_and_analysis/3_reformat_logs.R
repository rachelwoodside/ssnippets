# Should we start a CMAR header standard?
# Date, Name, 1 - 2 sentences about what the script does?

# Library imports --------------------------------------------------------------
library(dplyr)
library(here)

# Read in data -----------------------------------------------------------------
clean_logs_filename <- here("clean_stacked_logs_2024-05-09.rds")
clean_logs_df <- readRDS(clean_logs_filename)

# Generate file with standard 2024 log format ----------------------------------
# Filter for currently deployed logs
currently_deployed_clean <- clean_logs_df %>% filter(status == "currently deployed")
# Rename columns to match deployment tracking sheet/metadata
currently_deployed_clean <- currently_deployed_clean %>%
  rename(station = location_description) %>%
  rename(waterbody = deployment_waterbody) %>%
  rename(deployment_date = deployment) %>%
  rename(retrieval_date = retrieval) %>%
  rename(deployment_latitude = logger_latitude) %>%
  rename(deployment_longitude = logger_longitude) %>%
  rename(sensor_type = logger_model) %>%
  rename(sensor_serial_number = serial) %>%
  rename(sensor_depth_m = sensor_depth) %>%
  rename(string_configuration = configuration) %>%
  rename(sounding_m = sounding) %>%
  rename(tide_correction_m = tide_correction) %>%
  rename(vr2ar_lug_height_above_seafloor = height_of_vr_2_ar_base_off_bottom) %>%
  rename(primary_buoy_type = float_type) %>%
  rename(bottom_buoy_type = secondary_float_type) %>%
  rename(notes = comments) %>%
  # TODO: fix missing values
  mutate(deployment_time_utc = "NA") %>%
  mutate(retrieval_time_utc = "NA")  %>%
  mutate(anchor_weight_kg = "NA") %>%
  mutate(secondary_buoy_type = "NA") %>%
  mutate(biofouling_prevention = "none")

# Drop unneccessary columns
currently_deployed_clean <-
  currently_deployed_clean[, c(
    "station",
    "waterbody",
    "lease",
    "status",
    "deployment_date",
    "retrieval_date",
    "deployment_time_utc",
    "retrieval_time_utc",
    "deployment_latitude",
    "deployment_longitude",
    "retrieval_latitude",
    "retrieval_longitude",
    "sensor_type",
    "sensor_serial_number",
    "sensor_depth_m",
    "string_configuration",
    "sounding_m",
    "acoustic_release",
    "tide_correction_m",
    "vr2ar_lug_height_above_seafloor",
    "deployment_tide_direction",
    "primary_buoy_type",
    "secondary_buoy_type",
    "bottom_buoy_type",
    "anchor_type",
    "anchor_weight_kg",
    "biofouling_prevention",
    "datum",
    "deployment_attendant",
    "retrieval_attendant",
    "photos_taken",
    "notes"
  )]

# Generate tables for database import ------------------------------------------
# TODO