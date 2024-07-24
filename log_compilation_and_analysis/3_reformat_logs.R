# Should we start a CMAR header standard?
# Date, Name, 1 - 2 sentences about what the script does?

# Library imports --------------------------------------------------------------
library(dplyr)
library(here)
library(readxl)
library(writexl)
library(glue)
library(lubridate)

# Read in data -----------------------------------------------------------------
# Clean logs
clean_logs_filename <- here("clean_stacked_logs_2024-07-24.rds")
clean_logs_df <- readRDS(clean_logs_filename)

# Log format file
log_format_filename <-
  "R:/data_branches/water_quality/processing_resources/2024-05-28_new_log_format.xlsx"
log_format_df <- read_excel(path = log_format_filename,
                            sheet = "log_example",
                            col_names = TRUE)
log_col_names <- colnames(log_format_df)

# Deployment tracking metadata file
depl_tracking_metadata_filename <-
  "R:/tracking_sheets/metadata_tracking/water_quality_deployment_tracking.xlsx"
depl_tracking_metadata_df <-
  read_excel(path = depl_tracking_metadata_filename,
             col_names = TRUE)
depl_tracking_metadata_col_names <-
  colnames(depl_tracking_metadata_df)

# Check for column consistency between log format file and deployment tracking metadata file
identical(depl_tracking_metadata_col_names, log_col_names)

# Update to standard 2024 log format ------------------------------------------

# Rename columns to match deployment tracking sheet/metadata
clean_logs_df <- clean_logs_df %>%
  rename(waterbody = deployment_waterbody) %>%
  rename(station = location_description) %>%
  rename(deployment_date = deployment) %>%
  rename(retrieval_date = retrieval) %>%
  rename(deployment_latitude = logger_latitude) %>%
  rename(deployment_longitude = logger_longitude) %>%
  rename(sensor_type = logger_model) %>%
  rename(sensor_serial_number = serial) %>%
  rename(sensor_depth_m = sensor_depth) %>%
  rename(sounding_m = sounding) %>%
  rename(tide_correction_m = tide_correction) %>%
  rename(deployment_tide_direction = rising_or_falling) %>%
  rename(vr2ar_lug_height_above_seafloor_m = height_of_vr_2_ar_base_off_bottom) %>%
  rename(primary_buoy_type = float_type) %>%
  rename(deployment_time_utc = deployment_time) %>%
  rename(retrieval_time_utc = retrieval_time) %>%
  rename(secondary_buoy_type = secondary_float_type) %>%
  rename(string_configuration = configuration) %>%
  # Add and populate default values for missing columns
  mutate(bottom_buoy_type = NA) %>%
  mutate(biofouling_prevention = "none")

# Drop unneccessary columns and reorder to match log format
clean_logs_df <- clean_logs_df %>%
  select(all_of(log_col_names))

# Check that all column names are present
setdiff(log_col_names, colnames(clean_logs_df))

# Generate file with current CMP deployments in standard 2024 log format -------
# Filter for currently deployed logs
currently_deployed_clean <- clean_logs_df %>% filter(status == "deployed")
# Write to Excel file
write_xlsx(currently_deployed_clean, 
           path = glue("{getwd()}/deployment_metadata_{today()}.rds"),
           format_headers = FALSE)

# Generate file with all deployments in standard 2024 log format ---------------
# Write to RDS file
saveRDS(clean_logs_df,
        file = glue("{getwd()}/clean_stacked_reformatted_logs_{today()}.rds"))
