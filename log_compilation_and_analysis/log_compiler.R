library(dplyr)
library(tidyverse)
library(data.table)
library(readxl)
library(here)
library(glue)
library(fs)
library(purrr)
library(janitor)
library(snakecase)

all_station_folder_path <- "R:/data_branches/water_quality/station_folders"
# what does this do?
rel_all_station_folder_path <- path_rel(all_station_folder_path)

# Recurse to find all log folders
log_folder_list <- dir_ls(all_station_folder_path,
                          recurse=TRUE,
                          type="directory",
                          regexp="\\/log$|\\/LOG$|\\/Log$")

# Search first level of log folder for csv and excel files
# Should not include archived logs which are another level down
csv_log_file_list <- compact(lapply(log_folder_list, dir_ls, glob="*.csv"))
xls_log_file_list <- compact(lapply(log_folder_list, dir_ls, glob="*.xls"))
xlsx_log_file_list <- compact(lapply(log_folder_list, dir_ls, glob="*.xlsx"))

excel_log_file_list <- c(xls_log_file_list, xlsx_log_file_list)

# Filter temp files out
csv_log_file_list <- compact(map(
  csv_log_file_list, ~ discard(.x, .p = str_detect(.x, "~"))))
excel_log_file_list <- compact(map(
  excel_log_file_list, ~ discard(.x, .p = str_detect(.x, "~"))))

# TODO? Filter out deployments from a list of deployments to ignore?
# e.g. do we want ferry data in here? do we want data from log files in folders
# that say "do not use"?

# Read the log files into a list of data frames
csv_log_dfs <- lapply(csv_log_file_list, 
                      read_csv, 
                      na=c("n/a", "N/A", "NA", "na", ""))
excel_log_dfs <- lapply(excel_log_file_list, 
                        read_excel, 
                        na=c("n/a", "N/A", "NA", "na", ""))

all_log_dfs <- c(csv_log_dfs, excel_log_dfs)

#df_comparison <- compare_df_cols(all_log_dfs)

# Standardize names of columns
all_log_dfs_std <- lapply(all_log_dfs, rename_with, to_snake_case)

# Assign column names to a type to standardize type across dfs
char_cols <- c("acoustic_release", "anchor_type", "comments", "configuration",
               "datum", "deployment_attendant", "deployment_waterbody",
               "float_type", "lease", "location_description", "logger_model",
               "mount_type", "photos_taken", "retrieval_attendant",
               "rising_or_falling", "status", "surface_buoy", "dist_to_shore", 
               "substrate", "deployment_notes", "retrieval_notes")
numeric_cols <- c("deployment_waypoint", "distance_from_top_of_float_to_origin_first_sensor",
                  "duration", "height_of_vr_2_ar_base_off_bottom", "logger_latitude",
                  "logger_longitude", "retrieval_latitude", "retrieval_longitude",
                  "retrieval_waypoint", "sensor_voltage_deployed",
                  "sensor_voltage_retrieved", "sensor_depth", "serial", 
                  "sounding", "tide_correction",
                  "verified_measurement_below_origin_first_sensor_under_float",
                  "vessel_sounder_offset_transponder_depth", "vessel_offset")
date_cols <- c("deployment", "retrieval")
# TODO: add deployment_time to time cols
time_cols <- c("time_of_deployment")

# Renaming key in the format new_name = old_name
# TODO: rename time_of_deployment to deployment_time also?
rename_key <- c("distance_from_top_of_float_to_origin_first_sensor" = 
                  "distance_from_top_of_float_to_origin_first_sensor_36", 
                "distance_from_top_of_float_to_origin_first_sensor_1" = 
                  "distance_from_top_of_float_to_origin_first_sensor_37",
                "depth" = "depth_maybe",
                "data_processor_notes" = "data_processor_note",
                "deployment_time" = "6",
                "retrieval_time" = "8",
                #"logger_latitude" = "latitude_maybe",
                #"logger_longitude" = "longitude_maybe",
                #"lease" = "lease_maybe",
                "vessel_sounder_offset_transponder_depth" = "vessel_offset")

all_log_dfs_std <- lapply(all_log_dfs_std, rename, any_of(rename_key))

extract_numeric <- function(x) {
  if (!is.character(x)) {
    return(x)
  } else {
    return(parse_number(x))
  }
}

# Standardize format of columns
all_log_dfs_std <- map(all_log_dfs_std, ~ .x %>% 
                         mutate(across(any_of(char_cols), as.character)) %>%
                         mutate(across(any_of(numeric_cols), extract_numeric)) %>%
                         mutate(across(any_of(date_cols), ymd)) %>%
                         mutate(across(any_of(time_cols), hms)))

# Standardize format of columns
std_df_comparison <- compare_df_cols(all_log_dfs_std)

# TODO? Check for duplicate rows in dataframes and flag the file path?
# https://www.r-bloggers.com/2023/07/efficiently-finding-duplicate-rows-in-r-a-comparative-analysis/#:~:text=The%20simplest%20approach%20to%20finding%20duplicate%20rows%20is,our%20data%20frame%20df.%20duplicated_rows_base%20%3C-%20duplicated%20%28df%29

# TODO? Check that the number of deployments matches the expected number?

# Combine all the data frames into a single data frame
stacked_logs_df <- bind_rows(all_log_dfs_std)

# Save to RDS with date to avoid having to read in logs
saveRDS(stacked_logs_df, file=glue("{getwd()}/stacked_logs_{today()}.rds"))
