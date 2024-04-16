# DATE: 2023-11-14
# NAME: RW
# NOTES:

# Code for compiling a franken-deployment from one deployment and any other deployment 
# Takes in processed data as an rds file then exports it as a new rds file in 
# the folder in which this code is run

library(tidyr)
library(dplyr)
library(lubridate)
library(purrr)
library(rstudioapi)
library(sensorstrings)

# SECTION 1: **Get Base Data** -------------------------------------------------------------

# Set working directory to processed deployment data folder to facilitate RDS file selection
setwd("R:/data_branches/water_quality/processed_data/deployment_data/")
getwd()

# Select base dataset to work from: e.g. Birchy Head 2022-05-12 (Lunenberg)
#TODO: Does a function in sensorstrings exist to read in processed data? If not, perhaps write a 
# helper function that works similarly to the sensorstrings function for reading in raw data,
# but for processed data?
base_filename <- file.choose()
base_data <- readRDS(base_filename)

# SECTION 2: **Find Missing Variables in Base Data** ---------------------------------

# TODO: Instead check columns against list of all variables to see what isn't measured
# in this deployment?
var_list <- c("dissolved_oxygen_uncorrected_mg_per_l", "sensor_depth_measured_m", "temperature_degree_c", "dissolved_oxygen_percent_saturation", "salinity_psu")
# Generates the list of column names that are entirely NA values
base_data_missing_vars <- base_data %>% keep(\(x) all(is.na(x))) %>% names

# SECTION 3: **Select Date Range for Base Data** -------------------------------------------------------------

# Potentially useful date values
#base_data_min_date <- min(base_data$timestamp_utc)
#base_data_first_week_end <- base_data_min_date + days(7)
#base_data_median_date <- median(base_data$timestamp_utc)
base_data_max_date <- max(base_data$timestamp_utc)
base_data_last_week_start <- base_data_max_date - days(7)

# Set start and end dates to filter: fill these out as desired
base_data_start_date <- base_data_last_week_start
base_data_end_date <- base_data_max_date

# Customize which portion of the data to keep: e.g. last week
base_data_date_filtered <- filter(base_data, between(timestamp_utc, base_data_start_date, base_data_end_date))

# SECTION 4: **Get Additional Data** -------------------------------------------------------------

# Add in another deployment: e.g. Birchy Head 2019-05-02 (Lunenberg)
added_filename <- file.choose()
added_data <- readRDS(added_filename)

# Filter new data to rows that have the desired variable: e.g. salinity
added_data <- filter(added_data, !is.na(salinity_psu))

# SECTION 5: **Select Date Range for Additional Data** -------------------------------------------------------------

# Potentially useful date values
#added_data_min_date <- min(added_data$timestamp_utc)
#added_data_first_week_end <- added_data_min_date + days(7)
#added_data_median_date <- median(added_data$timestamp_utc)
added_data_max_date <- max(added_data$timestamp_utc)
added_data_last_week_start <- max(added_data$timestamp_utc) - days(7)

# Set start and end dates to filter: fill these out as desired
added_data_start_date <- added_data_last_week_start
added_data_end_date <- added_data_max_date

added_data_date_filtered <- filter(added_data, between(timestamp_utc, added_data_start_date, added_data_end_date))

# SECTION 6: **OPTIONAL: Overwrite Dates for Added Data ** ---------------------------------
# Notes: 
# This works even with a different number of observations for each dataset
# This sets the date values according to the start date of the base data
# This does not change the deployment range column, only the timestamp_utc 
# column for the purposes of plotting
# TODO: Add option to replace deployment range column?

date_diff <- base_data_start_date - added_data_start_date
added_data_date_filtered <- added_data_date_filtered %>% mutate(timestamp_utc = timestamp_utc + date_diff)

# SECTION 6: **Combine Data** -------------------------------------------------------------

franken_data <- bind_rows(base_data_date_filtered, added_data_date_filtered)

# SECTION 7: **OPTIONAL: Reduce Size of Final Data** -------------------------------------------------------------


# TODO: Figure out which depths are associated with which variables
#depth_var_pairs <- distinct(franken_data %>% select(any_of(c("sensor_depth_at_low_tide_m", var_list))))
#depth_var_pairs <- distinct(franken_data %>% 
 #                             select(any_of(c("sensor_depth_at_low_tide_m", var_list))) %>%
 #                             filter_at(var_list[0], any_vars(is.na(.))))
#depths_to_keep <- depths # keep all
#depths_to_keep <- c() # keep a selection
#distinct_depths <- distinct(base_data_date_filtered, sensor_depth_at_low_tide_m)$sensor_depth_at_low_tide_m
#distinct_depths <- sort(distinct_depths)
#val_depths_to_keep <- distinct_depths[1:n_depths_to_keep]

#franken_data <- filter(franken_data, sensor_depth_at_low_tide_m %in% val_depths_to_keep)

# SECTION 8: **Summarize Final Franken-deployment Data** -------------------------

# Check for any columns that are all NA values
franken_data_missing_vars <- franken_data %>% keep(\(x) all(is.na(x))) %>% names

# TODO: Check number of depths? Check dates the deployment spans? 

# Generate plot like in sensorstrings
ss_ggplot_variables(franken_data)

# SECTION 9: **Save Final Franken-deployment Data** ---------------------------------

# Set working directory to location of this R file (or edit for a directory of your choice)
setwd(dirname(getActiveDocumentContext()$path))
getwd()

franken_data_filename <- "rfq_sample_processed_data"

# Write to RDS
franken_data %>%
  saveRDS(file=paste(franken_data_filename,".rds", sep=""))

# Write to CSV
write.table(franken_data, file=paste(franken_data_filename,".csv", sep=""), sep=",")
