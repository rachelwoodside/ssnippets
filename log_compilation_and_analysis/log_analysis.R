library(dplyr)
library(tidyverse)
library(data.table)
library(RSocrata)
library(snakecase)
library(here)
library(glue)

filename <- glue("{here()}/stacked_logs_2024-04-15.rds")

logs <- readRDS(filename)

col_list <- colnames(logs)

# deployment_waterbody ---------------------------------------------------------
unique(logs$deployment_waterbody)
# check for rows missing waterbody values
missing_waterbody <- logs %>% filter(is.na(deployment_waterbody))

# location_description ---------------------------------------------------------
unique(logs$location_description)
# investigate unusual station names
weird_station_name_logs <- logs %>% filter(grepl("Logger", location_description))
# check for rows missing station values
missing_station <- logs %>% filter(is.na(location_description))

# lease ------------------------------------------------------------------------
unique(logs$lease)
# TODO: prepend zeroes where appropriate for both log list and ODP list
# no need to check for missing values - lease values are optional
# Check if lease values are valid
odp_lease_api_url <- "https://data.novascotia.ca/resource/h57h-p9mm.csv"
odp_lease_data <- read.socrata(odp_lease_api_url)
odp_lease_list <- as.character(odp_lease_data$license_le)

lease_crosscheck <- setdiff(logs$lease, odp_lease_list)
print(odp_lease_list)
print(lease_crosscheck)
# 5005 and 5007 acceptable because they are experimental leases that were then
# discontinued

# Checking for duplicate leases 
# Get distinct station-lease pairs
unique_stations <- logs %>% 
  select(location_description, lease) %>% 
  distinct()

# status -----------------------------------------------------------------------
# standardize status values
unique(logs$status)
logs <- logs %>% mutate(status = tolower(status))
unique(logs$status)
# acceptable values are: retrieved, lost, deployed
logs <- logs %>% 
  mutate(status = case_when(status == "missing" ~ "lost",
                            status == "currently_deployed" ~ "deployed",
                            status == "currently deployed" ~ "deployed",
                            status == "recovered" ~ "retrieved",
                            status == "retrived" ~ "retrieved",
                            status == "retreived" ~ "retrieved",
                            .default = status))
unique(logs$status)

missing_status <- logs %>% filter(is.na(status))

# deployment (date) ------------------------------------------------------------
unique(logs$deployment)
missing_deployment_date <- logs %>% filter(is.na(deployment))

# retrieval (date) -------------------------------------------------------------
unique(logs$retrieval)
# Correct missing or incorrect status based on whether there is a retrieval date
# Check that all status values where retrieval is null are either "deployed" or "lost"
deployed_or_lost <- logs %>% filter(is.na(retrieval))
unique(deployed_or_lost$status)

# Check that all status values where retrieval is not null are "retrieved"
# Unless there are lost sensors within a retrieved deployment
retrieved <- logs %>% filter(!is.na(retrieval))
unique(retrieved$status)
lost_but_have_retrieval_date <- retrieved %>% filter(status == "lost")
deployed_but_have_retrieval_date <- retrieved %>% filter(status == "deployed")

# duration ---------------------------------------------------------------------
# nothing to do here I don't think, will be calculated by DB
# quick check for unique values here, just to see if there's anything
# that doesn't belong in this column, as was found elsewhere
unique(logs$duration)

# logger_latitude --------------------------------------------------------------
unique(logs$logger_latitude)
missing_latitude <- logs %>% filter(is.na(logger_latitude))

# logger_longitude -------------------------------------------------------------
unique(logs$logger_longitude)
nonnegative_or_missing_longitude <- logs %>% 
  filter(is.na(logger_longitude)|logger_longitude > 0)

# logger_model -----------------------------------------------------------------
unique(logs$logger_model)
# standardize logger names
# set to lowercase to minimize case-sensitive and underscore vs space differences
logs <- logs %>% mutate(logger_model = to_snake_case(logger_model))
unique(logs$logger_model)
# acceptable values are (case-sensitive): HOBO Pro V2, HOBO DO, HOBO Level Logger,
# TidbiT MX 2203, aquaMeasure SAL, aquaMeasure CHL, aquaMeasure DOT, aquaMeasure SST
# aquaMeasure TURB, VR2AR, VR2ARX, DST Comp
# References: 
#https://www.innovasea.com/aquaculture-intelligence/environmental-monitoring/wireless-sensors/
#https://www.onsetcomp.com/products?f%5B0%5D=environment%3A346&f%5B1%5D=product_type%3A931&f%5B2%5D=product_type%3A936
#https://www.innovasea.com/fish-tracking/products/acoustic-receivers/
logs <- logs %>% 
  mutate(logger_model = case_when(logger_model == "hobo_pro_v_2" |
                                    logger_model == "hobo_v_2" ~ 
                                    "HOBO Pro V2",
                                  logger_model == "hobo_do" ~ 
                                    "HOBO DO",
                                  logger_model == "hobo_level_logger" ~ 
                                    "HOBO Level Logger",
                                  logger_model == "tidbit_mx_2303" |
                                    logger_model == "tidbit_mx_2203" |
                                    logger_model == "tidbi_t_mx_2203" |
                                    logger_model == "tidbi_t_mx_2303" ~ 
                                    "TidbiT MX 2203",
                                  logger_model == "aquameasure_sal" |
                                    logger_model == "aqua_measure_sal" |
                                    logger_model == "aqua_measure_salinity" |
                                    logger_model == "aquameasure_salinity" ~ 
                                    "aquaMeasure SAL",
                                  logger_model == "aquameasure_chl" |
                                    logger_model == "aqua_measure_chl" ~ 
                                    "aquaMeasure CHL",
                                  logger_model == "aquameasure_dot" |
                                    logger_model == "aqua_measure_dot" ~ 
                                    "aquaMeasure DOT",
                                  logger_model == "aquameasure_sst" |
                                    logger_model == "aqua_measure_sst" ~ 
                                    "aquaMeasure SST",
                                  logger_model == "aquameasure_turb" |
                                    logger_model == "aqua_measure_turb" ~ 
                                    "aquaMeasure TURB",
                                  logger_model == "vemco_vr_2_ar" |
                                    logger_model == "vr_2_ar" ~ 
                                    "VR2AR",
                                  logger_model == "vr_2_ar_x" ~ 
                                    "VR2ARX",
                                  logger_model == "dst_comp" ~ 
                                    "DST Comp",
                                  .default = logger_model))
unique(logs$logger_model)

# investigate mystery logger models and missing logger models
# HOBO, HOBO V2, HOBO Temp V2, HOBO Tidbit, HOBO Temp U22
mystery_and_missing_logger_models <- logs %>% 
  filter(
    is.na(logger_model)|
    logger_model == "hobo"| 
    logger_model == "hobo_temp_v_2"|
    logger_model == "hobo_tidbit"|
    logger_model == "hobo_v_2"|
    logger_model == "hobo_temp_u_22")

# serial (num) -----------------------------------------------------------------
unique(logs$serial)
missing_serial_num <- logs %>% filter(is.na(serial))
# TODO: Replace missing serial numbers with -111
# Nicole looked into these for sensor inventory 
# Note - serial numbers 3 characters long are DST comp sensors
unique(logs$serial)
short_serial_nums <- logs %>%
  filter(str_length(serial) < 4)

serial_num_len <- unlist(lapply(logs$serial, str_length))
max(serial_num_len, na.rm=TRUE)

# sensor_depth -----------------------------------------------------------------
unique(logs$sensor_depth)
missing_sensor_depth <- logs %>% filter(is.na(sensor_depth))
# super problem row I found with nothing but height of VR2AR off bottom
# anchor type, and float type
# try to find which rows have same values so I can tell which depl it belongs to
#problem_row_matches <- logs %>% filter(anchor_type == "7 rotors" &
#                          height_of_vr_2_ar_base_off_bottom == 1.5 &
#                          float_type == "vinyl")

# sounding ---------------------------------------------------------------------
unique(logs$sounding)
# found a sounding value of 670363 to investigate
#logs %>% filter(sounding == 670363)

# datum ------------------------------------------------------------------------
unique(logs$datum)
# TODO? investigate numeric values of datum (18.5, 13.5, 8.5, 3.5)
mystery_datum_vals <- logs %>% filter(datum == "18.5" | 
                                        datum == "13.5" | 
                                        datum == "8.5" | 
                                        datum == "3.5" |
                                        datum == "WGS85")

# mount_type -------------------------------------------------------------------
unique(logs$mount_type)
logs <- logs %>% mutate(mount_type = tolower(mount_type))
unique(logs$mount_type)

# mooring_type -----------------------------------------------------------------
unique(logs$mooring_type)
logs <- logs %>% mutate(mooring_type = tolower(mooring_type))
unique(logs$mooring_type)

# configuration ----------------------------------------------------------------
unique(logs$configuration)
# Fill in "cinderblock" mount_type as "attached to fixed structure" configuration
logs <- logs %>% 
  mutate(
    configuration = case_when(
      mount_type == "cinder block" | 
        mount_type == "cinderblock" ~ 
        "attached to fixed structure",
      .default = configuration))
unique(logs$configuration)
# TODO: fill in NAs based on configuration table file (and CB config table file)?
config_table_file_path <- "R:/tracking_sheets/water_quality_configuration_table.xlsx"
cb_config_table_file_path <- "R:/tracking_sheets/water_quality_cape_breton_configuration.xlsx"
config_table_data <- read_excel(config_table_file_path, 
                                na = c("", "n/a", "N/A", "NA")) %>% 
  select(Station_Name, Depl_Date, Configuration) %>%
  rename("location_description" = Station_Name,
         "deployment" = Depl_Date,
         "configuration_from_table" = Configuration)

cb_config_table_data <- read_excel(cb_config_table_file_path, 
                                   na = c("", "n/a", "N/A", "NA"))

config_table_join_data <- inner_join(logs, config_table_data, 
             by=c("location_description", "deployment"))
config_nas <- logs %>% filter(is.na(configuration))

# acoustic_release -------------------------------------------------------------
unique(logs$acoustic_release)
logs <- logs %>% 
  mutate(acoustic_release = tolower(str_sub(acoustic_release, 1, 1)))
unique(logs$acoustic_release)
logs %>% filter(is.na(acoustic_release))

# surface_buoy -----------------------------------------------------------------
unique(logs$surface_buoy)

# deployment_attendant ---------------------------------------------------------
unique(logs$deployment_attendant)
depl_att_vals <- count(logs %>% filter(!is.na(deployment_attendant)))
depl_att_nas <- count(logs %>% filter(is.na(deployment_attendant)))
message(glue("Deployment Attendant Values: {depl_att_vals}"))
message(glue("Deployment Attendant NAs: {depl_att_nas}"))

unique(logs$deployment_attendant)
depl_attendant_text_len <- unlist(lapply(logs$deployment_attendant, str_length))
max(depl_attendant_text_len, na.rm=TRUE)


# retrieval_attendant ----------------------------------------------------------
unique(logs$retrieval_attendant)
# Retrieval attendant as "Line 3, East end"?
#logs %>% filter(retrieval_attendant == "Line 3, East end")
retrieval_att_vals <- count(logs %>% filter(!is.na(retrieval_attendant)))
retrieval_att_nas <- count(logs %>% filter(is.na(retrieval_attendant)))
message(glue("Retrieval Attendant Values: {retrieval_att_vals}"))
message(glue("Retrieval Attendant NAs: {retrieval_att_nas}"))

# comments ---------------------------------------------------------------------
# each of these is likely unique
comment_vals <- count(logs %>% filter(!is.na(comments)))
comment_nas <- count(logs %>% filter(is.na(comments)))
message(glue("Comment Values: {comment_vals}"))
message(glue("Comment NAs: {comment_nas}"))

# deployment_waypoint ----------------------------------------------------------
unique(logs$deployment_waypoint)
depl_waypoint_vals <- count(logs %>% filter(!is.na(deployment_waypoint)))
depl_waypoint_nas <- count(logs %>% filter(is.na(deployment_waypoint)))
message(glue("Deployment Waypoint Values: {depl_waypoint_vals}"))
message(glue("Deployment Waypoint NAs: {depl_waypoint_nas}"))

# retrieval_waypoint -----------------------------------------------------------
unique(logs$retrieval_waypoint)
retrieval_waypoint_vals <- count(logs %>% filter(!is.na(retrieval_waypoint)))
retrieval_waypoint_nas <- count(logs %>% filter(is.na(retrieval_waypoint)))
message(glue("Retrieval Waypoint Values: {retrieval_waypoint_vals}"))
message(glue("Retrieval Waypoint NAs: {retrieval_waypoint_nas}"))

# retrieval_latitude -----------------------------------------------------------
unique(logs$retrieval_latitude)
retrieval_latitude_vals <- count(logs %>% filter(!is.na(retrieval_latitude)))
retrieval_latitude_nas <- count(logs %>% filter(is.na(retrieval_latitude)))
message(glue("Retrieval latitude Values: {retrieval_latitude_vals}"))
message(glue("Retrieval latitude NAs: {retrieval_latitude_nas}"))

# retrieval_longitude ----------------------------------------------------------
unique(logs$retrieval_longitude)
retrieval_longitude_vals <- count(logs %>% filter(!is.na(retrieval_longitude)))
retrieval_longitude_nas <- count(logs %>% filter(is.na(retrieval_longitude)))
message(glue("Retrieval longitude Values: {retrieval_longitude_vals}"))
message(glue("Retrieval longitude NAs: {retrieval_longitude_nas}"))

# Check for positive values
nonnegative_retrieval_longitude <- logs %>% 
  filter(retrieval_longitude > 0)

# sensor_voltage_deployed ------------------------------------------------------
unique(logs$sensor_voltage_deployed)
sensor_voltage_depl_vals <- count(
  logs %>% 
    filter(!is.na(sensor_voltage_deployed)))
sensor_voltage_depl_nas <- count(
  logs %>% 
    filter(is.na(sensor_voltage_deployed)))
message(
  glue("Sensor Voltage Deployed Values: {sensor_voltage_depl_vals}"))
message(
  glue("Sensor Voltage Deployed NAs: {sensor_voltage_depl_nas}"))

# sensor_voltage_deployed value to investigate = 354
logs %>% filter(sensor_voltage_deployed == 354)

# sensor_voltage_retrieved -----------------------------------------------------
unique(logs$sensor_voltage_retrieved)
sensor_voltage_retrieved_vals <- count(
  logs %>% 
    filter(!is.na(sensor_voltage_retrieved)))
sensor_voltage_retrieved_nas <- count(
  logs %>% 
    filter(is.na(sensor_voltage_retrieved)))
message(
  glue("Sensor Voltage retrieved Values: {sensor_voltage_retrieved_vals}"))
message(
  glue("Sensor Voltage retrieved NAs: {sensor_voltage_retrieved_nas}"))

# vessel_sounder_offset_transponder_depth --------------------------------------
unique(logs$vessel_sounder_offset_transponder_depth)
vessel_sounder_offset_vals <- count(
  logs %>% 
    filter(!is.na(vessel_sounder_offset_transponder_depth)))
vessel_sounder_offset_nas <- count(
  logs %>% 
    filter(is.na(vessel_sounder_offset_transponder_depth)))
message(
  glue("Vessel Sounder Offset Values: {vessel_sounder_offset_vals}"))
message(
  glue("Vessel Sounder Offset NAs: {vessel_sounder_offset_nas}"))

# sounder offset values to investigate
logs %>% filter(vessel_sounder_offset_transponder_depth > 25)

# verified_measurement_below_origin_first_sensor_under_float -------------------
unique(logs$verified_measurement_below_origin_first_sensor_under_float)
first_sensor_under_float_vals <- count(
  logs %>% 
    filter(!is.na(verified_measurement_below_origin_first_sensor_under_float)))
first_sensor_under_float_nas <- count(
  logs %>% 
    filter(is.na(verified_measurement_below_origin_first_sensor_under_float)))
message(
  glue("First Sensor Under Float Measurement Values: {first_sensor_under_float_vals}"))
message(
  glue("First Sensor Under Float Measurement NAs: {first_sensor_under_float_nas}"))

# first sensor under float measurements to investigate -------------------------
# TODO: What values for this are reasonable? More than 25? 
logs %>% 
  filter(verified_measurement_below_origin_first_sensor_under_float > 25)

# tide_correction --------------------------------------------------------------
unique(logs$tide_correction)
tide_correction_vals <- count(logs %>% filter(!is.na(tide_correction)))
tide_correction_nas <- count(logs %>% filter(is.na(tide_correction)))
message(glue("Tide Correction Values: {tide_correction_vals}"))
message(glue("Tide Correction NAs: {tide_correction_nas}"))

# rising_or_falling ------------------------------------------------------------
unique(logs$rising_or_falling)
rising_or_falling_vals <- count(logs %>% filter(!is.na(rising_or_falling)))
rising_or_falling_nas <- count(logs %>% filter(is.na(rising_or_falling)))
message(glue("Rising or Falling Tide Values: {rising_or_falling_vals}"))
message(glue("Rising or Falling Tide NAs: {rising_or_falling_nas}"))

# height_of_vr_2_ar_base_off_bottom --------------------------------------------
unique(logs$height_of_vr_2_ar_base_off_bottom)
height_of_vr2ar_vals <- count(
  logs %>% 
    filter(!is.na(height_of_vr_2_ar_base_off_bottom)))
height_of_vr2ar_nas <- count(
  logs %>% 
    filter(is.na(height_of_vr_2_ar_base_off_bottom)))
message(
  glue("Height of VR2AR Off Bottom Values: {height_of_vr2ar_vals}"))
message
(glue("Height of VR2AR Off Bottom NAs: {height_of_vr2ar_nas}"))

# time_of_deployment -----------------------------------------------------------
# TODO: Look at these values more carefully - the unique values are weird for time
unique(logs$time_of_deployment)
time_of_deployment_vals <- logs %>% filter(!is.na(time_of_deployment))
time_of_deployment_nas <- logs %>% filter(is.na(time_of_deployment))

# photos_taken -----------------------------------------------------------------
unique(logs$photos_taken)
# investigate unusual "metal slab" value for photos_taken
#logs %>% filter(photos_taken == "Metal slab")
logs <- logs %>% 
  mutate(photos_taken = tolower(str_sub(photos_taken, 1, 1)))
unique(logs$photos_taken)
# TODO: empty rows to "n"
logs %>% mutate(photos_taken = case_when(is.na(photos_taken) ~ "n"))
unique(logs$photos_taken)

# anchor_type ------------------------------------------------------------------
unique(logs$anchor_type)
logs <- logs %>%
  mutate(anchor_type = tolower(anchor_type))
unique(logs$anchor_type)
anchor_type_text_len <- unlist(lapply(logs$anchor_type, str_length))
max(anchor_type_text_len, na.rm=TRUE)

# float_type -------------------------------------------------------------------
unique(logs$float_type)
# TODO: investigate "2 big chains plus an anchor", and "Surface"
# might be in wrong column

# distance_from_top_of_float_to_origin_first_sensor
# and distance_from_top_of_float_to_origin_first_sensor_1 ----------------------

# deployment_time -- salmon rivers ---------------------------------------------
unique(logs$deployment_time)
# TODO: Potentially group in with time_of_deployment (see log_compiler.R)

# retrieval_time -- salmon rivers ----------------------------------------------
unique(logs$retrieval_time)

# dist_to_shore -- salmon rivers -----------------------------------------------
unique(logs$dist_to_shore)

# substrate -- salmon rivers ---------------------------------------------------
unique(logs$substrate)


# TODO: Consider whether these separate notes columns are useful i.e. should 
# they be standard for CMP?
# deployment_notes -------------------------------------------------------------
unique(logs$deployment_notes)

# retrieval_notes --------------------------------------------------------------
unique(logs$retrieval_notes)

# data_processor_notes ---------------------------------------------------------

# data_handler_notes -----------------------------------------------------------

# depth -- depth of what?? -----------------------------------------------------
# TODO: Check where this comes from?
unique(logs$depth)

# depth_of_water_m -------------------------------------------------------------
# TODO: Check which log this comes from - salmon rivers?
unique(logs$depth_of_water_m)

# secondary_float_type ---------------------------------------------------------
# TODO: Check which log this is coming from - this matches new log format
unique(logs$secondary_float_type)

# "14" -------------------------------------------------------------------------
unique(logs$`14`)
# TODO: delete

# "37" -------------------------------------------------------------------------
unique(logs$`37`)
# TODO: move one existing value into comments section, then delete

