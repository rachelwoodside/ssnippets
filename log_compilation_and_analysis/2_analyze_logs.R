# Should we start a CMAR header standard?
# Date, Name, 1 - 2 sentences about what the script does?

# Library imports --------------------------------------------------------------
library(dplyr)
library(data.table)
library(RSocrata)
library(snakecase)
library(here)
library(glue)
library(readxl)
library(stringr)
library(purrr)


# Helper functions -------------------------------------------------------------
prepend_lease_zeroes <- function(lease_num) {
  if (!is.na(lease_num)) {
    while (str_length(lease_num) < 4) {
      lease_num <- glue("0{lease_num}")
    }
  }
  return(lease_num)
}

sort_unique_vals <- function(vec) {
  sort(unique(vec), na.last = FALSE)
}

# File import and basic info ---------------------------------------------------

filename <- here("stacked_logs_2024-04-29.rds")

logs <- readRDS(filename)

col_list <- colnames(logs)

# deployment_waterbody ---------------------------------------------------------
sort_unique_vals(logs$deployment_waterbody)
# check for rows missing waterbody values
any(is.na(logs$deployment_waterbody))
# identify rows with missing waterbody values
#missing_waterbody <- logs %>% filter(is.na(deployment_waterbody))

# location_description ---------------------------------------------------------
sort_unique_vals(logs$location_description)
# investigate unusual station names
#weird_station_name_logs <- logs %>% filter(grepl("Logger", location_description))
# check for rows missing station values
any(is.na(logs$location_description))
# identify rows with missing station values
#missing_station <- logs %>% filter(is.na(location_description))

# lease ------------------------------------------------------------------------
sort_unique_vals(logs$lease)
# Prepend zeroes to leases in logs
logs$lease <- unlist(lapply(logs$lease, prepend_lease_zeroes))

# Check if lease values are valid based on ODP dataset
odp_lease_api_url <- "https://data.novascotia.ca/resource/h57h-p9mm.csv"
odp_lease_data <- read.socrata(odp_lease_api_url)
odp_lease_data <-
  odp_lease_data %>% mutate(license_le = as.character(license_le))
odp_lease_data$license_le <-
  unlist(lapply(odp_lease_data$license_le, prepend_lease_zeroes))

# TODO: add split on "/" character (or others?) for checking multi-lease stations
lease_crosscheck <- setdiff(logs$lease, odp_lease_data$license_le)
print(lease_crosscheck)
# The ODP Lease List does not seem to include historical or experimental leases
# Please check against other sources to confirm whether leases are valid
# For example: https://novascotia.ca/fish/aquaculture/site-mapping-tool/

# 5005 and 5007 were experimental leases
# 0967 was a historical lease

# Checking for leases paired with multiple stations and vice versa
unique_stations <- logs %>%
  distinct(location_description, lease)

# status -----------------------------------------------------------------------
# standardize status values
logs <- logs %>% mutate(status = tolower(status))
sort_unique_vals(logs$status)
# acceptable values are: retrieved, lost, deployed
logs <- logs %>%
  mutate(status = case_when(status == "missing" ~ "lost",
                            status == "currently_deployed" ~ "deployed",
                            status == "currently deployed" ~ "deployed",
                            status == "recovered" ~ "retrieved",
                            status == "retrived" ~ "retrieved",
                            status == "retreived" ~ "retrieved",
                            .default = status))
sort_unique_vals(logs$status)
# check for rows missing status values
any(is.na(logs$status))
# identify rows with missing status values
#missing_status <- logs %>% filter(is.na(status))

# deployment (date) ------------------------------------------------------------
sort_unique_vals(logs$deployment)
# check for rows missing deployment dates
any(is.na(logs$deployment))
# identify rows with missing deployment dates
#missing_deployment_date <- logs %>% filter(is.na(deployment))

# retrieval (date) -------------------------------------------------------------
sort_unique_vals(logs$retrieval)
# Correct missing or incorrect status based on whether there is a retrieval date
# Check that all status values where retrieval is null are either "deployed" or "lost"
deployed_or_lost <- logs %>% filter(is.na(retrieval))
sort_unique_vals(deployed_or_lost$status)

# Check that all status values where retrieval is not null are "retrieved"
# Unless there are lost sensors within a retrieved deployment
retrieved <- logs %>% filter(!is.na(retrieval))
sort_unique_vals(retrieved$status)
lost_but_have_retrieval_date <-
  retrieved %>% filter(status == "lost")
deployed_but_have_retrieval_date <-
  retrieved %>% filter(status == "deployed")

# duration ---------------------------------------------------------------------
# nothing to do here I don't think, will be calculated by DB
# quick check for unique values here, just to see if there's anything
# that doesn't belong in this column, as was found elsewhere
sort_unique_vals(logs$duration)

# logger_latitude --------------------------------------------------------------
sort_unique_vals(logs$logger_latitude)
# check for rows missing logger latitudes
any(is.na(logs$logger_latitude))
# identify rows with missing logger_latitude
#missing_latitude <- logs %>% filter(is.na(logger_latitude))

# logger_longitude -------------------------------------------------------------
sort_unique_vals(logs$logger_longitude)
# check for rows missing logger_longitude
any(is.na(logs$logger_longitude))
any(logs$logger_longitude > 0)
# identify rows with missing or nonnegative logger_longitude
nonnegative_or_missing_longitude <-
  logs %>% filter(is.na(logger_longitude) | logger_longitude > 0)

# logger_model -----------------------------------------------------------------
sort_unique_vals(logs$logger_model)
# standardize logger names
# set to lowercase to minimize case-sensitive and underscore vs space differences
logs <- logs %>% mutate(logger_model = to_snake_case(logger_model))
sort_unique_vals(logs$logger_model)
# acceptable values are (case-sensitive): HOBO Pro V2, HOBO DO, HOBO Level Logger,
# TidbiT MX 2203, aquaMeasure SAL, aquaMeasure CHL, aquaMeasure DOT, aquaMeasure SST
# aquaMeasure TURB, VR2AR, VR2ARX, DST Comp
# References:
#https://www.innovasea.com/aquaculture-intelligence/environmental-monitoring/wireless-sensors/
#https://www.onsetcomp.com/products?f%5B0%5D=environment%3A346&f%5B1%5D=product_type%3A931&f%5B2%5D=product_type%3A936
#https://www.innovasea.com/fish-tracking/products/acoustic-receivers/

# Define alternative naming groups
hobo_pro_v2_synonyms <-
  c("hobo_pro_v_2",
    "hobo_v_2",
    "hobo_temp_v_2",
    "hobo_v_2",
    "hobo_temp_u_22")
hobo_do_synonyms <- c("hobo_do")
hobo_level_logger_synonyms <- c("hobo_level_logger")
tidbit_mx_2203_synonyms <-
  c("tidbit_mx_2303",
    "tidbit_mx_2203",
    "tidbi_t_mx_2203",
    "tidbi_t_mx_2303")
aquameasure_sal_synonyms <-
  c(
    "aquameasure_sal",
    "aqua_measure_sal",
    "aqua_measure_salinity",
    "aquameasure_salinity"
  )
aquameasure_chl_synonyms <- c("aquameasure_chl", "aqua_measure_chl")
aquameasure_dot_synonyms <- c("aquameasure_dot", "aqua_measure_dot")
aquameasure_sst_synonyms <- c("aquameasure_sst", "aqua_measure_sst")
aquameasure_turb_synonyms <- c("aquameasure_turb", "aqua_measure_turb")
vr2ar_synonyms <- c("vemco_vr_2_ar", "vr_2_ar")
vr2arx_synonyms <- c("vr_2_ar_x")
dst_comp_synonyms <- c("dst_comp")

logs <-
  logs %>% mutate(
    logger_model = case_when(
      logger_model %in% hobo_pro_v2_synonyms ~ "HOBO Pro V2",
      logger_model %in% hobo_do_synonyms ~ "HOBO DO",
      logger_model %in% hobo_level_logger_synonyms ~ "HOBO Level Logger",
      logger_model %in% tidbit_mx_2203_synonyms ~ "TidbiT MX 2203",
      logger_model %in% aquameasure_sal_synonyms ~ "aquaMeasure SAL",
      logger_model %in% aquameasure_chl_synonyms ~ "aquaMeasure CHL",
      logger_model %in% aquameasure_dot_synonyms ~ "aquaMeasure DOT",
      logger_model %in% aquameasure_sst_synonyms ~ "aquaMeasure SST",
      logger_model %in% aquameasure_turb_synonyms ~ "aquaMeasure TURB",
      logger_model %in% vr2ar_synonyms ~ "VR2AR",
      logger_model %in% vr2arx_synonyms ~ "VR2ARX",
      logger_model %in% dst_comp_synonyms ~ "DST Comp",
      .default = logger_model
    )
  )
sort_unique_vals(logs$logger_model)

# investigate mystery logger models and missing logger models
#sort_unique_vals(logs$logger_model)
# Edit the list below if coming across an unfamiliar logger model:
# mystery_logger_models <- c("hobo")
# identify rows with missing or unusual logger_models
# mystery_and_missing_logger_models <-
#   logs %>% filter(is.na(logger_model) |
#                     logger_model %in% mystery_logger_models)

# serial (num) -----------------------------------------------------------------
sort_unique_vals(logs$serial)
# check for rows missing serial num
any(is.na(logs$logger_latitude))
# identify rows with missing serial numbers
# missing_serial_num <- logs %>% filter(is.na(serial))

# Note - serial numbers 3 characters long are DST comp sensors
# short_serial_nums <- logs %>%
#   filter(str_length(serial) < 4)

# Determine max length of serial numbers
serial_num_len <- unlist(lapply(logs$serial, str_length))
max(serial_num_len, na.rm=TRUE) # smart to check this

# sensor_depth -----------------------------------------------------------------
sort_unique_vals(logs$sensor_depth)
# check for rows missing sensor depth
any(is.na(logs$sensor_depth))
# identify rows with missing serial numbers
# missing_sensor_depth <- logs %>% filter(is.na(sensor_depth))

# sounding ---------------------------------------------------------------------
sort_unique_vals(logs$sounding)
# found a sounding value of 670363 to investigate
# logs %>% filter(sounding == 670363)

# datum ------------------------------------------------------------------------
sort_unique_vals(logs$datum)
# Investigate unusual values of datum (18.5, 13.5, 8.5, 3.5, WGS85)
# unusual_datum_vals <- c("3.5", "8.5", "13.5", "18.5", "WGS85")
# mystery_datum_vals <- logs %>% filter(datum %in% unusual_datum_vals)

# mount_type -------------------------------------------------------------------
sort_unique_vals(logs$mount_type)
logs <- logs %>% mutate(mount_type = tolower(mount_type))
# TODO: Clean up and standardize mount types
sort_unique_vals(logs$mount_type)

# mooring_type -----------------------------------------------------------------
sort_unique_vals(logs$mooring_type)
logs <- logs %>% mutate(mooring_type = tolower(mooring_type))
# TODO: Clean up and standardize mooring types
sort_unique_vals(logs$mooring_type)

# configuration ----------------------------------------------------------------
sort_unique_vals(logs$configuration)
# Fill in "cinderblock" mount_type as "attached to fixed structure" configuration
# logs <-
#   logs %>% mutate(
#     configuration = case_when(
#       mount_type == "cinder block" |
#         mount_type == "cinderblock" ~ "attached to fixed structure",
#       .default = configuration
#     )
#   )

logs <- logs %>% rename("log_configuration" = configuration)

# Fill in NAs based on configuration table file (and CB config table file)?
config_table_file_path <-
  "R:/tracking_sheets/water_quality_configuration_table.xlsx"
cb_config_table_file_path <-
  "R:/tracking_sheets/water_quality_cape_breton_configuration.xlsx"

config_table_data <-
  read_excel(config_table_file_path, na = c("", "n/a", "N/A", "NA")) %>%
  select(Station_Name, Depl_Date, Configuration) %>%
  rename(
    "location_description" = Station_Name,
    "deployment" = Depl_Date,
    "table_configuration" = Configuration
  ) %>%
  mutate(deployment = ymd(deployment))

config_table_join_data <-
  left_join(logs,
            config_table_data,
            by = c("location_description", "deployment"))

cb_config_table_data <-
  read_excel(cb_config_table_file_path, na = c("", "n/a", "N/A", "NA")) %>%
  select(Station_Name, Depl_Date, Configuration) %>%
  rename(
    "location_description" = Station_Name,
    "deployment" = Depl_Date,
    "cb_table_configuration" = Configuration
  ) %>%
  mutate(deployment = ymd(deployment))

config_table_join_data <-
  left_join(config_table_join_data,
            cb_config_table_data,
            by = c("location_description", "deployment"))

colnames(config_table_join_data)

# Check if any configuration data is not filled by the logs or the config table
missing_any_config <- config_table_join_data %>%
  filter(is.na(config_table_join_data$log_configuration) &
           is.na(config_table_join_data$table_configuration) &
           is.na(config_table_join_data$cb_table_configuration)) %>%
  select(location_description, deployment, log_configuration, table_configuration, cb_table_configuration)
# TODO: Review log entries with no configuration data

# TODO: Consider using right join to check for entries in the config table
# that are missing from the logs

# TODO: Merge configuration data into a single column

# acoustic_release -------------------------------------------------------------
sort_unique_vals(logs$acoustic_release)
logs <-
  logs %>% mutate(acoustic_release = tolower(str_sub(acoustic_release, 1, 1)))
sort_unique_vals(logs$acoustic_release)
# check for rows missing acoustic_release values
any(is.na(logs$acoustic_release))

# surface_buoy -----------------------------------------------------------------
sort_unique_vals(logs$surface_buoy)
# TODO: Check "mounted to oyster cage" value for surface buoy
logs %>% filter(surface_buoy == "Mounted to oyster cage")
# check for rows missing surface_buoy values
any(is.na(logs$surface_buoy))

# CODE REVIEW FEEDBACK ENDS AROUND HERE ----------------------------------------

# deployment_attendant ---------------------------------------------------------
# TODO: Clean up and standardize deployment attendant names
sort_unique_vals(logs$deployment_attendant)
depl_att_vals <- count(logs %>% filter(!is.na(deployment_attendant)))
depl_att_nas <- count(logs %>% filter(is.na(deployment_attendant)))
message(glue("Deployment Attendant Values: {depl_att_vals}"))
message(glue("Deployment Attendant NAs: {depl_att_nas}"))

sort_unique_vals(logs$deployment_attendant)
depl_attendant_text_len <- unlist(lapply(logs$deployment_attendant, str_length))
max(depl_attendant_text_len, na.rm=TRUE)

# retrieval_attendant ----------------------------------------------------------
# TODO: Clean up and standardize retrieval attendant names
sort_unique_vals(logs$retrieval_attendant)
# Retrieval attendant as "Line 3, East end"?
#logs %>% filter(retrieval_attendant == "Line 3, East end")
retrieval_att_vals <-
  count(logs %>% filter(!is.na(retrieval_attendant)))
retrieval_att_nas <-
  count(logs %>% filter(is.na(retrieval_attendant)))
message(glue("Retrieval Attendant Values: {retrieval_att_vals}"))
message(glue("Retrieval Attendant NAs: {retrieval_att_nas}"))


# Examine all attendant values
all_attendant_vals <- sort(unique(c(logs$deployment_attendant, logs$retrieval_attendant)))
all_attendant_vals <- all_attendant_vals %>% lapply(str_split_1, pattern=",|&|/|(and)") %>% unlist %>% trimws %>% sort_unique_vals
all_attendant_vals

albert_spears_alternatives <- c("Albert")


identified_individual_attendants <-
  c(
    "Albert Spears", # need to add last name
    "Aleasha Boudreau",
    "Andrew Bagnall",
    "Bear River", # check relationship with innovative fisheries
    "Betty Roethlisberger", # need to add last name, manage typos
    "Blair Golden",
    "Brett Savoury", # need to add last name, manage nicknames
    "Brian Fortune",
    "Brian Lewis",
    "Bruce Hatcher",
    "Carol Ann",
    "CMAR",
    "Connors Diving",
    "Corey Bowen",
    "David Burns", # need to expand first initial
    "Danny Rowe", # need to add last name, expand first initial, expand last initial, manage nicknames
    "Danielle St. Louis",
    "David Cook", # need to expand first initial, manage nicknames?
    "Dave Macneil", # same as Dave McNeill? maybe check occurrence of each in logs
    "Duncan Bates",
    "Esha", # last name anywhere?
    "Evan", # last name anywhere?
    "Gregor Reid",
    "Innovative Fisheries", # group with innovative crew
    "Isabelle Trembley",
    "Jamie Warford", # check for typos
    "Jamie Sangster",
    "Jesse Fortune",
    "Jessica Feindel",
    "Joe Erly",
    "Josh Hatt", # need to expand first initial
    "Karen Campbell",
    "Kate Richardson", # need to expand first initial
    "Kiersten Watson", # confirm this is CMAR kiersten and parse out spare info
    "L Clancey", # find full first name and expand?
    "Leeway Marine", # group with leeway crew and leeway marine crew
    "Matthew Hatcher", # need to expand first initial, manage nicknames
    "Mark Decker", # need to add last name
    "Matt King",
    "Matthew Theriault",
    "Merinov",
    "Michelle Plamondon", # need to add last name
    "Mike (B&S)", # include the b&s for clarity?
    "Nathaniel Feindel",
    "Nick Nickerson",
    "Paul Budreski",
    "Phil Docker",
    "Robin Stuart",
    "Sam Pascoe (B&s)",
    "Scott Hatcher", # need to expand first initial
    "Stephen Macintosh",
    "Timothy Dada", # need to expand first initial, manage typos, manage nicknames
    "Toby Balch", # need to expand first initial
    "Todd Mosher", # need to expand first initial
    "Trevor Munroe",
    "Troy (B&S)",
    "Vicki Swan",
    "Will Rowe" # need to add last name
  )
length(identified_individual_attendants)


# comments ---------------------------------------------------------------------
# each of these is likely unique
# TODO? Check for unusual values, like numerics
comment_vals <- count(logs %>% filter(!is.na(comments)))
comment_nas <- count(logs %>% filter(is.na(comments)))
message(glue("Comment Values: {comment_vals}"))
message(glue("Comment NAs: {comment_nas}"))

# deployment_waypoint ----------------------------------------------------------
sort_unique_vals(logs$deployment_waypoint)
depl_waypoint_vals <- count(logs %>% filter(!is.na(deployment_waypoint)))
depl_waypoint_nas <- count(logs %>% filter(is.na(deployment_waypoint)))
message(glue("Deployment Waypoint Values: {depl_waypoint_vals}"))
message(glue("Deployment Waypoint NAs: {depl_waypoint_nas}"))

# retrieval_waypoint -----------------------------------------------------------
sort_unique_vals(logs$retrieval_waypoint)
retrieval_waypoint_vals <- count(logs %>% filter(!is.na(retrieval_waypoint)))
retrieval_waypoint_nas <- count(logs %>% filter(is.na(retrieval_waypoint)))
message(glue("Retrieval Waypoint Values: {retrieval_waypoint_vals}"))
message(glue("Retrieval Waypoint NAs: {retrieval_waypoint_nas}"))

# retrieval_latitude -----------------------------------------------------------
sort_unique_vals(logs$retrieval_latitude)
retrieval_latitude_vals <- count(logs %>% filter(!is.na(retrieval_latitude)))
retrieval_latitude_nas <- count(logs %>% filter(is.na(retrieval_latitude)))
message(glue("Retrieval latitude Values: {retrieval_latitude_vals}"))
message(glue("Retrieval latitude NAs: {retrieval_latitude_nas}"))

# retrieval_longitude ----------------------------------------------------------
sort_unique_vals(logs$retrieval_longitude)
retrieval_longitude_vals <- count(logs %>% filter(!is.na(retrieval_longitude)))
retrieval_longitude_nas <- count(logs %>% filter(is.na(retrieval_longitude)))
message(glue("Retrieval longitude Values: {retrieval_longitude_vals}"))
message(glue("Retrieval longitude NAs: {retrieval_longitude_nas}"))

# Check for positive values
nonnegative_retrieval_longitude <- logs %>% filter(retrieval_longitude > 0)

# sensor_voltage_deployed ------------------------------------------------------
sort_unique_vals(logs$sensor_voltage_deployed)
sensor_voltage_depl_vals <- count(logs %>% filter(!is.na(sensor_voltage_deployed)))
sensor_voltage_depl_nas <- count(logs %>% filter(is.na(sensor_voltage_deployed)))
message(glue("Sensor Voltage Deployed Values: {sensor_voltage_depl_vals}"))
message(glue("Sensor Voltage Deployed NAs: {sensor_voltage_depl_nas}"))

# sensor_voltage_deployed value to investigate = 354
logs %>% filter(sensor_voltage_deployed == 354)

# sensor_voltage_retrieved -----------------------------------------------------
sort_unique_vals(logs$sensor_voltage_retrieved)
sensor_voltage_retrieved_vals <- count(logs %>% filter(!is.na(sensor_voltage_retrieved)))
sensor_voltage_retrieved_nas <- count(logs %>% filter(is.na(sensor_voltage_retrieved)))
message(glue("Sensor Voltage retrieved Values: {sensor_voltage_retrieved_vals}"))
message(glue("Sensor Voltage retrieved NAs: {sensor_voltage_retrieved_nas}"))

# vessel_sounder_offset_transponder_depth --------------------------------------
sort_unique_vals(logs$vessel_sounder_offset_transponder_depth)
vessel_sounder_offset_vals <- count(logs %>% filter(!is.na(vessel_sounder_offset_transponder_depth)))
vessel_sounder_offset_nas <- count(logs %>% filter(is.na(vessel_sounder_offset_transponder_depth)))
message(glue("Vessel Sounder Offset Values: {vessel_sounder_offset_vals}"))
message(glue("Vessel Sounder Offset NAs: {vessel_sounder_offset_nas}"))

# sounder offset values to investigate
logs %>% filter(vessel_sounder_offset_transponder_depth > 25)

# verified_measurement_below_origin_first_sensor_under_float -------------------
sort_unique_vals(logs$verified_measurement_below_origin_first_sensor_under_float)
first_sensor_under_float_vals <- count(logs %>% filter(!is.na(verified_measurement_below_origin_first_sensor_under_float)))
first_sensor_under_float_nas <- count(logs %>% filter(is.na(verified_measurement_below_origin_first_sensor_under_float)))
message(glue("First Sensor Under Float Measurement Values: {first_sensor_under_float_vals}"))
message(glue("First Sensor Under Float Measurement NAs: {first_sensor_under_float_nas}"))

# first sensor under float measurements to investigate -------------------------
# TODO: What values for this are reasonable? More than 25?
logs %>% filter(verified_measurement_below_origin_first_sensor_under_float > 25)

# tide_correction --------------------------------------------------------------
sort_unique_vals(logs$tide_correction)
tide_correction_vals <- count(logs %>% filter(!is.na(tide_correction)))
tide_correction_nas <- count(logs %>% filter(is.na(tide_correction)))
message(glue("Tide Correction Values: {tide_correction_vals}"))
message(glue("Tide Correction NAs: {tide_correction_nas}"))

# rising_or_falling ------------------------------------------------------------
sort_unique_vals(logs$rising_or_falling)
rising_or_falling_vals <- count(logs %>% filter(!is.na(rising_or_falling)))
rising_or_falling_nas <- count(logs %>% filter(is.na(rising_or_falling)))
message(glue("Rising or Falling Tide Values: {rising_or_falling_vals}"))
message(glue("Rising or Falling Tide NAs: {rising_or_falling_nas}"))

# height_of_vr_2_ar_base_off_bottom --------------------------------------------
sort_unique_vals(logs$height_of_vr_2_ar_base_off_bottom)
height_of_vr2ar_vals <- count(logs %>% filter(!is.na(height_of_vr_2_ar_base_off_bottom)))
height_of_vr2ar_nas <- count(logs %>% filter(is.na(height_of_vr_2_ar_base_off_bottom)))
message(glue("Height of VR2AR Off Bottom Values: {height_of_vr2ar_vals}"))
message(glue("Height of VR2AR Off Bottom NAs: {height_of_vr2ar_nas}"))

# time_of_deployment -----------------------------------------------------------
# TODO: Look at these values more carefully - the unique values are weird for time
sort_unique_vals(logs$time_of_deployment)
time_of_deployment_vals <- logs %>% filter(!is.na(time_of_deployment))
time_of_deployment_nas <- logs %>% filter(is.na(time_of_deployment))

# photos_taken -----------------------------------------------------------------
sort_unique_vals(logs$photos_taken)
# investigate unusual "metal slab" value for photos_taken
#logs %>% filter(photos_taken == "Metal slab")
logs <- logs %>% mutate(photos_taken = tolower(str_sub(photos_taken, 1, 1)))
unique(logs$photos_taken)
# TODO: empty rows to "n"
logs %>% mutate(photos_taken = case_when(is.na(photos_taken) ~ "n"))
sort_unique_vals(logs$photos_taken)

# anchor_type ------------------------------------------------------------------
sort_unique_vals(logs$anchor_type)
logs <- logs %>% mutate(anchor_type = tolower(anchor_type))
sort_unique_vals(logs$anchor_type)
anchor_type_text_len <- unlist(lapply(logs$anchor_type, str_length))
max(anchor_type_text_len, na.rm=TRUE)

# float_type -------------------------------------------------------------------
sort_unique_vals(logs$float_type)
# TODO: investigate "2 big chains plus an anchor", and "Surface"
# might be in wrong column

# distance_from_top_of_float_to_origin_first_sensor
# and distance_from_top_of_float_to_origin_first_sensor_1 ----------------------

# deployment_time -- salmon rivers ---------------------------------------------
sort_unique_vals(logs$deployment_time)
# TODO: Potentially group in with time_of_deployment (see log_compiler.R)

# retrieval_time -- salmon rivers ----------------------------------------------
sort_unique_vals(logs$retrieval_time)

# dist_to_shore -- salmon rivers -----------------------------------------------
sort_unique_vals(logs$dist_to_shore)

# substrate -- salmon rivers ---------------------------------------------------
sort_unique_vals(logs$substrate)


# TODO: Consider whether these separate notes columns are useful i.e. should
# they be standard for CMP?
# deployment_notes -------------------------------------------------------------
sort_unique_vals(logs$deployment_notes)

# retrieval_notes --------------------------------------------------------------
sort_unique_vals(logs$retrieval_notes)

# data_processor_notes ---------------------------------------------------------

# data_handler_notes -----------------------------------------------------------

# depth -- depth of what?? -----------------------------------------------------
# TODO: Check where this comes from?
sort_unique_vals(logs$depth)

# depth_of_water_m -------------------------------------------------------------
# TODO: Check which log this comes from - salmon rivers?
sort_unique_vals(logs$depth_of_water_m)

# secondary_float_type ---------------------------------------------------------
# TODO: Check which log this is coming from - this matches new log format
sort_unique_vals(logs$secondary_float_type)

# "14" -------------------------------------------------------------------------
sort_unique_vals(logs$`14`)
# TODO: delete

# "37" -------------------------------------------------------------------------
sort_unique_vals(logs$`37`)
# TODO: move one existing value into comments section, then delete

