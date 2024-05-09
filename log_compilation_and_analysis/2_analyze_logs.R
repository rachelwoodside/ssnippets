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
library(tidyr)
library(lubridate)

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

# TODO: Add in option to read in most recent stacked log copy

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
logs <-
  logs %>% mutate(
    configuration = case_when(
      mount_type == "cinder block" |
        mount_type == "cinderblock" ~ "attached to fixed structure",
      .default = configuration
    )
  )

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

logs <-
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

logs <-
  left_join(logs,
            cb_config_table_data,
            by = c("location_description", "deployment"))

colnames(logs)

# Check if any configuration data is not filled by the logs or the config table
missing_any_config <- config_table_join_data %>%
  filter(is.na(config_table_join_data$log_configuration) &
           is.na(config_table_join_data$table_configuration) &
           is.na(config_table_join_data$cb_table_configuration)) %>%
  select(location_description, deployment, log_configuration, table_configuration, cb_table_configuration)

# TODO? Consider using right join to check for entries in the config table
# that are missing from the logs

# Check if any columns have more than one configuration column filled in
logs <-
  logs %>% mutate(config_count = (
    as.numeric(!is.na(log_configuration)) + 
      as.numeric(!is.na(table_configuration)) + 
      as.numeric(!is.na(cb_table_configuration))
  ))

duplicate_config_logs <- logs %>% filter(config_count > 1) %>% select(location_description, deployment, log_configuration, table_configuration, cb_table_configuration)

# Confirm that for columns with more than one configuration recorded, 
# the records match
# Since we only have two of the columns filled for any given row, this simple
# check works. Something more would be necessary for checking across all three 
# columns while discounting NA values
duplicate_config_logs %>% filter(
  !(log_configuration == table_configuration |
    log_configuration == cb_table_configuration |
    table_configuration == cb_table_configuration)
)

# Merge configuration data into a single column
# Since the duplicate config entry values all match, we can simply take the 
# first value that is not NA out of all of the columns
logs <-
  logs %>% mutate(configuration = case_when(!is.na(log_configuration) ~ log_configuration, # prioritize log configuration
                                             !is.na(table_configuration) ~ table_configuration, # then config table
                                             !is.na(cb_table_configuration) ~ cb_table_configuration, # then CB config table
                                             .default = NA)  
  )

# Check for any final NAs
logs %>% filter(is.na(configuration)) %>% distinct(location_description, deployment)
# NOTE: there are some in the dataset as of 2024-05-09 
# due to some required updates to the logs and some partially implemented
# station name changes

# Clean up temporary columns
colnames(logs)
logs <- logs %>% select(!c(log_configuration, table_configuration, cb_table_configuration, config_count))
colnames(logs)

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

# deployment_attendant and retrieval_attendant cleanup prep --------------------
# Examine all attendant values
all_attendant_vals <- sort(unique(c(logs$deployment_attendant, logs$retrieval_attendant)))
all_attendant_vals <-
  all_attendant_vals %>%
  lapply(str_split_1, pattern = ",| & | &|/|(and)") %>%
  unlist %>%
  str_remove(pattern = "\\.") %>%
  trimws %>%
  sort_unique_vals
all_attendant_vals

# Define alternative naming identified in manual examination of attendant vals
albert_spears_alts <- c("Albert")
betty_roethlisberger_alts <-
  c("betty",
    "Betty",
    "Betty Roethlsiberger",
    "Betty Roethsisberger")
brett_savoury_alts <- c("Brett", "Brett Savoury", "Bretty Savoury")
david_burns_alts <- c("D Burns")
danny_rowe_alts <-
  c("Dan Rowe", "danny", "Danny", "Danny R", "DannyR")
david_cook_alts <- c("D Cook")
innovative_fisheries_alts <- c("Innovative crew")
jamie_warford_alts <- c("Jaime Warford")
josh_hatt_alts <- c("J Hatt")
kate_richardson_alts <- c("K Richardson")
kiersten_watson_alts <-
  c("Kiersten", "Kiersten (string didn't surface for Leeway)")
leeway_alts <-
  c(
    "leeway crew",
    "Leeway crew",
    "Leeway Crew",
    "leeway marine",
    "Leeway marine",
    "Leeway Marine Crew"
  )
matthew_hatcher_alts <- c("M Hatcher", "Matt Hatcher", "MHatcher")
mark_decker_alts <- c("mark", "Mark")
michelle_plamondon_alts <- c("michelle", "Michelle")
scott_hatcher_alts <- c("S Hatcher", "SHatcher")
timothy_dada_alts <- c("Tim D", "Tim Dada", "Timpthy Dada")
toby_balch_alts <- c("T Balch")
todd_mosher_alts <- c("T Mosher")
will_rowe_alts <- c("will", "Will")

# Define helper functions

# Replace values by matching correct naming to alternative names
fix_attendant_val <- function(attendant_val) {
  attendant_val = case_when(
    attendant_val %in% albert_spears_alts ~ "Albert Spears",
    attendant_val %in% betty_roethlisberger_alts ~ "Betty Roethlisberger",
    attendant_val %in% brett_savoury_alts ~ "Brett Savoury",
    attendant_val %in% david_burns_alts ~ "David Burns",
    attendant_val %in% danny_rowe_alts ~ "Danny Rowe",
    attendant_val %in% david_cook_alts ~ "David Cook",
    attendant_val %in% innovative_fisheries_alts ~ "Innovative Fisheries",
    attendant_val %in% jamie_warford_alts ~ "Jamie Warford",
    attendant_val %in% josh_hatt_alts ~ "Josh Hatt",
    attendant_val %in% kate_richardson_alts ~ "Kate Richardson",
    attendant_val %in% kiersten_watson_alts ~ "Kiersten Watson",
    attendant_val %in% leeway_alts ~ "Leeway Marine",
    attendant_val %in% matthew_hatcher_alts ~ "Matthew Hatcher",
    attendant_val %in% mark_decker_alts ~ "Mark Decker",
    attendant_val %in% michelle_plamondon_alts ~ "Michelle Plamondon",
    attendant_val %in% scott_hatcher_alts ~ "Scott Hatcher",
    attendant_val %in% timothy_dada_alts ~ "Timothy Dada",
    attendant_val %in% toby_balch_alts ~ "Toby Balch",
    attendant_val %in% todd_mosher_alts ~ "Todd Mosher",
    attendant_val %in% will_rowe_alts ~ "Will Rowe",
    .default = attendant_val
  )
}

# Set all delimiters to commas
standardize_delimiter <- function(attendant_str) {
  punctuation_replacement_regex <-
    regex(
      "[:blank:]+&[:blank:]*|[:blank:]*/[:blank:]*|[:blank:]*and[:blank:]*|[:blank:]*,[:blank:]*"
    )
  attendant_str %>%
    str_remove_all("\\.") %>%
    str_replace_all(pattern = punctuation_replacement_regex, replacement = ",")
}

# TODO? Add function to alphabetically sort attendant names within column?
# Looked into this initially but seemed overcomplicated compared to the value
# added

# deployment_attendant ---------------------------------------------------------
# sort_unique_vals(logs$deployment_attendant)
# depl_att_vals <- count(logs %>% filter(!is.na(deployment_attendant)))
# depl_att_nas <- count(logs %>% filter(is.na(deployment_attendant)))
# message(glue("Deployment Attendant Values: {depl_att_vals}"))
# message(glue("Deployment Attendant NAs: {depl_att_nas}"))

# Clean up deployment attendant column
logs <- logs %>%
  # Standardize delimiter
  mutate(deployment_attendant_sep = 
           standardize_delimiter(deployment_attendant)) %>%
  # Separate into columns for each individual attendant
  separate_wider_delim(
    deployment_attendant_sep,
    delim = ",",
    names_sep = "_",
    too_few = "align_start"
  ) %>%
  # Replace invalid values according to mapping defined previously
  mutate(across(contains("deployment_attendant_sep"), fix_attendant_str)) %>%
  # Remove empty strings
  mutate(across(contains("deployment_attendant_sep"), na_if, "")) %>%
  # Reunite separate columns and remove temporary columns
  unite(
    col = "deployment_attendant",
    contains("deployment_attendant_sep"),
    sep = ", ",
    remove = TRUE,
    na.rm = TRUE
  )

sort_unique_vals(logs$deployment_attendant)
  
# Check deployment_attendant text length
sort_unique_vals(logs$deployment_attendant)
depl_attendant_text_len <-
  unlist(lapply(logs$deployment_attendant, str_length))
max(depl_attendant_text_len, na.rm = TRUE)

# retrieval_attendant ----------------------------------------------------------
sort_unique_vals(logs$retrieval_attendant)
# Retrieval attendant as "Line 3, East end"?
#logs %>% filter(retrieval_attendant == "Line 3, East end")
# retrieval_att_vals <-
#   count(logs %>% filter(!is.na(retrieval_attendant)))
# retrieval_att_nas <-
#   count(logs %>% filter(is.na(retrieval_attendant)))
# message(glue("Retrieval Attendant Values: {retrieval_att_vals}"))
# message(glue("Retrieval Attendant NAs: {retrieval_att_nas}"))

# Clean up retrieval attendant column
logs <- logs %>%
  # Standardize delimiter
  mutate(retrieval_attendant_sep = standardize_delimiter(retrieval_attendant)) %>%
  # Separate into columns for each individual attendant
  separate_wider_delim(
    retrieval_attendant_sep,
    delim = ",",
    names_sep = "_",
    too_few = "align_start"
  ) %>%
  # Replace invalid values according to mapping defined previously
  mutate(across(contains("retrieval_attendant_sep"), fix_attendant_str)) %>%
  # Remove empty strings
  mutate(across(contains("retrieval_attendant_sep"), na_if, "")) %>%
  # Reunite separate columns and remove temporary columns
  unite(
    col = "retrieval_attendant",
    contains("retrieval_attendant_sep"),
    sep = ", ",
    remove = TRUE,
    na.rm = TRUE
  )

sort_unique_vals(logs$retrieval_attendant)

# Check retrieval_attendant text length
sort_unique_vals(logs$retrieval_attendant)
retrieval_attendant_text_len <-
  unlist(lapply(logs$retrieval_attendant, str_length))
max(retrieval_attendant_text_len, na.rm = TRUE)

# comments ---------------------------------------------------------------------
# each of these is likely unique
# TODO? Check for unusual values, like numerics
comment_vals <- count(logs %>% filter(!is.na(comments)))
comment_nas <- count(logs %>% filter(is.na(comments)))
message(glue("Comment Values: {comment_vals}"))
message(glue("Comment NAs: {comment_nas}"))
sort_unique_vals(logs$comments)
sort_unique_vals(logs$deployment_notes)
sort_unique_vals(logs$retrieval_notes)
sort_unique_vals(logs$data_handler_notes)
sort_unique_vals(logs$data_processor_notes)


#deployment_notes: All from salmon fieldwork. Append "deployment:" before each
logs <- logs %>%
  mutate(deployment_notes = if_else(
    !is.na(deployment_notes),
    paste0("Deployment: ", deployment_notes),
    NA))

sort_unique_vals(logs$deployment_notes)

#retrieval_notes: All from salmon fieldwork. Append "retrieval: " before each
logs <- logs %>%
  mutate(retrieval_notes = if_else(
    !is.na(retrieval_notes),
    paste0("Retrieval: ", retrieval_notes),
    NA))

sort_unique_vals(logs$retrieval_notes)

#data_handler_notes. Append "Data processor note: " before each
logs <- logs %>%
  mutate(data_handler_notes = if_else(
    !is.na(data_handler_notes),
    paste0("Data processor note: ", data_handler_notes),
    NA))

sort_unique_vals(logs$data_handler_notes)

#data_processor_notes. Append "Data processor note: " before each
logs <- logs %>%
  mutate(data_processor_notes = if_else(
    !is.na(data_processor_notes),
    paste0("Data processor note: ", data_processor_notes),
    NA))

sort_unique_vals(logs$data_processor_notes)

#combine comments, data_processor_notes, deployment_notes, retrieval_notes,
#and data_handler_notes columns into one 'notes' column
logs <- logs %>%
  unite(col = "notes",
        c(comments, deployment_notes, retrieval_notes, data_processor_notes, 
          data_handler_notes), 
        sep = ". ",
        na.rm = TRUE)
sort_unique_vals(logs$notes)

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

# Define alternative naming groups
rising_synonyms <-
  c("+", "low rising", "mid-high rising", "Mid, rising", "rising", "RISING",
    "Mid tide, rising", "Rising")

falling_synonyms <-
  c("-", "Dropping", "Falling", "High, falling", "falling", "High tide, falling")

slack_high_synonyms <-
  c("high", "High tide", "neutral high", "High", "high, turning", "slack_high")

slack_low_synonyms <- c("Low", "low", "Falling (but almost back to rising)",
                        "Low neutral", "neutral low", "slack_low")

cannot_evaluate_tide_dir <- c("0.55m", "Mid", "Neutral")

logs <-
  logs %>% mutate(
    rising_or_falling = case_when(
      rising_or_falling %in% rising_synonyms ~ "rising",
      rising_or_falling %in% falling_synonyms ~ "falling",
      rising_or_falling %in% slack_high_synonyms ~ "slack_high",
      rising_or_falling %in% slack_low_synonyms ~ "slack_low",
      rising_or_falling %in% cannot_evaluate_tide_dir ~ NA,
      .default = rising_or_falling
    )
  )
sort_unique_vals(logs$rising_or_falling)

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

