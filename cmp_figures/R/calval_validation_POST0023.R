# DATE: 2024-10-24
# NAME: NLT
# NOTES: 
# calval version: 0.1.0

# Code to visualize pre and post deployment validation data
# SET UP -----------------------------------------------------------------------
# Create an empty folder called "Log" on the path
# Hobo data must be extracted and placed in a folder on the path called "hobo"
# aquaMeasure data must be extracted and placed in a folder on the path called "aquameasure"
# Vemco data must be extracted and placed in a folder on the path called "vemco"


# Load libraries
library(calval)
library(dplyr)
library(ggview)
library(here)
library(stringr)
library(tidyr)
library(sensorstrings)
library(lubridate)
library(data.table)
library(ggplot2)

# Set the validation ID
VALID <- "POST0023"

## CONSTRUCT METADATA LOG AND DEFINE VARIABLES----------------------------------
# Allow access to the Calibration/Validation Tracking Google sheet
googlesheets4::gs4_deauth()
sheet = ifelse(str_detect(VALID, "^POST"),
               "Post Deployment Validation",
               "Pre Deployment CalVal")
link <-"https://docs.google.com/spreadsheets/d/1u1beyNL02NQvMblhkpGX9tazRqlhfZaJbzifvOKNP54/edit#gid=0"

# Create metadata log from Tracking Google sheet
tracking <- googlesheets4::read_sheet(link, sheet = sheet, col_types = "c") %>%
  filter(`validation event id` == VALID)

log <- create_val_log(tracking) 

# Get a list of variables measured in this validation event
val_var_list <-
  tracking %>%
  # Get the sensor models and modify HOBO DO sensor model into HDO
  distinct(`sensor model`, `validation variable`) %>%
  mutate(`validation variable` = case_when(`sensor model` == "HOBO DO" ~ "HDO",
         .default = `validation variable`)) %>%
  # Get only validation variables
  distinct(`validation variable`) %>%
  pull(`validation variable`)

# Create table of test start/end times for each variable
trimtime_table <- assign_trim_times_all(var_list = val_var_list,
                                        log = log)

# Could probably put this line into assign_trim_times_all()
trimtime_table <- pivot_wider(trimtime_table,
                              names_from = TimeVariable,
                              values_from = DateTime)

# Apply final Log edits for data processing
# TODO: should this be part of the create_val_log function?
cleaned_log <- clean_log_all(log)

## READ IN LOG AND DATA --------------------------------------------------------
raw_val_data <- ss_compile_deployment_data(here("data"))

# General visualization of all test data together
ss_ggplot_variables(raw_val_data)


# FLAGGING TEMP DATA -----------------------------------------------------------
# Filter for just temp variable and temp test time range
temp_data <- filter(raw_val_data, `temperature_degree_c` != "NA") %>%
  filter(
    timestamp_utc > trimtime_table$temp_starttime_utc &
      timestamp_utc < trimtime_table$temp_endtime_utc
  )

# Manually adjust temp start or end time after viewing data if necessary
# temp_starttime_utc <- as_datetime("2024-05-13 14:00:00", tz = "UTC")
# temp_endtime_utc <- as_datetime("2023-11-16 13:40:00", tz = "UTC")

# Create new column rounding timestamps to nearest 15 minutes and calculate 
#   median value for each time grouping. Then populate the flag column
# TODO: Can any of this code be more "behind the scenes" to reduce script?
#       Maybe a function so you can still set the rounding interval and accuracy?
temp_data <- temp_data %>%
  mutate(rounddate = round_date(timestamp_utc, unit = "15 minutes"))

medians <- temp_data %>%
  group_by(rounddate) %>%
  summarise_at(vars(temperature_degree_c), list(median = median))

final_temp <-
  merge(temp_data, medians, by = "rounddate", all.x = TRUE) %>%
  mutate(accuracy = if_else(sensor_type == "vr2ar", 0.5, 0.2)) %>%
  mutate(
    Result = case_when(
      temperature_degree_c > median + accuracy |
        temperature_degree_c < median - accuracy ~ "Pass",
      TRUE ~ "Fail"
    )
  )


ggplot(final_temp, 
       aes(x = timestamp_utc, y = temperature_degree_c, 
           color = Result)) + 
  geom_point(size = 1) + 
  scale_color_manual(values = c("#082735", "#1E9FD4")) + 
  # geom_line(aes(y = median), color = "black") + 
  geom_ribbon(
    aes(ymin = median - 0.4, ymax = median + 0.4), 
    alpha = 0.3, color = "grey87"
  ) +
  theme(
    #legend.position = c(0.7, 0.7),
    legend.position = "none",
    text = element_text(size = 8)
  ) +
  canvas(width = 6, height = 4.5, units = "cm")

ggsave(
  
  filename = here("figures/2024-10-28_val.png"),
  device = "png",
  width = 6, height = 4.5, units = "cm",
  dpi = 600
)



# Plot final_temp colorized by flag (0 = pass)
r <- ggplot_temp_flag(final_temp, point_size = 1.5)
r

# Plot final_temp colorized by sensor
s <- ggplot_temp_val(final_temp, point_size = 1.5)
s

