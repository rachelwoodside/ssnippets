library(sensorstrings)
library(dplyr)
library(googlesheets4)
library(lubridate)

##set path --------------------------------------------------------------------------------------------------------------
# First set path to desktop temporary folder run the below code to ensure data download will run with no errors
# If no errors occur, you can delete the downloaded folders in your temporary desktop path, then set the path to the Y drive and re-run the code
path<- "C:/Users/Nicole Torrie/Desktop/Data_Strings"
#path <- "Y:/coastal_monitoring_program/data_branches/water_quality/station_folders"


##to use STRING TRACKING google sheet to set list of files to download---------------------------------------------------
#allow access to the google sheet
googlesheets4::gs4_deauth()

# link to the "STRING TRACKING" google sheet:
link <- "https://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit?usp=sharing"

# read in the "Tracking" tab of the STRING TRACKING sheet
Tracking <- googlesheets4::read_sheet(link, sheet = "Tracking",col_types = "c")

#filter the list of stations which need to be downloaded
to_download <- filter(
  Tracking,
  download_yet == "N",
  status == "Needs Processing"
) %>%
  select(station, deployment)


#Complete the download of multiple files
for (i in seq_along(to_download$station)){
  station_i <- to_download$station[i]
  d_date_i <- as.character(to_download$deployment[i])
  ss_download_data(path, station_i, d_date_i)
}




##to manually set list of files to download---------------------------------------------------------------------------
# station <- c("Flat Island","Birchy Head")
# d_date <- c("2022-11-19","2022-11-03")
# 
#   for (i in seq_along(station)){
#    station_i <- station[i]
#    d_date_i <- as.character(d_date[i])
#    ss_download_data(path, station_i, d_date_i)
#  }


##to download just 1 file---------------------------------------------------------------------------------------------
# ss_download_data(path, "Olding Island", "2022-09-30")

