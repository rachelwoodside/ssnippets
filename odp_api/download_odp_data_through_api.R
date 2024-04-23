library(RSocrata)

annapolis_wq_api_url <- "https://data.novascotia.ca/resource/knwz-4bap.csv"
annapolis_wq_data <- read.socrata(annapolis_wq_api_url)
