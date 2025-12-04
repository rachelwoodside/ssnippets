library(RSocrata)

# SODA2 API
annapolis_wq_api_url <- "https://data.novascotia.ca/resource/knwz-4bap.csv"
annapolis_wq_data <- read.socrata(annapolis_wq_api_url)

# SODA3 API
# !!NOTE: to match the SODA3 API, the API endpoint provided by the ODP must be modified
# For example, the ODP provides the endpoint: "https://data.novascotia.ca/api/v3/views/ktyz-sxuu/query.json"
# However the "query.json" portion must be removed to be considered a valid URL by the RSocrata package

# Public dataset example
# Pull in app token from config.yml
odp_api_config <- config::get(config = "default", file = "odp_api/config.yml")
# Read in dataset from API
annapolis_wq_api_url <- "https://data.novascotia.ca/api/v3/views/knwz-4bap/"
annapolis_wq_data <- read.socrata(
  annapolis_wq_api_url,
  app_token = odp_api_config$app_token
)

# Private dataset example
# Pull in login info and app token from config.yml
odp_api_config <- config::get(config = "default", file = "odp_api/config.yml")
# Read in dataset from API
wq_metrics_url <- "https://data.novascotia.ca/api/v3/views/ktyz-sxuu/"
wq_metrics_data <- read.socrata(
  wq_metrics_url,
  app_token = odp_api_config$app_token,
  email = odp_api_config$email,
  password = odp_api_config$password
)
