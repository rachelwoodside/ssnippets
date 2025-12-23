library(dplyr)
library(sensorstrings)


dat_raw <- ss_import_data(county = "Guysborough") %>%
  select(-contains("flag"))

dat <- dat_raw %>%
  ss_pivot_longer()

nrow(dat)
