# July 17, 2023
# microbenchmark from the office

library(dplyr)
library(microbenchmark)
library(purrr)

# desktop
x_path <- file.path("C:/Users/Danielle Dempsey/Desktop/RProjects/thresholds_analysis/data-raw")

# perennia shared drive (perennia internet)
y_path <- file.path("Y:/coastal_monitoring_program/benchmark_test/data-raw")

# server (perennia internet)
z_path <- file.path("R:/data-raw")

files = "rds"

# list files --------------------------------------------------------------

microbenchmark(
  x_files <- list.files(x_path, full.names = TRUE, pattern = files),
  
  y_files <- list.files(y_path, full.names = TRUE, pattern = files),
  
  z_files <- list.files(z_path, full.names = TRUE, pattern = files)
)

# results for Annapolis data
# Unit: microseconds
#           min        lq       mean
# x_files 378.9     451.1         554.952
# y_files 205194.3  207077.0  211860.207
# z_files 620.2     712.5       1434.706
# median       uq      max     neval
# 507.2     610.3     1122.9     100
# 208177.5  210419.3  473511.6   100
# 769.2     890.7     27276.9    100

# results for all data --> Perennia drive is 330 times slower than desktop
# Unit: microseconds
# expr      min        lq        mean      median
# x_files  454.5    520.15         640.713     558.0
# y_files  205476.3 207357.45   212373.599  208737.5
# z_files  697.8    786.65        1259.829    841.5
# uq          max     neval
# 685.50     1569.2    100
# 210265.30  464931.5  100
# 998.05     14660.4   100


# read in files  --------------------------------------------------------

microbenchmark(

  x <- purrr::map_dfr(x_files, readRDS),
  
  y <- purrr::map_dfr(y_files, readRDS),
  
  z <- purrr::map_dfr(z_files, readRDS)
)

# read in Annapolis data
# Unit: milliseconds
# expr       min       lq      mean      median       uq      max       neval
# x         890.6219 1031.684  1164.937  1098.983  1179.063  2606.448   100
# y         013.4597 1151.528  1332.764  1225.616  1347.766  3368.060   100
# z         930.8055 1033.683  1203.225  1117.283  1218.849  2997.096   100

# all data

# list and read -----------------------------------------------------------

microbenchmark(
  
  x <- list.files(x_path, full.names = TRUE, pattern = files) %>% 
    purrr::map_dfr(readRDS),
  
  y <- list.files(y_path, full.names = TRUE, pattern = files) %>% 
    purrr::map_dfr(readRDS),
  
  z <- list.files(z_path, full.names = TRUE, pattern = files) %>% 
    purrr::map_dfr(readRDS),
  
  times = 10
)

# Annapolis
# Unit: milliseconds
# expr       min       lq     mean
# x        926.1942 1013.284 1158.181
# y       1115.3225 1282.856 1483.003
# z        901.4247 1009.495 1185.200
# median       uq      max     neval
# 1117.087 1217.686 1833.588   100
# 1397.510 1619.958 2653.076   100
# 1127.803 1250.147 2341.651   100

# All Data
# Unit: seconds
# expr       min        lq
# x        95.03421  95.37343
# y       145.72167 150.60874
# z        99.39078 102.08128
# mean   median       uq      max      neval
# 104.4478 105.2098 108.7127 118.0380    10
# 171.8076 168.9832 186.6716 207.6058    10
# 111.2022 109.6005 117.9216 130.1276    10
