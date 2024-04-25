# July 17, 2023
# microbenchmark from home (on vpn)

library(dplyr)
library(microbenchmark)
library(purrr)

# desktop
x_path <- file.path("C:/Users/Danielle Dempsey/Desktop/RProjects/thresholds_analysis/data-raw")

# perennia shared drive (perennia internet)
y_path <- file.path("Y:/coastal_monitoring_program/benchmark_test/data-raw")

# server (perennia internet)
z_path <- file.path("R:/data-raw")

files = "Annapolis"

# list files --------------------------------------------------------------

microbenchmark(
  x_files <- list.files(x_path, full.names = TRUE, pattern = files),
  
  y_files <- list.files(y_path, full.names = TRUE, pattern = files),
  
  z_files <- list.files(z_path, full.names = TRUE, pattern = files)
)

# Annapolis - from wifi
# Unit: microseconds
# expr     min       lq      mean   median
# x_files 494.1   786.25   823.284   829.45
# y_files 46292.4 55095.50 66496.670 61687.15
# z_files 641.4   933.50   6048.382  1024.95
# uq      max neval
# 883.0   1330.4   100
# 68117.1 400466.4   100
# 1052.2 362628.1   100


# Annapolis - ethernet
# Unit: microseconds
# expr     min      lq      mean   median
# x_files 373.2   465.5   552.342   516.05
# y_files 26385.9 28177.5 29687.209 28883.60
# z_files 615.5   716.2   795.177   764.90
# uq     max neval
# 601.05  1269.8   100
# 29186.40 73781.6   100
# 854.85  1265.2   100


# results for all data - from wifi
# Unit: microseconds
# expr     min       lq       mean       median
# x_files  516.6    960.45    1340.894   1251.80
# y_files  51324.8  58015.45  69343.381  64178.00
# z_files  695.8    1145.75    6186.034  1797.65
# uq         max      neval
# 1751.25   2084.2     100
# 70408.65  233939.5   100
# 2028.85   291125.0   100

# # data - ethernet
# Unit: microseconds
# expr     min       lq      mean     median
# x_files 389.7   498.95   605.171    561.55
# y_files 25421.1 28030.10 30835.808  28728.00
# z_files 637.2   786.20   5293.986   837.25
# uq       max        neval
# 662.70   1321.7     100
# 29584.15 194542.7   100
# 946.95   312813.4   100


# read in files  --------------------------------------------------------

microbenchmark(

  x <- purrr::map_dfr(x_files, readRDS),
  
  y <- purrr::map_dfr(y_files, readRDS),
  
  z <- purrr::map_dfr(z_files, readRDS)
)

# Annapolis - wifi
# Unit: milliseconds
# expr      min        lq     mean    median       uq      max    neval
# x      787.0112  872.4484 1028.794  932.6866 1034.116 2482.483   100
# y      840.8600  928.5559 1074.012 1017.4246 1102.883 1964.954   100
# z      810.6931 1021.1021 1131.937 1106.1818 1181.600 2181.582   100

# Annapolis - ethernet
# Unit: milliseconds
# expr      min       lq      mean      median       uq      max   neval
# x        943.4575 1034.908 1130.774 1118.967 1164.909 1702.145   100
# y        949.8108 1013.127 1134.110 1130.245 1208.076 1615.849   100
# z        929.0575 1141.932 1255.304 1225.466 1332.940 2282.039   100


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
# expr      min
# x      953.0824
# y      992.9103
# z      932.7443
# lq         mean   median       uq      max     neval
# 1022.679 1105.900 1090.663 1160.884 2094.530   100
# 1108.365 1204.163 1178.704 1254.933 2204.241   100
# 1085.505 1210.663 1167.464 1298.906 1981.275   100


# All data
# Unit: seconds
# expr       min        lq      mean    median
# x       90.91976  91.15024  93.25005  92.10725
# y      104.15422 104.79370 105.64007 105.42001
# z      122.30901 124.14091 124.57965 124.72218
# uq         max      neval
# 93.92738  102.3140    10
# 105.76279 107.9826    10
# 125.19365 126.3937    10
