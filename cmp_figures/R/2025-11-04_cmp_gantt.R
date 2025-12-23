library(dplyr)
library(ggplot2)
library(lubridate)
library(sensorstrings)
library(tgc)

theme_set(theme_light())

dat_raw <- ss_import_data(county = "digby")

dat <- dat_raw %>%
  select(
    station, sensor_depth_at_low_tide_m, timestamp_utc, temperature_degree_c
  ) %>%
  filter(!is.na(temperature_degree_c)) %>%
  group_by(station, sensor_depth_at_low_tide_m) %>%
  mutate(
    depl_start = as_date(min(timestamp_utc)),
    depl_end = as_date(max(timestamp_utc))
  ) %>%
  ungroup() #%>%
  filter(station == "Long Island 2")


dat_end <- dat %>%
  distinct(station, sensor_depth_at_low_tide_m, depl_end) %>%
  rename(end_date = depl_end)


dat_gap <- dat %>%
  rename(DEPTH = sensor_depth_at_low_tide_m, TIMESTAMP = timestamp_utc) %>%
  check_for_data_gaps(
    gap_length = 72, gap_warning = 48, station, depl_start, depl_end
  ) %>%
  rename(
    sensor_depth_at_low_tide_m = DEPTH,
    end_date = GAP_START, # date data ends; start of gap
    gap_length_days = GAP_LENGTH_DAYS,
    gap_length_hours = GAP_LENGTH_HOURS
  ) %>%
  bind_rows(dat_end) %>%
  group_by(station, sensor_depth_at_low_tide_m) %>%
  mutate(
    row_id = 1:n(),

    start_date = if_else(
      row_id == 1, depl_start, lag(end_date) + lag(days(round(gap_length_days)))
    ),
    end_date = if_else(
      is.na(end_date) & row_id == 1 & gap_length_days == 0, depl_end, end_date
    ),

    text = paste0(
      "sensor_depth: ", sensor_depth_at_low_tide_m
    )
  ) %>%
  filter(!is.na(start_date)) %>%  # duplication rows for stations without any gaps
  arrange(station, sensor_depth_at_low_tide_m, start_date) %>%
  select(station, sensor_depth_at_low_tide_m, start_date, end_date) %>%
  ss_convert_depth_to_ordered_factor()


p <- ggplot(dat_gap, aes(col = sensor_depth_at_low_tide_m, text)) +
  geom_segment(
    aes(
      x = start_date, xend = end_date,
      y = sensor_depth_at_low_tide_m, yend = sensor_depth_at_low_tide_m,

      text = paste(
        "station: ", station, "\n",
        "depth_m: ", sensor_depth_at_low_tide_m, "\n",
        "start_date: ", as_date(start_date), "\n",
        "end_date: ", as_date(end_date)
      )
    ), linewidth = 4
  ) +
  scale_y_discrete(
    "Depth (m)",
    limits = rev(levels(dat_gap$sensor_depth_at_low_tide_m))
  ) +
  scale_colour_manual("Depth (m)", values = ss_get_colour_palette(dat)) +
  theme(
    axis.title.x = element_blank(),
  ) +
  facet_wrap(~station)


library(plotly)
ggplotly(p, tooltip = "text")
