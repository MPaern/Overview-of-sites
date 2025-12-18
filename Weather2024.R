## Weather data


# setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(MetBrewer)
library(hms)
library(zoo)
library(circular)


# data --------------------------------------------------------------------

weather <- read.csv("WeatherData2024.csv")
overview_summary <- read.csv("overview_2024.csv")

  # fix weather
  weather$X <- NULL
  weather$Unnamed..0 <- NULL
  weather$Unnamed..0.1 <- NULL
  
  summary(weather)
  
  weather <- na.omit(weather)
  
  summary(weather)

# interpolation of data --------------------------------------------------------------------

  # for temperature, pressure, cloud area, humidity and wind speed, I'll use linear interpolation
  
  # create a column with datetime format
  
  weather <- weather %>%
    mutate(datetime = make_datetime(
      year = year,  
      month = month,
      day = day,
      hour = hour
    ))
  
  missing_time <- make_datetime(2024, 8, 20, 22)
  
  weather_full <- weather %>%
    group_by(name) %>%
    tidyr::complete(datetime = c(datetime, missing_time)) %>%
    ungroup()
  
  linear_vars <- c(
    "air_temperature_2m",
    "air_pressure_at_sea_level",
    "cloud_area_fraction",
    "relative_humidity_2m",
    "wind_speed_10m"
  )
  
  
  weather_full <- weather_full %>%
    group_by(name) %>%
    arrange(datetime) %>%
    mutate(across(
      all_of(linear_vars),
      ~ na.approx(., x = datetime, na.rm = FALSE)
    ))
  
  # for wind direction, I'll use circular interpolation
  
  weather_full <- weather_full %>%
    mutate(
      wd_rad = wind_direction_10m * pi / 180,
      u = sin(wd_rad),
      v = cos(wd_rad)
    )
  
  weather_full <- weather_full %>%
    group_by(name) %>%
    arrange(datetime) %>%
    mutate(across(
      c(u, v),
      ~ zoo::na.approx(., x = as.numeric(datetime), na.rm = FALSE)
    )) %>%
    ungroup()
  
  weather_full <- weather_full %>%
    mutate(
      wind_direction_10m =
        (atan2(u, v) * 180 / pi) %% 360
    ) %>%
    select(-wd_rad, -u, -v)
  
  # precipitation I'll assess based on the values before/after. It seems that the rain stopped before that timepoint, I'll add 0 to everything
  
  weather_full <- weather_full %>%
    mutate(
      precipitation_amount = if_else(
        is.na(precipitation_amount),
        0,
        precipitation_amount
      )
    )
  
  # fill other datarows that have NA-s
  
  weather_full <- weather_full %>%
    mutate(
      year  = year(datetime),
      month = month(datetime),
      day   = day(datetime),
      hour  = hour(datetime)
    )
  
  site_vars <- c("location_latitude", "location_longitude", "nearest_latitude", "nearest_longitude")
  
  weather_full <- weather_full %>%
    group_by(name) %>%
    fill(all_of(site_vars), .direction = "downup") %>%
    ungroup()

# temperature -------------------------------------------------------------

  weather_full_ns <- weather_full %>%
    arrange(location_latitude) %>%
    mutate(
      name = factor(name, levels = unique(name))
    )
  
  ggplot(weather_full_ns, aes(x = name, y = air_temperature_2m)) +
    geom_boxplot(outlier.alpha = 0.3) +
    theme_bw() +
    labs(x = "Site", y = "Temperature (°C)") +
    theme(axis.text.x = element_text(angle = 90))+
    coord_flip()
  
  
  temp_summary <- weather_full %>%
    group_by(name) %>%
    summarise(
      mean_temp = mean(air_temperature_2m, na.rm = TRUE),
      sd_temp   = sd(air_temperature_2m, na.rm = TRUE)
    )
  
  library(ggrepel)
  
  ggplot(temp_summary, aes(mean_temp, sd_temp, label = name)) +
    geom_point() +
    geom_text_repel(size = 3, max.overlaps = 50) +
    theme_bw() +
    labs(
      x = "Mean temperature",
      y = "Temperature SD"
    )
  
  

# wind --------------------------------------------------------------------
   
  wind_summary <- weather_full %>%
    group_by(name) %>%
    summarise(
      max_wind = max(wind_speed_10m, na.rm = TRUE),
      p95_wind = quantile(wind_speed_10m, 0.95, na.rm = TRUE)
    )
  
  ggplot(wind_summary, aes(x = name, y = max_wind)) +
    geom_col() +
    theme_bw() +
    labs(y = "Max wind speed (m/s)") +
    theme(axis.text.x = element_text(angle = 90))

# looking around ----------------------------------------------------------

  ggplot(weather_full_ns,
         aes(location_longitude, location_latitude, color = air_temperature_2m)) +
    geom_point() +
    coord_equal() +
    theme_bw()
  
  ggplot(weather_full, aes(cloud_area_fraction)) +
    geom_histogram(bins = 30) +
    theme_bw()
  
  library(openair)
  
  windRose(
    weather_full,
    ws = "wind_speed_10m",
    wd = "wind_direction_10m",
    facet = "name",
    ncol = 5
  )
  
  windRose(
    weather_full,
    ws = "wind_speed_10m",
    wd = "wind_direction_10m",
    facet = "name",
    ncol = 5,
    angle = 30
  )
  
  windRose(
    subset(weather_full, month %in% c(6, 7, 8)),
    ws = "wind_speed_10m",
    wd = "wind_direction_10m",
    facet = "name",
    ncol = 5
  )
  
  windRose(
    subset(weather_full, month %in% c(9, 10)),
    ws = "wind_speed_10m",
    wd = "wind_direction_10m",
    facet = "name",
    ncol = 5
  )
  
  wind_dir_summary <- weather_full %>%
    group_by(name) %>%
    summarise(
      mean_dir = as.numeric(
        mean(circular(wind_direction_10m,
                      units = "degrees",
                      modulo = "2pi"),
             na.rm = TRUE)
      )
    )
  
  ggplot(wind_summary,
         aes(x = name, y = 0,
             xend = name,
             yend = mean_speed)) +
    geom_segment(
      arrow = arrow(length = unit(0.25, "cm")),
      linewidth = 0.8
    ) +
    coord_flip() +
    theme_bw() +
    labs(
      y = "Mean wind vector magnitude",
      x = "Site",
      title = "Dominant wind direction and strength by site"
    )
  )

ggplot(wind_summary,
       aes(x = location_longitude,
           y = location_latitude)) +
  geom_segment(
    aes(
      xend = location_longitude + u_bar * 0.2,
      yend = location_latitude  + v_bar * 0.2
    ),
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.8
  ) +
  coord_equal() +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Dominant wind direction by site"
  )

wind_uv <- weather_full %>%
  mutate(
    u = sin(wind_direction_10m * pi / 180) * wind_speed_10m,
    v = cos(wind_direction_10m * pi / 180) * wind_speed_10m
  ) %>%
  group_by(name) %>%
  summarise(
    u_bar = mean(u, na.rm = TRUE),
    v_bar = mean(v, na.rm = TRUE)
  ) %>%
  mutate(
    mean_speed = sqrt(u_bar^2 + v_bar^2),
    mean_dir   = (atan2(u_bar, v_bar) * 180 / pi) %% 360
  )

ggplot(wind_uv,
       aes(x = name, y = 0,
           xend = name,
           yend = mean_speed)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm"))) +
  coord_flip()
