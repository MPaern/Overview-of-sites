# Find out if data is cohesive

# If you have duplicates
# if you have all the weather data and if it's only for the times I need
# What's the timeframe of the recordings
# What's the timezone of the data?
# where are the NA-s?
# Do you have all the dates/times
# Are the missing dates = dates that the detector was on, other days are missing because the detector was off.
# Find mistakes in autoid
# Make a dataset with 1 h before and 1 h after data
# Make dataset with daytime data (between sunrise and sunset)

# read data in ---------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(MetBrewer)
library(hms)
library(zoo)
library(circular)
library(ggrepel)
library(openair)
library(suncalc)
library(quarto)

df <- read.csv("cm_all_2024_v2.csv")


# initial check---------------------------------

df[1] <- NULL
str(df) 
head(df)

summary(df$AUTO.ID.) 

df <- df %>% 
  mutate(AUTO.ID. = factor(AUTO.ID.), 
         Site = factor(Site))

summary(df$AUTO.ID.) 

colSums(is.na(df))

#check for duplicates

any(duplicated(df$OUT.FILE.FS)) # TRUE

unique(df$INDIR)
unique(df$OUTDIR)

na_rows <- df %>% filter(is.na(OUT.FILE.FS)) # 1214

# make IN.FILE as file name OUT.FILE.FS

df <- df %>% 
  mutate(OUT.FILE.FS = coalesce(OUT.FILE.FS, IN.FILE))

na_rows <- df %>% filter(is.na(OUT.FILE.FS)) # now it's 0

duplicates <- df %>%
  group_by(OUT.FILE.FS) %>%
  filter(n() > 1) %>%
  ungroup() # 120590 - because Noise has same name if it comes from same file

# rename the ones with "Noise", add "_n"

df_v1 <- df %>%
  group_by(OUT.FILE.FS) %>%
  mutate(
    noise_num = cumsum(AUTO.ID. == "Noise"),
    OUT.FILE.FS = ifelse(
      n() > 1 & AUTO.ID. == "Noise",
      paste0(sub("\\.wav$", "", OUT.FILE.FS),
             "_n", noise_num, ".wav"),
      OUT.FILE.FS
    )
  ) %>%
  ungroup() %>%
  select(-noise_num)

# do I still have duplicates?

duplicates_v1 <- df_v1 %>%
  group_by(OUT.FILE.FS) %>%
  filter(n() > 1) %>%
  ungroup() # 89970

# make new data set with only distinct rows

df_v2 <- df_v1 %>%
  distinct()

# duplicates

duplicates_v2 <- df_v2 %>%
  group_by(OUT.FILE.FS) %>%
  filter(n() > 1) %>%
  ungroup() # 34, weird folder names

df_v3 <- df_v2 %>%
  group_by(OUT.FILE.FS) %>%
  arrange(desc(FOLDER == "Data"), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

duplicates_v3 <- df_v3 %>%
  group_by(OUT.FILE.FS) %>%
  filter(n() > 1) %>%
  ungroup() # 0

# fix INDIR and FOLDER now

unique(df_v3$INDIR)
unique(df_v3$OUTDIR)
unique(df_v3$FOLDER)

df_v3 <- df_v3 %>%
  mutate(INDIR = ifelse(INDIR == "D:\\CM-22\\DATA" , "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-22\\DATA", INDIR))

df_v3 <- df_v3 %>%
  mutate(INDIR = ifelse(INDIR == "D:\\CM-28\\DATA" , "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-28\\DATA", INDIR))

# remove _n from noise files, left with duplicates but at least the correct name and location.

df_v3 <- df_v3 %>%
  mutate(
    OUT.FILE.FS = sub("_n[0-9]+\\.wav$", ".wav", OUT.FILE.FS)
  )

write.csv(df_v1, "cm_all_2024_v2.csv")

# new overview file too

cm_v1 <- df_v1 %>% 
  rename(
    filename = "OUT.FILE.FS",
    autoid = "AUTO.ID.") %>% 
  mutate(autoid = factor(autoid)) %>% 
  dplyr::select(INDIR, OUTDIR, FOLDER, IN.FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE.12, TIME.12, HOUR.12,
                autoid, PULSES, MATCHING, MATCH.RATIO, ALTERNATE.1, ALTERNATE.2 , Site
  )

write.csv(cm_v1, "cm_2024.csv")

# check if small cm is the same, make new smaller dataset and work with that.

cm <- read.csv("cm_2024.csv")

extra_in_cm <- cm %>%
  anti_join(df_v1, by = c("filename" = "OUT.FILE.FS"))

extra_in_df_v1 <- df_v1 %>%
  anti_join(cm, by = c("OUT.FILE.FS" = "filename"))

# weather data check 5h before sunset and 1h after sunrise--------------------

weather <- read.csv("WeatherData2024.csv")
overview_summary <- read.csv("overview_2024.csv")

# fix weather
weather$X <- NULL
weather$Unnamed..0 <- NULL
weather$Unnamed..0.1 <- NULL


weather <- na.omit(weather)

summary(weather)

# look if I have full weather data from 2024 and delete unnecessary ones making a complete data set. 

cm[1]<- NULL

# do I need to make datetime column?
# # cm[nchar(cm$datetime) < 19, ]
# cm$datetime <- as.POSIXct(cm$datetime)
# 
# # datetime lenght 10 needs time 00:00:00
# 
# cm$datetime <- ymd_hms(
#   ifelse(nchar(cm$datetime) == 10,
#          paste0(cm$datetime, " 00:00:00"),
#          cm$datetime)
# )

# make hour list of hours the detector was working- 1h after sunrise and 5h before sunset

overview_summary <- overview_summary %>%
  mutate(
    earliest_active = as.Date(earliest_active),
    last_active     = as.Date(last_active)
  )

# make dates for each site
dates_working <- overview_summary %>%
  rowwise() %>%
  mutate(date = list(seq(
    earliest_active,
    last_active,
    by = "day"
  ))) %>%
  ungroup() %>%
  unnest(date)

# sunrise and sunset
sun_dates <- bind_rows(
  dates_working,
  dates_working %>% mutate(date = date + 1)
) %>%
  distinct(Site, date, .keep_all = TRUE) %>%
  mutate(sun_id = row_number())

# sunrise and sunset in UTC
sun_times <- bind_cols(
  sun_dates,
  suncalc::getSunlightTimes(
    data = sun_dates %>%
      transmute(date, lat = latitude, lon = longitude),
    keep = c("sunrise", "sunset"),
    tz = "UTC"
  ) %>%
    select(sunrise, sunset)
)

# sunset on the current date and sunrise on the next date
daily_windows <- sun_dates %>%
  select(Site, date) %>%
  left_join(
    sun_times %>% select(Site, date, sunset),
    by = c("Site", "date")
  ) %>%
  left_join(
    sun_times %>%
      transmute(Site, date = date - 1, sunrise),
    by = c("Site", "date")
  ) %>%
  mutate(
    recording_start = sunset - hours(5),
    recording_end = sunrise + hours(1)
  )

# individual UTC hours
expected_hours <- daily_windows %>%
  filter(
    !is.na(recording_start),
    !is.na(recording_end),
    recording_end > recording_start
  ) %>%
  rowwise() %>%
  mutate(
    datetime_utc = list(seq(
      floor_date(recording_start, unit = "hour"),
      floor_date(recording_end - seconds(1), unit = "hour"),
      by = "hour"
    ))
  ) %>%
  ungroup() %>%
  unnest(datetime_utc) %>%
  mutate(
    date_utc = as.Date(datetime_utc, tz = "UTC"),
    hour_utc = hour(datetime_utc)
  ) %>%
  select(
    Site, date, datetime_utc, date_utc, hour_utc,
    recording_start, recording_end
  ) %>%
  arrange(Site, datetime_utc)

# compare hours to the weather data
# rename name to Site

weather <- weather %>%
  rename(
    Site = name)

# create date column

weather <- weather %>%
  mutate(date = make_date(
    year = year,  
    month = month,
    day = day
  ))


observed_hours <- weather %>%
  transmute(
    Site,
    date_utc = as.Date(date),
    hour_utc = as.integer(hour)
  ) %>%
  distinct()

# Expected hours with no corresponding observed data
missing_hours <- expected_hours %>%
  anti_join(
    observed_hours,
    by = c("Site", "date_utc", "hour_utc")
  )

# Observed hours outside the expected recording windows
extra_hours <- observed_hours %>%
  anti_join(
    expected_hours %>%
      distinct(Site, date_utc, hour_utc),
    by = c("Site", "date_utc", "hour_utc")
  )

missing <- cm_times |>
  anti_join(weather, by = c("Site" = "name", "year", "month", "day", "hour"))

# calculate completeness

weather_completeness <- expected_hours %>%
  distinct(Site, date_utc, hour_utc) %>%
  left_join(
    observed_hours %>% mutate(found = TRUE),
    by = c("Site", "date_utc", "hour_utc")
  ) %>%
  group_by(Site) %>%
  summarise(
    expected_hours = n(),
    observed_hours = sum(!is.na(found)),
    missing_hours = sum(is.na(found)),
    completeness_pct = round(
      100 * observed_hours / expected_hours, 2
    ),
    .groups = "drop"
  )

# from 1h before and 1h after: 

# sunset on the current date and sunrise on the next date
daily_windows_1 <- sun_dates %>%
  select(Site, date) %>%
  left_join(
    sun_times %>% select(Site, date, sunset),
    by = c("Site", "date")
  ) %>%
  left_join(
    sun_times %>%
      transmute(Site, date = date - 1, sunrise),
    by = c("Site", "date")
  ) %>%
  mutate(
    recording_start = sunset - hours(1),
    recording_end = sunrise + hours(1)
  )

expected_hours_1 <- daily_windows_1 %>%
  filter(
    !is.na(recording_start),
    !is.na(recording_end),
    recording_end > recording_start
  ) %>%
  rowwise() %>%
  mutate(
    datetime_utc = list(seq(
      floor_date(recording_start, unit = "hour"),
      floor_date(recording_end - seconds(1), unit = "hour"),
      by = "hour"
    ))
  ) %>%
  ungroup() %>%
  unnest(datetime_utc) %>%
  mutate(
    date_utc = as.Date(datetime_utc, tz = "UTC"),
    hour_utc = hour(datetime_utc)
  ) %>%
  select(
    Site, date, datetime_utc, date_utc, hour_utc,
    recording_start, recording_end
  ) %>%
  arrange(Site, datetime_utc)

weather_completeness_1 <- expected_hours_1 %>%
  distinct(Site, date_utc, hour_utc) %>%
  left_join(
    observed_hours %>% mutate(found = TRUE),
    by = c("Site", "date_utc", "hour_utc")
  ) %>%
  group_by(Site) %>%
  summarise(
    expected_hours = n(),
    observed_hours = sum(!is.na(found)),
    missing_hours = sum(is.na(found)),
    completeness_pct = round(
      100 * observed_hours / expected_hours, 2
    ),
    .groups = "drop"
  )

missing_hours_1 <- expected_hours_1 %>%
  anti_join(
    observed_hours,
    by = c("Site", "date_utc", "hour_utc")
  )

# make dataset with +-1h for weather

weather_full_1 <- weather_full %>%
  inner_join(
    expected_hours_1,
    by = c("name" = "Site", "datetime" = "datetime_utc")
  )

write.csv(weather_full_1, "WeatherData2024night.csv")

# Make dataset with +-1h data and daytime data only-------------------------------------

cm <- read.csv("cm_2024.csv")
overview_summary <- read.csv("overview_2024.csv")

cm[1] <- NULL
overview_summary[1] <- NULL

str(cm)

# change all dates to date format and all times to time format, some times are read in incorrectly, fix that


convert_time <- function(x) {
  is_numeric <- grepl("^\\d+$", x) & !is.na(x)
  
  seconds <- as.numeric(x[is_numeric]) %% 86400
  
  x[is_numeric] <- sprintf(
    "%02d:%02d:%02d",
    seconds %/% 3600,
    (seconds %% 3600) %/% 60,
    seconds %% 60
  )
  
  readr::parse_time(x, format = "%H:%M:%S")
}

cm <- cm %>%
  mutate(
    DATE = ymd(DATE),
    DATE.12 = ymd(DATE.12),
    TIME = convert_time(TIME),
    TIME.12 = convert_time(TIME.12)
  )

#write.csv(cm, "cm_2024.csv")

cm <- cm %>%
  mutate(
    datetime = as.POSIXct(
      paste(DATE, TIME),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Oslo"
    ),
    datetime.12 = as.POSIXct(
      paste(DATE.12, TIME.12),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Oslo"
    )
  )

# get sunset and sunrise times

sun_input <- cm %>%
  distinct(Site, date = DATE) %>%
  left_join(
    overview_summary %>% select(Site, latitude, longitude),
    by = "Site"
  )

sun_times <- bind_cols(
  sun_input %>% select(Site, DATE = date),
  suncalc::getSunlightTimes(
    data = sun_input %>%
      transmute(date = date, lat = latitude, lon = longitude),
    keep = c("sunrise", "sunset"),
    tz = "Europe/Oslo"
  ) %>%
    select(sunrise, sunset)
)

cm_sun <- cm %>%
  left_join(sun_times, by = c("Site", "DATE"))

# daytime bats 

cm_daytime <- cm_sun %>%
  filter(
    !is.na(sunrise), !is.na(sunset),
    datetime >= sunrise,
    datetime <= sunset
  )

# dataset with +-1h 

night_windows <- sun_times %>%
  arrange(Site, DATE) %>%
  group_by(Site) %>%
  mutate(
    next_sunrise = lead(sunrise)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(sunset),
    !is.na(next_sunrise)
  ) %>%
  transmute(
    Site,
    night_start = sunset - 3600,
    night_end = next_sunrise + 3600
  )

cm_night <- cm %>%
  inner_join(
    night_windows,
    by = "Site",
    relationship = "many-to-many"
  ) %>%
  filter(
    datetime >= night_start,
    datetime <= night_end
  ) %>%
  select(-night_start, -night_end)

write.csv(cm_daytime, "cm_2024_day.csv")
write.csv(cm_night, "cm_2024_night.csv")
