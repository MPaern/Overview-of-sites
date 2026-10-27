# Find out if data is cohesive

# If you have duplicates
# if you have all the weather data and if it's only for the times I need
# What's the timeframe of the recordings
# What's the timezone of the data?
# where are the NA-s?
# Do you have all the dates/times
# Are the missing dates = dates that the detector was on, other days are missing because the detector was off.
# Find mistakes in autoid
# Make a dataset with 30 min before and 30 min after data
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

write.csv(df_v3, "cm_all_2024_v2.csv")

# check if small cm is the same, make new smaller dataset and work with that.

cm <- read.csv("cm_2024.csv")

extra_in_cm <- cm %>%
  anti_join(df_v3, by = c("filename" = "OUT.FILE.FS"))

extra_in_df_v3 <- df_v3 %>%
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

cm <- read.csv("cm_2025.csv")
cm[1]<- NULL
# cm[nchar(cm$datetime) < 19, ]
cm$datetime <- as.POSIXct(cm$datetime)

# datetime lenght 10 needs time 00:00:00

cm$datetime <- ymd_hms(
  ifelse(nchar(cm$datetime) == 10,
         paste0(cm$datetime, " 00:00:00"),
         cm$datetime)
)


# compare hours to the weather data
# Fix timezone
# then convert to true UTC to match weather
cm_times <- cm |>
  mutate(
    dt_utc = with_tz(force_tz(datetime, "Europe/Oslo"), "UTC"),
    year  = year(dt_utc),
    month = month(dt_utc),
    day   = day(dt_utc),
    hour  = hour(dt_utc)
  )


# Find cm rows with no matching weather record
missing <- cm_times |>
  anti_join(weather, by = c("Site" = "name", "year", "month", "day", "hour"))

nrow(missing)       # how many cm rows lack weather
missing |> count(Site)  # broken down by site

# there are some hours missing for the data, but not between sunset-sunrise  
# will fix the data by downloading more weather data 

