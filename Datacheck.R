# Find out if data is cohesive

# If you have filenames
# if you have all the weather data
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

df <- read.csv("cm_all_2024_v2.csv")

# initial check--------------------

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

duplicates <- df %>%
  group_by(OUT.FILE.FS) %>%
  filter(n() > 1) %>%
  ungroup() # 121758 - because Noise has same name if it comes from same file


unique(df$INDIR)
unique(df$OUTDIR)

na_rows <- df %>% filter(is.na(OUT.FILE.FS))
