# Changed outdir paths to many id-s, might be needed later so putting together a new csv file for 2024

#version 2 of 2024 data--------
library(tidyverse)
library(ggplot2)
library(lubridate)

# make cm-11 as a template for other merges

cm11 <- read_csv("data/IDfiles/v2/CM-11/all_id_v2.csv")

spec_template <- spec(cm11)

# list files

files <- list.files(
  path = "data/IDfiles/v2/",
  pattern = "\\.csv$",   # use double backslash for regex
  recursive = TRUE,      # search subfolders
  full.names = TRUE      # return full path
)

# merge files adding a Site column

all <- files |>
  lapply(function(f) {
    read_csv(f, col_types = cols(DATE = col_character(),`DATE-12` = col_character())) |>
      mutate(Site = basename(dirname(f)))
  }) |>
  bind_rows()

# there are 5 INDIR and OUTDIR for CM-09 that did not have the correct filepath, fix this

all$OUTDIR <- replace(
  all$OUTDIR,
  all$OUTDIR == "D:\\CM-09\\WAV\\KPRO_V1_13.08.2024_CM-09",
  "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-09\\WAV\\KPRO_V1_13.08.2024_CM-09"
)
all$INDIR <- replace(
  all$INDIR,
  all$INDIR == "D:\\CM-09\\DATA\\13.08.2024_CM-09",
  "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-09\\DATA\\13.08.2024_CM-09"
)
all$OUTDIR <- replace(
  all$OUTDIR,
  all$OUTDIR == "P:\\SW_CoastalMonitoring\\Data_Collection_2024\\CM-34\\WAV\\KPRO_V1_15.10.2024_CM-34",
  "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-34\\WAV\\KPRO_V1_15.10.2024_CM-34"
)

# check if merging succeeded

colSums(is.na(all))
str(all) 
summary(all) 
na_rows <- all %>% filter(is.na(`DATE-12`))
unique(na_rows$Site)
unique(na_rows$INDIR)

# 2 runs in CM-34 not merging with date, loading them in separately

csv_files <- c("P:/SW_CoastalMonitoring/Data_collection_2024/CM-34/WAV/KPRO_V1_18.09.2024_CM-34/id.csv",
               "P:/SW_CoastalMonitoring/Data_collection_2024/CM-34/WAV/KPRO_V1_22.07.2024_CM-34/id.csv")

df34 <- csv_files |>
  lapply(function(f) {
  read_csv(f, col_types = cols(DATE = col_character(),`DATE-12` = col_character())) |>
    mutate(Site = "CM-34")
}) |>
  bind_rows()

colSums(is.na(df34))

str(all)
str(df34)

# remove data from those 2 runs from main dataframe

all <- all[!(all$OUTDIR %in% c("P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-34\\WAV\\KPRO_V1_18.09.2024_CM-34","P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-34\\WAV\\KPRO_V1_22.07.2024_CM-34")),]

# make all and df34 DATE and DATE-12 columns as date format----------------------
# they are in a lot of different formats, find out what formats are in use

formatsdate<- all %>%
  group_by(OUTDIR) %>%
  summarise(dates = paste(unique(DATE), collapse = ", "))

# 3 formats, OUTDIR CM-12 (14&20) and CM-34 DD/MM/YYYY, some m/d/y, most YYYY-MM-DD

group1 <- c(	
  "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-34\\WAV\\KPRO_V1_15.10.2024_CM-34", "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_14.10.2024_CM-12", 
  "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_20.07.2024_CM-12")

group2 <- c("CM-03", "CM-04", "CM-05", "CM-06", "CM-07", "CM-08", "CM-09", "CM-10", "CM-13", "CM-14", "CM-15", "CM-16", "CM-17", "CM-21", "CM-23", "CM-27")

all_1 <- all %>%
  mutate(
    DATE = replace(DATE, OUTDIR %in% group1, dmy(DATE[OUTDIR %in% group1])),
    DATE = replace(DATE, Site %in% group2, mdy(DATE[Site %in% group2])),
    DATE = replace(DATE, OUTDIR == "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12",
                   mdy(DATE[OUTDIR == "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12"])),
    DATE = replace(DATE, !OUTDIR %in% group1 & !Site %in% group2 &
                     OUTDIR != "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12",
                   ymd(DATE[!OUTDIR %in% group1 & !Site %in% group2 &
                              OUTDIR != "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12"]))
  )

# still chr, make it a date

all_1 <- all_1 %>%
  mutate(DATE = as.Date(as.numeric(DATE), origin = "1970-01-01"))

# make df34 date

df34$DATE = as.Date(df34$DATE, format = "%m/%d/%y")

# also for DATE-12

all_2 <- all_1 %>%
  mutate(
    `DATE-12` = replace(`DATE-12`, OUTDIR %in% group1, dmy(`DATE-12`[OUTDIR %in% group1])),
    `DATE-12` = replace(`DATE-12`, Site %in% group2, mdy(`DATE-12`[Site %in% group2])),
    `DATE-12` = replace(`DATE-12`, OUTDIR == "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12",
                   mdy(`DATE-12`[OUTDIR == "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12"])),
    `DATE-12` = replace(`DATE-12`, !OUTDIR %in% group1 & !Site %in% group2 &
                     OUTDIR != "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12",
                   ymd(`DATE-12`[!OUTDIR %in% group1 & !Site %in% group2 &
                              OUTDIR != "P:\\SW_CoastalMonitoring\\Data_collection_2024\\CM-12\\WAV\\KPRO_V1_31.08.2024_CM-12"]))
  )

all_2 <- all_2 %>%
  mutate(`DATE-12` = as.Date(as.numeric(`DATE-12`), origin = "1970-01-01"))

df34$`DATE-12` = as.Date(df34$`DATE-12`, format = "%m/%d/%y")

# merge all_2 and df34

cm <- rbind(all_2, df34)

# how did it go?

colSums(is.na(cm))
str(cm) 
summary(cm) 

# write working csv-------------------------------
write.csv(cm, "cm_all_2024_v2.csv")

