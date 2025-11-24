
# This Rscript is made mainly to put together the data from all sites into 1 csv file
# This script also gives a simple overview of the data.
# Acquire cm_all_2024.csv (on Onedrive, too big to upload to Github) to use the script
# Last run through of script: 21/11/2025

# setup -------------------------------------------------------------------
# libraries used in this project
library(tidyverse)
library(ggplot2)
library(dplyr)
library(suncalc)
library(suntools)
library(lubridate)
library(readr)
library(esquisse)
library(MetBrewer)
library(purrr)
library(sp)
library(sf)
library(hms)
#library(maptools)
#library(ggcorrplot)


# read location overview data in ------------------------------------------------------------

deployment <-  read_csv("data/survey_deployment.csv")
maintenance <-  read_csv("data/survey_maintenance.csv")
overview_data <-  read_csv("data/overview_table.csv")
coordinates <- read_csv("data/GPS_2024.csv")


# Make directories of all the sites and the id files (takes a long time) -----------

  # Define the source and destination directories # computer shut down randomly, real codechunck did not survive
source_dir <- "P:/SW_CoastalMonitoring/Data_collection_2024"
destination_dir <- "data/IDfiles"

  # Get a list of all 50 site folders in source_dir
site_folders <- list.dirs(source_dir, recursive = FALSE)

  ##Don't run this if you are not certain that you want to run the code! 

  # Loop through each site folder
for (site in site_folders) {
  
  # Extract folder name
  site_name <- basename(site)
  
  # Find all "id.csv" files within this folder (including subdirectories)
  csv_files <- list.files(site, pattern = "id\\.csv$", recursive = TRUE, full.names = TRUE)
  
  # Read and merge all CSVs if there are any
  if (length(csv_files) > 0) {
    merged_data <- bind_rows(lapply(csv_files, read_csv))  # Efficient merging
    
    # Create a corresponding site folder in "data"
    site_output_folder <- file.path(destination_dir, site_name)
    if (!dir.exists(site_output_folder)) dir.create(site_output_folder, recursive = TRUE)
    
    # Save merged file
    write_csv(merged_data, file.path(site_output_folder, "all_id.csv"))
  }
}

print("Merging complete!")



# Correcting a mistake made before ----------------------------------------------------------

# df <- list.files(path='data/IDfiles/CM-11', full.names = TRUE) %>% 
#   lapply(read_csv) %>% 
#   bind_rows
# df <- df[-c(45)]
# write.csv(df, "data/IDfiles/CM-11/all_id.csv", row.names = FALSE)
# input <- read.csv("data/IDfiles/CM-11/all_id.csv")
# 
# df <- read.csv("data/IDfiles/CM-43/all_id.csv")
# df <- df[-c(1)]
# df <- df %>% 
#   rename(
#     'AUTO ID*' = "AUTO.ID.",
#     'DATE-12' = "DATE.12",
#     'HOUR-12' = "HOUR.12",
#     'TIME-12' = "TIME.12")
# write.csv(df, "data/IDfiles/CM-43/all_id.csv", row.names = FALSE)
# input <- read.csv("data/IDfiles/CM-43/all_id.csv")

#29
# csv_files <- list.files("P:/SW_CoastalMonitoring/Data_collection_2024/CM-29", pattern = "id\\.csv$", recursive = TRUE, full.names = TRUE)
# if (length(csv_files) > 0) {
#   merged_data <- bind_rows(lapply(csv_files, read_csv))  # Efficient merging
# 
#   # Save merged file
#   write_csv(merged_data, file.path("data/IDfiles/CM-29", "all_id.csv"))
# }
# 
# print("Merging complete!")


# read in all the data from 50 directories  --------------------------------------------------------------

site_folders <- list.dirs("data/IDfiles", recursive = FALSE)
idfiles_dir <- "data/IDfiles"

  # Loop through each site folder
for (site in site_folders) {
  
  # Extract site name
  site_name <- basename(site)
  
  # Remove hyphen for variable naming
  site_name_clean <- str_replace_all(site_name, "-", "")
  
  # Construct file path using site_name
  csv_file <- file.path(idfiles_dir, site_name, "all_id.csv")
  
  # Check if file exists before trying to read
  if (file.exists(csv_file)) {
    # Read the CSV file
    input_data <- read_csv(csv_file)
    
    # Dynamically assign it to a variable
    assign(paste0("input", site_name_clean), input_data, envir = .GlobalEnv)
    
    print(paste("Loaded:", paste0("input", site_name_clean)))
  } else {
    print(paste("No CSV found for:", site_name))
  }
}


# how many rows of data do I have?  ------------------------------------------------------------

  # List all site names
site_names <- sprintf("inputCM%02d", 1:50)


  # Initialize row sum
total_rows <- 0

  # Loop through each data frame
for (site_var in site_names) {
  if (exists(site_var)) {  # Check if the dataframe exists
    total_rows <- total_rows + nrow(get(site_var))  # Add row count
  } else {
    print(paste("Missing:", site_var))
  }
}

  # Print total row count
print(paste("Total number of rows across all datasets:", total_rows))


# Combine all into one file for 2024  --------------------------------------------------------------

  #make column with site name 
for (i in 1:50) {
  
  # Generate site variable name dynamically
  site_var <- sprintf("inputCM%02d", i)  # inputCM01, inputCM02, ..., inputCM50
  site_name <- sprintf("CM-%02d", i)  # CM-01, CM-02, ..., CM-50
  
  if (exists(site_var)) {  # Check if dataframe exists
    temp_data <- get(site_var)  # Retrieve dataframe
    temp_data$Site <- site_name  # Add new column with site name
    
    assign(site_var, temp_data, envir = .GlobalEnv)  # Save back to original variable
  } else {
    print(paste("Missing:", site_var))  # Debugging message
  }
}


  # all into cm

  # Retrieve and combine all existing dataframes
all_sites_data <- lapply(site_names, function(site_var) {
  if (exists(site_var)) {
    get(site_var)  # Retrieve dataframe
  } 
})

  # Bind all dataframes into one large dataframe
cm <- bind_rows(all_sites_data)

  # Check the final merged dataframe
dim(cm)  # Should return (3503006 this is 3773 rows more than I got with a dataset that has no merging problems - 3499216, 45) where X includes the new "Site" column
#run on 05.03 3499233
head(cm)

  #choose variables for dataset
cm <- cm %>% 
  rename(
    filename = "OUT FILE FS",
    autoid = "AUTO ID*",
    DATE.12 = 'DATE-12',
    HOUR.12 = 'HOUR-12',
    TIME.12 = 'TIME-12',
    IN_FILE = "IN FILE", 
    MATCH_RATIO = "MATCH RATIO") %>% 
  mutate(autoid = factor(autoid)) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE.12, TIME.12, HOUR.12,
                autoid, PULSES, MATCH_RATIO, Site
  )

  # remove tests, by location: date deployed and date retrieved

cm <- cm %>% 
  filter(DATE >= as.POSIXct("2024-06-14"))

cm <- cm %>% 
  mutate(DATE = replace(DATE, Site == "CM-39" & DATE > as.POSIXct("2024-10-10"), NA_POSIXct_)) %>% 
  filter(!is.na(DATE))

  # write working csv
write.csv(cm, "cm_all_2024.csv")

  # read csv in

cm <- read.csv("cm_all_2024.csv")



# Make overview table for the year, mostly based on the 2024 all data combined folder.  --------
  #CM-34 was lakesite, changing that in the dataframe

overview_data[34,2] = "lake"  

# new table called overview_summary

overview_summary <- cm %>%
  group_by(Site) %>%
  summarise(
    total = n(),
    noise = sum(autoid == "Noise"),
    noise_pct = 100 * noise / total,
    pnatcalls = sum(autoid == "PIPNAT"),
    batcalls = sum(autoid!="Noise"),
    pnatcalls_pct = 100 * pnatcalls/ batcalls,
    days_active = sum(n_distinct(DATE)),
    earliest_active = min(DATE),
    last_active = max(DATE),
  )

  # add info from other tables
    # Make date more readable
deployment <- deployment %>%
  mutate(`Date and Time Deployed` = parse_date_time(`Date and Time Deployed`,
                                                    orders = "m/d/Y I:M:S p"))

retrieval <- maintenance %>%
  mutate(
    `Date and Time` = parse_date_time(`Date and Time`,
                                      orders = c("m/d/Y I:M:S p", "m/d/Y H:M")),
    dateretrieved = if_else(
      grepl("etri", Comments, ignore.case = TRUE), 
      `Date and Time`,
      as.POSIXct(NA)
    )
  ) %>%
  distinct(`Site Name`, dateretrieved) %>%   
  drop_na(dateretrieved)                     

  # Join onto deployment
deployment <- deployment %>%
  left_join(retrieval, by = "Site Name")

deployment <- deployment %>% 
  rename(
    Site = "Site Name")

coordinates <- coordinates %>% 
  rename(
    Site = "Name")

overview_data <- overview_data %>% 
  rename(
    Site = Location)

overview_summary <- overview_summary %>%
  left_join(
    deployment %>%
      mutate(
        distance_coast_km = as.numeric(gsub(",", ".", `Distance to coast (km)...28`))
      ) %>%
      select(
        Site,
        date_deployed = `Date and Time Deployed`,
        date_retrieved = dateretrieved,
        distance_water_m = `Distance from detector to closest water body (m)`,
        distance_coast_km
      ),
    by = "Site"
  )

overview_summary <- overview_summary %>%
  left_join(
    overview_data %>%
      select(Site,
             type = type),
    by= "Site"
  )

overview_summary <- overview_summary %>%
  left_join(
    coordinates %>%
      select(Site,
             latitude = Latitude,
             longitude = Longitude),
    by= "Site"
  )

  # CM-46 wrong distance (more wrong distances, fix it by hand)
overview_summary[46,14] = 1.246  

#esquisser()

# plot of noise and nr of bat calls
ggplot(overview_summary) +
  aes(x = `noise_pct`, y = `batcalls`, colour = type) +
  geom_point(size = 3.35, 
             shape = "diamond") +
  scale_color_viridis_d(option = "cividis", direction = 1) +
  labs(x = "Percentage of noise files in recordings", 
       y = "Number of recordings with bat calls", title = "Correlation between noise and bat calls", subtitle = "By location") +
  theme_classic() +
  theme(legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))


# Where is the most noise, visualisation  -------------------------------------------------------------------

barnoise <- ggplot(cm) + 
  geom_bar(aes(x= Site, fill = autoid), position = "fill") +
  scale_fill_manual(name = "AutoID", values = met.brewer("Signac", n = 13)) + 
  ylab("Proportion of recordings") + 
  xlab("Site") +
  theme(text = element_text(size = 10)) 

barnoise


# visualize recording period

ggplot(cm) + 
  geom_bin2d(aes(x = DATE.12, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Date in season") + ylab(" ") +
  ggtitle("Recording activity") + 
  theme_minimal()


# gaps in dataset, more info for overview ---------------------------------------------------------
# DATE.12 is the start of the night eg. the start date. 

# Step 1: Generate a complete sequence of dates for each site from deployment
complete_dates <- overview_summary %>% 
  mutate(
    BeginDate = as.Date(date_deployed),
    EndDate   = as.Date(date_retrieved) - 1,
    full_dates = map2(BeginDate, EndDate, ~ seq.Date(from = .x, to = .y, by = "day"))
  ) %>%
  select(Site, full_dates) %>%
  unnest(full_dates) %>%
  rename(ExpectedDate = full_dates)

# Step 2: Get the actual available dates from cm by site
available_dates <- cm %>%
  mutate(ActualDate = as.Date(DATE.12)) %>%
  reframe(ActualDate = unique(ActualDate), .by = "Site")

# Step 3: For each site, find the dates that are expected but missing in the actual data

missing_dates <- complete_dates %>%
  anti_join(available_dates, by = c("Site", "ExpectedDate" = "ActualDate"))

# View missing dates by site

ggplot(missing_dates) + 
  geom_point(aes(x = ExpectedDate, y = Site)) +  
  scale_fill_viridis_c() + 
  xlab("Date in season") + ylab(" ") +
  ggtitle("Missing dates") + 
  theme_minimal()

# add missing dates to overview_summary

n_missing_days <- missing_dates %>%
  group_by(Site) %>%
  summarise(
    missing_days = n(),
  )
    
overview_summary <- overview_summary %>%
  left_join(
    n_missing_days %>%
      select(Site,
              missing_days = missing_days),
    by= "Site"
  )

# change NA to 0 in missing_days

overview_summary <- overview_summary %>% 
  mutate(missing_days = ifelse(is.na(missing_days), 0, missing_days))

write.csv(overview_summary, "overview_2024.csv")

overview_summary <- read.csv("overview_2024.csv")

#write.csv(missing_dates, "Missing dates v5.csv")


# daytime noise -----------------------------------------------------------
# make new df with sunrise and sunset times 
cm_sun <- cm

cm_sun <- cm_sun |> 
  dplyr::select(FOLDER, filename, 
                DATE, TIME,
                autoid, Site
  )
cm_sun$DATE = as.POSIXct(cm_sun$DATE, format= "%Y-%m-%d")

  
# read in df with locations
location <-  read_csv("data/locations.csv")
location$Z <- NULL
location <- location |> 
  rename(
    Site = "Name"
  )

location_sf <- st_as_sf(location, coords = c("X", "Y"), crs = 4326)


# add location to table

cm_sun <- merge(cm_sun, location_sf, by = "Site")
cm_sun<- cm_sun |> 
  rename(
    location = "geometry"
  )
cm_sun$location <- st_as_sf(cm_sun$location)
# names(cm_sun)[7] <- "location"

# calculate sunrise and sunset for each day

cm_sun$sunrise <- sunriset(cm_sun$location, cm_sun$DATE, direction = "sunrise", POSIXct.out = TRUE)$time
cm_sun$sunset <- sunriset(cm_sun$location, cm_sun$DATE, direction = "sunset", POSIXct.out = TRUE)$time

# correct timezone

cm_sun$sunrise <- with_tz(cm_sun$sunrise, "Europe/Oslo")
cm_sun$sunset <- with_tz(cm_sun$sunset, "Europe/Oslo")

# add that to the cm by date? like sunrisetime and sunsettime and then make a column that says yes or no? or just extract the row if it's true and put it into another df?

#put DATETIME together for a posixct 

cm_sun$DateTime <- as.POSIXct(paste(cm_sun$DATE, cm_sun$TIME), format="%Y-%m-%d %H:%M:%S")

#debug why it's not working- convert everything to just times

df <- cm_sun %>%
  # mutate(result = ifelse(autoid != "Noise" & DateTime >= sunrise & DateTime <= sunset, TRUE, FALSE))
  mutate(across(c(sunrise, sunset, DateTime), as_hms)) %>%
mutate(
  daytime = ifelse(autoid != "Noise" & DateTime >= sunrise & DateTime <= sunset, TRUE, FALSE)
)

#find autoIDs that are bats or NoID in cm and put them to another dataframe

daytimebats <- subset(df, daytime == "TRUE")



# plot the result 

ggplot(daytimebats) + 
  geom_bin2d(aes(x = DATE, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Month") + ylab("Site") +
  ggtitle("Bats during daytime") + 
  theme_minimal()

ggplot(daytimebats) +
  aes(x = autoid, y = Site, fill = autoid) +
  geom_tile() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# save file as csv to get all data later:

daytimebats <- daytimebats[, -7] # it was a matrix

write_csv(daytimebats, "daytimebats_all.csv")


# looking at only before sunset so eliminate detections after sunrise

hrs_1 <- 1 * 60 * 60 

df1 <- cm_sun %>%
  # mutate(result = ifelse(autoid != "Noise" & DateTime >= sunrise & DateTime <= sunset, TRUE, FALSE))
  mutate(across(c(sunrise, sunset, DateTime), as_hms)) %>%
  mutate(
    daytime = ifelse(autoid != "Noise" & DateTime >= sunrise + hrs_1 & DateTime <= sunset, TRUE, FALSE)
  )

#find autoIDs that are bats or NoID in cm and put them to another dataframe

daytimebats1 <- subset(df1, daytime == "TRUE")

ggplot(daytimebats1) + 
  geom_bin2d(aes(x = DATE, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Month") + ylab("Site") +
  ggtitle("Bats during daytime") + 
  theme_minimal()

# Graph with 3 instead of 5 aka sunset is 3h before aka all that is left from 

hrs_3 <- 3 * 60 * 60 

df3 <- cm_sun %>%
  # mutate(result = ifelse(autoid != "Noise" & DateTime >= sunrise & DateTime <= sunset, TRUE, FALSE))
  mutate(across(c(sunrise, sunset, DateTime), as_hms)) %>%
  mutate(
    daytime = ifelse(autoid != "Noise" & DateTime >= sunrise + hrs_1 & DateTime <= sunset - hrs_3, TRUE, FALSE)
  )

#find autoIDs that are bats or NoID in cm and put them to another dataframe

daytimebats3 <- subset(df3, daytime == "TRUE")

ggplot(daytimebats3) + 
  geom_bin2d(aes(x = DATE, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Month") + ylab("Site") +
  ggtitle("Bats during daytime") + 
  theme_minimal()

ggplot(daytimebats3) +
  aes(x = autoid, y = Site, fill = autoid) +
  geom_tile() +
  scale_fill_hue(direction = 1) +
  theme_minimal()


# Graph with 2 instead of 5

hrs_2 <- 2 * 60 * 60 

df2 <- cm_sun %>%
  # mutate(result = ifelse(autoid != "Noise" & DateTime >= sunrise & DateTime <= sunset, TRUE, FALSE))
  mutate(across(c(sunrise, sunset, DateTime), as_hms)) %>%
  mutate(
    daytime = ifelse(autoid != "Noise" & DateTime >= sunrise + hrs_1 & DateTime <= sunset - hrs_2, TRUE, FALSE)
  )

#find autoIDs that are bats or NoID in cm and put them to another dataframe

daytimebats2 <- subset(df2, daytime == "TRUE")

ggplot(daytimebats2) + 
  geom_bin2d(aes(x = DATE, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Month") + ylab("Site") +
  ggtitle("Bats during daytime") + 
  theme_minimal()

# Graph with 4 instead of 5

hrs_4 <- 4 * 60 * 60 

df4 <- cm_sun %>%
  # mutate(result = ifelse(autoid != "Noise" & DateTime >= sunrise & DateTime <= sunset, TRUE, FALSE))
  mutate(across(c(sunrise, sunset, DateTime), as_hms)) %>%
  mutate(
    daytime = ifelse(autoid != "Noise" & DateTime >= sunrise + hrs_1 & DateTime <= sunset - hrs_4, TRUE, FALSE)
  )

#find autoIDs that are bats or NoID in cm and put them to another dataframe

daytimebats4 <- subset(df4, daytime == "TRUE")

ggplot(daytimebats4) + 
  geom_bin2d(aes(x = DATE, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Month") + ylab("Site") +
  ggtitle("Bats during daytime") + 
  theme_minimal()


# Subset with daytime bats before sunset ----------------------------------

# all the listings in daytimebats1 (this is without those after sunrise)
# to go through 2024 and put new ID files to the ones with different outdir.

# esquisser()


