# This is the code for site-specific overviews

library(esquisse)

esquisser()

## Housekeeping
cm01 <- inputCM01 %>% 
  rename(
    filename = "OUT FILE FS",
    autoid = "AUTO ID*",
    DATE.12 = 'DATE-12',
    HOUR.12 = 'HOUR-12',
    TIME.12 = 'TIME-12') %>% 
  mutate(autoid = factor(autoid)) %>% 
  dplyr::select(OUTDIR, FOLDER, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE.12, TIME.12, HOUR.12,
                autoid, Site
  )
# Get a sense for how much noise there is across these sites 

## Make a figure illustrating this 
barnoise <- ggplot(cm01) + 
  geom_bar(aes(x= site, fill = autoid), position = "fill") +
  scale_fill_manual(name = "AutoID", values = met.brewer("Egypt", n = 13)) + 
  ylab("Proportion of recordings") + 
  xlab("Site") +
  theme(text = element_text(size = 25)) 
barnoise




# example daylight graph for CM-10 --------------------------------------------------
# clean table of columns you won't use
clean_data_10 <- inputCM10[-c(1:3,5:6,8:9,13:15,20:36,38:44)]

# rename columns for better compatibility with R
clean_data_10 <- clean_data_10 |> 
  rename(IN.FILE = 1, AUTO.ID = 6, MATCH.RATIO = 9, MANUAL.ID = 10)

# data to correct class
clean_data_10 <- clean_data_10 %>%
  mutate(DATETIME = ymd_hms(paste(DATE, TIME)))

noise_10 <- subset(clean_data_10, AUTO.ID == "Noise")
wo_noise_10 <- subset(clean_data_10, AUTO.ID != "Noise")

# vector of all the days active in 2024
days <- seq(from = as.POSIXct("2024-06-24"), to = as.POSIXct("2024-10-02"), by = "days")

# coordinates 
lat <- 58.80411
lon <- 5.620517

coord <- matrix(c(lon, lat), nrow = 1)

# compute sunrise and sunset times for every day
sun_times <- data.frame(
  date = days,
  sunrise = sunriset(coord, days, direction = "sunrise", POSIXct.out = TRUE)$time,
  sunset = sunriset(coord, days, direction = "sunset", POSIXct.out = TRUE)$time
)

# strip date information from sunrise and sunset columns
sun_times$sunrise <- format(sun_times$sunrise, format = "%H:%M:%S")
sun_times$sunset <- format(sun_times$sunset, format = "%H:%M:%S")

# convert character to time
sun_times$sunrise <- hms(sun_times$sunrise)
sun_times$sunset <- hms(sun_times$sunset)

# what class is used in plot
#class(sun_times$date)
#class(sun_times$sunrise)

# convert points of noise and non-noise to correct classes
noise_10$DATE <- as.POSIXct(noise_10$DATE, format="%Y-%m-%d")
#class(wo_noise$MANUAL.ID)
noise_10$TIME <- as.character(noise_10$TIME)
noise_10$TIME <- hms(noise_10$TIME)
wo_noise_10$DATE <- as.POSIXct(wo_noise_10$DATE, format="%Y-%m-%d")
wo_noise_10$TIME <- as.character(wo_noise_10$TIME)
wo_noise_10$TIME <- hms(wo_noise_10$TIME)

#```

#```{r sunrise and sunset plot}
# plot the result
ggplot(sun_times, aes(x = date, ymin = sunrise, ymax = sunset)) +
  labs(
    title = "2024 Recorded Sounds Compared to Daylight",
    subtitle = "Voll, Norge"
  ) +
  geom_ribbon(fill = "orange", alpha = 0.5) + 
  scale_x_datetime(
    name = "Date",
    date_labels = "%b",
    date_breaks = "1 month",
    minor_breaks = NULL,
  ) +
  scale_y_time(
    name = "Time",
    breaks = hm(paste(seq(from = 0, to = 24, by = 2), 0)),
    labels = function(t) sprintf("%02d:%02d", hour(t), minute(t)),
    limits = c(hm("0 00"), hm("24 00"))
  ) +
  geom_point(
    data = noise_10,
    aes(x = DATE, y = TIME, fill = AUTO.ID),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = wo_noise_10,
    inherit.aes = FALSE,
    aes(x = DATE, y = TIME, colour = AUTO.ID),
  ) +
  scale_colour_manual(
    values = c(
      "VESMUR" = "blue", 
      "PIPPYG" = "hotpink2", 
      "PIPNAT" = "red", 
      "NYCNOC" = "darkred", 
      "EPTNIL" = "steelblue3")
  )

clean_data_10 %>%
  filter(DATE >= "2024-06-19" & DATE <= "2024-10-01") %>%
  filter(!(AUTO.ID %in% c("BARBAR", "MYONAT", "MYODAU", "Noise"))) %>%
  ggplot() +
  aes(x = AUTO.ID) +
  geom_bar(fill = "#228B22") +
  labs(
    x = "Automatic ID",
    y = "Recordings",
    title = "Detector overview"
  ) +
  theme_linedraw()



