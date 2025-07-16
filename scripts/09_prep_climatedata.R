######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to krige daily climate data for study area

library(tidyverse)
library(sf)
library(terra)
source("scripts/functions/raster_prep_functions.R")

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds") %>%
  sf::st_transform(32609)

### Load climate station spatial data
climate_stations <- read_csv("data/raw/weather/climate_station_list.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326)) %>%
  st_transform(32609) %>%
  filter(Province == "NORTHWEST TERRITORIES")

### create buffer of collar data bounding box
buffer <- musk_collar %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_buffer(100000)

### crop climate station data
climate_stations_crop <- climate_stations %>%
  sf::st_intersection(buffer)

### list and import station data files
station_files <- tibble(file = list.files("data/raw/weather/station_data")) %>%
  mutate(year = str_split_i(file, "_", 5),
         year = substr(year,1,4),
         year = as.numeric(year),
         station = str_split_i(file, "_", 4)) %>%
  filter(station %in% climate_stations_crop$`Climate ID`)

station_data <- map(str_c("data/raw/weather/station_data/",
                          station_files$file),
                    function(x){
                      read_csv(x, locale=locale(encoding="latin1"),
                               col_types = cols(.default = col_character()))
                    }) %>%
  list_rbind() %>%
  mutate(date = as.Date(`Date/Time`),
         Month = as.numeric(Month),
         Year = as.numeric(Year),
         max_temp = as.numeric(`Max Temp (°C)`),
         mean_temp = as.numeric(`Mean Temp (°C)`),
         min_temp = as.numeric(`Min Temp (°C)`),
         precip = as.numeric(`Total Precip (mm)`),
         wind = ifelse(`Spd of Max Gust (km/h)`=="<31",31,
                       as.numeric(`Spd of Max Gust (km/h)`)))

### create average for each day 
### I checked and there is at least 2 stations for each day in the monitoring 
### period for temperature and 1 for precipitation and wind
weather_data_average <- station_data %>%
  group_by(date) %>%
  summarize(avg_mean_temp = mean(mean_temp, na.rm = TRUE),
            avg_max_temp = mean(max_temp, na.rm = TRUE),
            avg_min_temp = mean(min_temp, na.rm = TRUE),
            avg_precip = mean(precip, na.rm = TRUE),
            avg_wind = mean(wind, na.rm = TRUE),
            num_stations_temp = sum(!is.na(mean_temp)),
            num_stations_precip = sum(!is.na(precip)),
            num_stations_wind = sum(!is.na(wind)))

saveRDS(weather_data_average, "data/processed/weather_data_average.rds")
 
ggplot(station_average, aes(x = date, y = avg_max_temp)) +
  geom_line()
