######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to tidy date time information stored in collar data

library(tidyverse)

## default empty point to fill in missing values when calculating distances 
## between successive points
empty <- sf::st_as_sfc("POINT(EMPTY)", crs = sf::st_crs(musk_collar))

musk_collar <- sf::st_read("data/raw/muskox_data/Muskox_GPS.shp") %>%
  ### add time to date
  mutate(
    fraction_of_day = SerialDate %%1,
    datetime = round_date(as_datetime(Date) + 
                            ddays(fraction_of_day), unit = "hour"),
    Id_Number = as.factor(Id_Number),
    month = month(datetime),
    year = year(datetime)
  ) %>% 
  ## add column for distance traveled from previous point
  group_by(Id_Number) %>% 
  arrange(datetime) %>%
  mutate(steplength_m = sf::st_distance(geometry, 
                                    lag(geometry, default = empty),
                                    by_element = TRUE),
         steplength_m = as.numeric(steplength_m))

saveRDS(musk_collar, "data/processed/musk_collar.rds")

