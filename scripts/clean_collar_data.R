library(tidyverse)
library(sf)

musk_collar <- st_read("data/raw/muskox_data/Muskox_GPS.shp") %>%
  ### add time to date
  mutate(
    fraction_of_day = SerialDate %%1,
    datetime = round_date(as_datetime(Date) + 
                            ddays(fraction_of_day), unit = "hour"),
    Id_Number = as.factor(Id_Number),
    month = month(datetime),
    year = year(datetime)
  )

saveRDS(musk_collar, "data/processed/musk_collar.rds")

