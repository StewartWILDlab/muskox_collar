######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script for processing aerial survey data

library(tidyverse)
library(sf)
library(basemaps)
library(tidyterra)

musk_aer <- read_csv("data/raw/muskox_data/aerial_data/MXSurveyData_Thesis_clean.csv") %>%
  drop_na(Lat) %>%
  st_as_sf(coords = c("Long", "Lat")) %>%
  filter(Study.Area == "Sahtu") %>%
  st_set_crs(4326) %>%
  st_transform(3857)

musk_aer_buff <- musk_aer %>%
  st_buffer(dist = 11000)

saveRDS(musk_aer, "data/processed/musk_aer.rds")

fire_data_subset_aer <- readRDS("data/processed/fire_data_subset_aer.rds")%>%
  st_transform(3857) 

ggplot() +
  basemap_gglayer(st_bbox(fire_data_subset_aer)) +
  geom_sf(data = fire_data_subset_aer %>%
            filter(fireyear > 1990)) +
  geom_sf(data = musk_aer_buff, aes(colour = Study.Area), fill = NA) +
  scale_fill_identity() + 
  coord_sf()

ggplot() +
  geom_spatraster(data = lc_2010_crop_aer) +
  geom_sf(data = musk_aer_buff, aes(colour = Study.Area), fill = NA, linewidth = 1)
  

