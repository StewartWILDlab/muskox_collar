######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to crop waterbody data and create distance to water raster

library(tidyverse)
library(sf)
library(terra)
source("scripts/functions/raster_prep_functions.R")

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds") %>%
  sf::st_transform(32609)

### Load NRCan water body data
water_data <- sf::read_sf("data/raw/geography/waterbodies/nwt_water/canvec_50K_NT_hydro/waterbody_2_3.shp")
large_water_data <- sf::read_sf("data/raw/geography/waterbodies/lhy_000c16a_e.shp")

### create buffer of collar data bounding box
large_buffer <- musk_collar %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_buffer(100000)
small_buffer <- musk_collar %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_buffer(10000)

### crop water data, transform coordinates, and crop again
water_data_crop <- water_data %>%
  sf::st_intersection(large_buffer %>% st_transform(st_crs(water_data))) %>%
  st_transform(32609)%>%
  ### select lakes
  filter(definit == 83,
         perm == 59)
large_water_data_crop <- large_water_data %>%
  sf::st_intersection(large_buffer %>% st_transform(st_crs(large_water_data))) %>%
  st_transform(32609)

saveRDS(water_data_crop, "data/processed/water_data_crop.rds")
saveRDS(large_water_data_crop, "data/processed/large_water_data_crop.rds")
water_data_crop <- readRDS("data/processed/water_data_crop.rds")

### create distance to water raster
rast <- terra::rast(water_data_crop, resolution = 30)
water_rast <- rasterize(water_data_crop, rast)
water_dist <- distance(water_rast) %>%
  crop_and_reproject_to_gps_points(
    musk_collar,
    crs_epsg = 32609,
    buffer_dist = 10000
  )
  
writeRaster(water_dist, "data/processed/water_dist.tif", overwrite = TRUE)

ggplot() +
  geom_sf(data = water_data_crop) +
  geom_sf(data = musk_collar, aes(colour = Id_Number), alpha = 0.5)
