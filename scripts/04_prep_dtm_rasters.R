######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to crop raster data to extent of muskox collar data

library(tidyverse)
library(sf)
library(terra)

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds")

### Load land cover Canada 2010 data
mrdtm <- terra::rast("data/raw/MRDEM/mrdtm.tif")

### buffer collar data 1km
buffer <- musk_collar %>%
  sf::st_transform(terra::crs(mrdtm)) %>%
  sf::st_buffer(dist = 100000)

### crop land cover data to collar data buffer
mrdtm_crop <- terra::crop(mrdtm, buffer)

## now project land cover to same crs as location data for plotting
mrdtm_proj <- mrdtm_crop %>%
  terra::project(y = "epsg:4326") %>%
  terra::crop(musk_collar %>% sf::st_buffer(dist = 10000))

writeRaster(mrdtm_crop, "data/processed/mrdtm_crop.tif", overwrite = TRUE)
writeRaster(mrdtm_proj, "data/processed/mrdtm_proj.tif", overwrite = TRUE)



