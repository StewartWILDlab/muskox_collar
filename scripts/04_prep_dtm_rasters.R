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
source("scripts/functions/raster_prep_functions.R")

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds")

### Load land cover Canada 2010 data
mrdtm <- terra::rast("data/raw/MRDEM/mrdtm.tif")

### resize data to make subsequent analyses computationally easier
mrdtm_crop <- resize_raster_to_gps_points(
  mrdtm, 
  musk_collar, 
  buffer_dist = 100000)


## now project to UTM Zone 9N crs for future analysis
mrdtm_proj <- crop_and_reproject_to_gps_points(
  mrdtm_crop,
  musk_collar,
  crs_epsg = 32609,
  buffer_dist = 10000
)

### start with a 3 by 3 region around cell of interest, resolution is 30 m so 
### that equates to a 90 by 90 m box around each point
mrtri3_proj <- spatialEco::tri(mrdtm_proj, s = 3, exact = FALSE)

writeRaster(mrdtm_crop, "data/processed/mrdtm_crop.tif", overwrite = TRUE)
writeRaster(mrdtm_proj, "data/processed/mrdtm_proj.tif", overwrite = TRUE)
writeRaster(mrtri3_proj, "data/processed/mrtri3_proj.tif", overwrite = TRUE)



