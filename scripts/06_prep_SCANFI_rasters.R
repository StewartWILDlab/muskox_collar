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
prcC_2020 <- terra::rast("data/raw/SCANFI/scanfi_prcC.tif")
biomass_2020 <- terra::rast("data/raw/SCANFI/scanfi_biomass.tif")
lc_2020 <- terra::rast("data/raw/SCANFI/scanfi_lc.tif")
cats <- data.frame(id = 1:8, cover = c("Bryoid", "Herbs", "Rock", "Shrub",
                                       "Treed broadleaf", "Treed conifer",
                                       "Treed mixed", "Water"))
coltab <- data.frame(id = 1:8, cover = c("#408A73","#BAD48F","#A8ABAE","#B38A33",
                                         "#148C3D","#003D00","#5C752B","#4C70A3"))
levels(lc_2020) <- cats
coltab(lc_2020) <- coltab

### resize data to make subsequent analyses computationally easier
prcC_2020_crop <- resize_raster_to_gps_points(
  prcC_2020, 
  musk_collar, 
  buffer_dist = 100000)
biomass_2020_crop <- resize_raster_to_gps_points(
  biomass_2020, 
  musk_collar, 
  buffer_dist = 100000)
lc_2020_crop <- resize_raster_to_gps_points(
  lc_2020, 
  musk_collar, 
  buffer_dist = 100000)


## now project to UTM Zone 9N crs for future analysis
prcC_2020_proj <- crop_and_reproject_to_gps_points(
  prcC_2020_crop,
  musk_collar,
  crs_epsg = 32609,
  buffer_dist = 10000
)
biomass_2020_proj <- crop_and_reproject_to_gps_points(
  biomass_2020_crop,
  musk_collar,
  crs_epsg = 32609,
  buffer_dist = 10000
)
lc_2020_proj <- crop_and_reproject_to_gps_points(
  lc_2020_crop,
  musk_collar,
  crs_epsg = 32609,
  buffer_dist = 10000
)


writeRaster(prcC_2020_crop, "data/processed/prcC_2020_crop.tif", overwrite = TRUE)
writeRaster(prcC_2020_proj, "data/processed/prcC_2020_proj.tif", overwrite = TRUE)
writeRaster(biomass_2020_crop, "data/processed/biomass_2020_crop.tif", overwrite = TRUE)
writeRaster(biomass_2020_proj, "data/processed/biomass_2020_proj.tif", overwrite = TRUE)
writeRaster(lc_2020_crop, "data/processed/lc_2020_crop.tif", overwrite = TRUE)
writeRaster(lc_2020_proj, "data/processed/lc_2020_proj.tif", overwrite = TRUE)

ggplot() +
  tidyterra::geom_spatraster(data = prcC_2020_proj)
