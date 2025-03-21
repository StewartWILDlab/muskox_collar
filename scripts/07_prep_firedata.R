######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to crop historical fire data 

library(tidyverse)
library(sf)
library(terra)
source("scripts/functions/raster_prep_functions.R")


### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds") %>%
  sf::st_transform(32609)

### Load fire data
# fire_data <- sf::read_sf("data/raw/fire/nwt_firehistory/NWT_FireHistory_HighRes.shp") %>%
#   sf::st_transform(32609)
fire_data_nbac <- sf::read_sf("data/raw/fire/NBAC/nbac_1972_2023_20240530.shp") 
fire_data_nfd <- sf::read_sf("data/raw/fire/NFD/NFDB_poly_20210707.shp") 

### create buffer of collar data bounding box
buffer <- musk_collar %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_buffer(50000) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc()
  

### crop fire data
fire_data_nbac_crop <- fire_data_nbac %>%
  sf::st_intersection(buffer %>% st_transform(st_crs(fire_data_nbac))) %>%
  sf::st_transform(32609)

fire_data_nfd_crop <- fire_data_nfd %>%
  sf::st_intersection(buffer %>% st_transform(st_crs(fire_data_nfd))) %>%
  sf::st_transform(32609)

### combine cropped fire data
fire_data_subset <- fire_data_nbac_crop %>%
  bind_rows(fire_data_nfd_crop %>%
              filter(YEAR < 1972)) %>%
  rename(fireyear = YEAR) 
  # mutate(effectdate = as.POSIXct(effectdate))

saveRDS(fire_data_subset, "data/processed/fire_data_subset.rds")

### Create raster of years since fire for all years covered by muskox collar data
### create a stacked raster of years since fire
dates <- seq(ymd("2007-01-01"), ymd("2012-01-01"), by="year")
temp_rast <- rast(fire_data_subset, resolution = 100)

map(dates, function(x){
  rasterize_fire_year(fire_data_subset, 
                    year_col = "fireyear", 
                    date = x, 
                    temp_rast = temp_rast) %>%
    crop_and_reproject_to_gps_points(
      musk_collar,
      crs_epsg = 32609,
      buffer_dist = 10000) %>%
    save_raster_to_folder(folder_name = "fire_year",
                          date = x)
})

### combine rasters into a single stack
rasts <- list.files("data/processed/fire_year", 
                    pattern = "*.tif$", 
                    full.names = TRUE)
fire_year <- rast(rasts)


### Load NTEMS fire NBR
ntems_fire_nbr <- terra::rast("data/raw/fire/NTEMS/CA_Forest_Wildfire_dNBR_1985_2020.tif")

### resize data to make subsequent analyses computationally easier
ntems_fire_nbr_crop <- resize_raster_to_gps_points(
  ntems_fire_nbr, 
  musk_collar, 
  buffer_dist = 100000)

## now project to UTM Zone 9N crs for future analysis
ntems_fire_nbr_proj <- crop_and_reproject_to_gps_points(
  ntems_fire_nbr_crop,
  musk_collar,
  crs_epsg = 32609,
  buffer_dist = 10000
)

writeRaster(ntems_fire_nbr_proj, "data/processed/ntems_fire_nbr_proj.tif", overwrite = TRUE)


### No Longer using fire distance rasters
# ### create fire data rasters for different year thresholds (0-10 years, 10-25 years)
# 
# ### raster template
# temp_rast <- rast(fire_data_subset, resolution = 100)
# dates <- seq(ymd("2007-01-01"), ymd("2012-12-31"), by="week")
# 
# ### create and save tifs of 10 year fire data for each day from 2007 to 2012
# map(dates, function(x){
#   rasterize_by_date(fire_data_subset, 
#                     datetime_col = effectdate, 
#                     date = x, 
#                     max_years = 10, 
#                     min_years = 0,
#                     temp_rast = temp_rast) %>%
#     distance() %>%
#     crop_and_reproject_to_gps_points(
#       musk_collar,
#       crs_epsg = 32609,
#       buffer_dist = 10000) %>%
#     save_raster_to_folder(folder_name = "fire_10year",
#                           date = x)
# })
# ### combine rasters into a single raster stack
# rasts <- list.files("data/processed/fire_10year", 
#                     pattern = "*.tif$", 
#                     full.names = TRUE)
# fire_10year_stack <- rast(rasts) 
# 
# 
# ### create and save tifs of 0-25 year fire data for each day from 2007 to 2012
# map(dates, function(x){
#   rasterize_by_date(fire_data_subset, 
#                     datetime_col = "effectdate", 
#                     date = x, 
#                     max_years = 25, 
#                     min_years = 0,
#                     temp_rast = temp_rast) %>%
#     distance() %>%
#     crop_and_reproject_to_gps_points(
#       musk_collar,
#       crs_epsg = 32609,
#       buffer_dist = 10000) %>%
#     save_raster_to_folder(folder_name = "fire_25year",
#                           date = x)
# })
# ### combine rasters into a single stack
# rasts <- list.files("data/processed/fire_25year", 
#                     pattern = "*.tif$", 
#                     full.names = TRUE)
# fire_25year_stack <- rast(rasts)
# 
# ### create and save tifs of all fires occurring after each day from 2007 to 2012
# map(dates, function(x){
#   rast <- rasterize_by_date(fire_data_subset, 
#                     datetime_col = "effectdate", 
#                     date = x, 
#                     ## specify negative years to focus on fires occurring after each date
#                     max_years = 0, 
#                     min_years = -20,
#                     temp_rast = temp_rast) %>%
#     crop_and_reproject_to_gps_points(
#       musk_collar,
#       crs_epsg = 32609,
#       buffer_dist = 10000)
#   rast[is.na(rast)] <- 0
#   save_raster_to_folder(rast,
#                         folder_name = "fire_postdate",
#                         date = x)
# })
# ### combine rasters into a single stack
# rasts <- list.files("data/processed/fire_postdate", 
#                     pattern = "*.tif$", 
#                     full.names = TRUE)
# fire_postdate_stack <- rast(rasts)
