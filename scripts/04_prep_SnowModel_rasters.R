######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to crop SNOW Model data to extent of muskox collar data

library(tidyverse)
library(sf)
library(terra)

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds")

### Load SnowModel data from each year
sm_2007 <- terra::rast("data/raw/SnowModel/SnowModel_snow_depth_2007.nc4", 
                       drivers="NETCDF")
sm_2008 <- terra::rast("data/raw/SnowModel/SnowModel_snow_depth_2008.nc4", 
                       drivers="NETCDF")
sm_2009 <- terra::rast("data/raw/SnowModel/SnowModel_snow_depth_2009.nc4", 
                       drivers="NETCDF")
sm_2010 <- terra::rast("data/raw/SnowModel/SnowModel_snow_depth_2010.nc4", 
                       drivers="NETCDF")
sm_2011 <- terra::rast("data/raw/SnowModel/SnowModel_snow_depth_2011.nc4", 
                       drivers="NETCDF")
sm_2012 <- terra::rast("data/raw/SnowModel/SnowModel_snow_depth_2012.nc4", 
                       drivers="NETCDF")

### buffer collar data 1km
buffer <- musk_collar %>%
  sf::st_transform(terra::crs(sm_2008)) %>%
  sf::st_buffer(dist = 100000)

### crop snow depth data to collar buffer
sm_2007_crop <- terra::crop(sm_2007, buffer)
sm_2008_crop <- terra::crop(sm_2008, buffer)
sm_2009_crop <- terra::crop(sm_2009, buffer)
sm_2010_crop <- terra::crop(sm_2010, buffer)
sm_2011_crop <- terra::crop(sm_2011, buffer)
sm_2012_crop <- terra::crop(sm_2012, buffer)

writeCDF(sm_2007_crop, "data/processed/sm_2007_crop.nc", overwrite = TRUE)
writeCDF(sm_2008_crop, "data/processed/sm_2008_crop.nc", overwrite = TRUE)
writeCDF(sm_2009_crop, "data/processed/sm_2009_crop.nc", overwrite = TRUE)
writeCDF(sm_2010_crop, "data/processed/sm_2010_crop.nc", overwrite = TRUE)
writeCDF(sm_2011_crop, "data/processed/sm_2011_crop.nc", overwrite = TRUE)
writeCDF(sm_2012_crop, "data/processed/sm_2012_crop.nc", overwrite = TRUE)

### Calculate per day averages for snow depth across the stusy area

snowdepth_mean <- global(sm_2007_crop, "mean") %>%
  bind_rows(global(sm_2008_crop, "mean")) %>%
  bind_rows(global(sm_2009_crop, "mean")) %>%
  bind_rows(global(sm_2010_crop, "mean")) %>%
  bind_rows(global(sm_2011_crop, "mean")) %>%
  bind_rows(global(sm_2012_crop, "mean")) %>%
  mutate(date = seq( as.Date("2007-01-01"), as.Date("2012-12-31"), by="+1 day"),
         snow_depth = round(mean,2))

saveRDS(snowdepth_mean, "data/processed/snowdepth_mean.rds")
