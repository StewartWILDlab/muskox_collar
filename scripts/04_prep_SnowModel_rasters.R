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
source("scripts/functions/raster_prep_functions.R")

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

### crop snow depth data to collar buffer
sm_2007_crop <- resize_raster_to_gps_points(
  sm_2007, 
  musk_collar, 
  buffer_dist = 100000)
sm_2008_crop <- resize_raster_to_gps_points(
  sm_2008, 
  musk_collar, 
  buffer_dist = 100000)
sm_2009_crop <- resize_raster_to_gps_points(
  sm_2009, 
  musk_collar, 
  buffer_dist = 100000)
sm_2010_crop <- resize_raster_to_gps_points(
  sm_2010, 
  musk_collar, 
  buffer_dist = 100000)
sm_2011_crop <- resize_raster_to_gps_points(
  sm_2011, 
  musk_collar, 
  buffer_dist = 100000)
sm_2012_crop <- resize_raster_to_gps_points(
  sm_2012, 
  musk_collar, 
  buffer_dist = 100000)
sm_crop <- c(sm_2007_crop, sm_2008_crop, sm_2009_crop, sm_2010_crop,
             sm_2011_crop, sm_2012_crop)

## now project to UTM Zone 9N crs for future analysis
sm_proj <- crop_and_reproject_to_gps_points(
  sm_crop,
  musk_collar,
  crs_epsg = 32609,
  buffer_dist = 10000
)

writeCDF(sm_2007_crop, "data/processed/sm_2007_crop.nc", overwrite = TRUE)
writeCDF(sm_2008_crop, "data/processed/sm_2008_crop.nc", overwrite = TRUE)
writeCDF(sm_2009_crop, "data/processed/sm_2009_crop.nc", overwrite = TRUE)
writeCDF(sm_2010_crop, "data/processed/sm_2010_crop.nc", overwrite = TRUE)
writeCDF(sm_2011_crop, "data/processed/sm_2011_crop.nc", overwrite = TRUE)
writeCDF(sm_2012_crop, "data/processed/sm_2012_crop.nc", overwrite = TRUE)
writeCDF(sm_proj, "data/processed/sm_proj.nc", overwrite = TRUE)

sm_2007_crop <- terra::rast("data/processed/sm_2007_crop.nc", 
                       drivers="NETCDF")
sm_2008_crop <- terra::rast("data/processed/sm_2008_crop.nc", 
                       drivers="NETCDF")
sm_2009_crop <- terra::rast("data/processed/sm_2009_crop.nc", 
                       drivers="NETCDF")
sm_2010_crop <- terra::rast("data/processed/sm_2010_crop.nc", 
                       drivers="NETCDF")
sm_2011_crop <- terra::rast("data/processed/sm_2011_crop.nc", 
                       drivers="NETCDF")
sm_2012_crop <- terra::rast("data/processed/sm_2012_crop.nc", 
                       drivers="NETCDF")
### Calculate per day averages for snow depth across the study area

### Rasterize the muskox collar points to create zones for zonal statistics
zones <- rasterize(musk_collar %>%
                    sf::st_transform(terra::crs(sm_2007_crop)),
                  sm_2007_crop, fun = "first")

snowdepth_mean <- zonal(sm_2007_crop, zones, "mean", wide = FALSE) %>%
  bind_rows(zonal(sm_2008_crop, zones, "mean", wide = FALSE)) %>%
  bind_rows(zonal(sm_2009_crop, zones, "mean", wide = FALSE)) %>%
  bind_rows(zonal(sm_2010_crop, zones, "mean", wide = FALSE)) %>%
  bind_rows(zonal(sm_2011_crop, zones, "mean", wide = FALSE)) %>%
  bind_rows(zonal(sm_2012_crop, zones, "mean", wide = FALSE)) %>%
  mutate(date = seq( as.Date("2007-01-01"), as.Date("2012-12-31"), by="+1 day"),
         snow_depth = round(value,2)) %>%
  select(date, snow_depth, value)

saveRDS(snowdepth_mean, "data/processed/snowdepth_mean.rds")


### create a column for snow depth for each GPS relocation

temp <- terra::extract(sm_2007_crop, 
                       musk_collar %>%
                  sf::st_transform(terra::crs(sm_2008_crop))) %>%
  pivot_longer(-ID, names_to = c(NA, "year", NA, "day"),
               names_sep = "_", values_to = "snow_depth") %>%
  bind_rows(
    terra::extract(sm_2008_crop, 
                   musk_collar %>%
                     sf::st_transform(terra::crs(sm_2008_crop))) %>%
      pivot_longer(-ID, names_to = c(NA, "year", NA, "day"),
                   names_sep = "_", values_to = "snow_depth")
  ) %>%
  bind_rows(
    terra::extract(sm_2009_crop, 
                   musk_collar %>%
                     sf::st_transform(terra::crs(sm_2008_crop))) %>%
      pivot_longer(-ID, names_to = c(NA, "year", NA, "day"),
                   names_sep = "_", values_to = "snow_depth")
  )%>%
  bind_rows(
    terra::extract(sm_2010_crop, 
                   musk_collar %>%
                     sf::st_transform(terra::crs(sm_2008_crop))) %>%
      pivot_longer(-ID, names_to = c(NA, "year", NA, "day"),
                   names_sep = "_", values_to = "snow_depth")
  )%>%
  bind_rows(
    terra::extract(sm_2011_crop, 
                   musk_collar %>%
                     sf::st_transform(terra::crs(sm_2008_crop))) %>%
      pivot_longer(-ID, names_to = c(NA, "year", NA, "day"),
                   names_sep = "_", values_to = "snow_depth")
  )%>%
  bind_rows(
    terra::extract(sm_2012_crop, 
                   musk_collar %>%
                     sf::st_transform(terra::crs(sm_2008_crop))) %>%
      pivot_longer(-ID, names_to = c(NA, "year", NA, "day"),
                   names_sep = "_", values_to = "snow_depth")
  ) %>%
  group_by(ID) %>%
  mutate(date2 = seq( as.Date("2007-01-01"), 
                     as.Date("2012-12-31"), 
                     by="+1 day")) %>%
  group_by(date2) %>%
  arrange(ID) %>%
  mutate(Id_Number = musk_collar$Id_Number,
         datetime = musk_collar$datetime,
         Date = musk_collar$Date) %>%
  filter(Date == date2) %>%
  ungroup() %>%
  select(Id_Number, datetime, snow_depth)
snowdepth_locs <- musk_collar %>%
  ungroup() %>%
  mutate(snow_depth = temp$snow_depth)

saveRDS(snowdepth_locs, "data/processed/snowdepth_locs.rds")

  
