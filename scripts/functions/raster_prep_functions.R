######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Functions to crop and reproject raster data to the same coordinate system

### This function projects gps points to the CRS used by raster and 
### uses a buffer to reduce raster size for reprojection
resize_raster_to_gps_points <- function(rast, gps_points, buffer_dist) {
  buffer <- gps_points  %>%
    sf::st_transform(terra::crs(rast)) %>%
    sf::st_buffer(dist = buffer_dist)
  rast_crop <- terra::crop(rast, buffer)
  return(rast_crop)
}

crop_and_reproject_to_gps_points <- function(rast, gps_points, crs_epsg, buffer_dist){
  buffer <- gps_points  %>%
    sf::st_transform(crs_epsg) %>%
    sf::st_buffer(dist = buffer_dist)
  rast_prj <- rast %>%
    terra::project(str_c("EPSG:",crs_epsg)) %>%
    terra::crop(buffer)
  return(rast_prj)
}

rasterize_by_date <- function(vec, datetime_col, date, 
                              max_years, min_years,
                              temp_rast){
  print(date)
  rast <- vec %>%
    rename("datetime":={{datetime_col}}) %>%
    filter(datetime >= date - years(max_years),
           datetime <= date - years(min_years)) %>%
    rasterize(temp_rast) 
  time(rast) <- date 
  return(rast)
}

rasterize_fire_year <- function(vec, year_col, date, 
                              temp_rast){
  print(date)
  min_year = vec %>%
    pull(year_col) %>%
    min(na.rm = TRUE)
  rast <- vec %>%
    rename("year":={{year_col}}) %>%
    mutate(year_diff = year(date) - year) %>%
    filter(year_diff > 0) %>%
    rasterize(temp_rast,
              field = "year_diff",
              fun = "min",
              background = year(date) - min_year) 
  time(rast) <- date 
  return(rast)
}

save_raster_to_folder <- function(raster, folder_name, date){
  dir.create(str_c("data/processed/",folder_name), showWarnings = FALSE)
  writeRaster(raster, 
              str_c("data/processed/",folder_name,"/",date,".tif"),
              overwrite = TRUE)
}


