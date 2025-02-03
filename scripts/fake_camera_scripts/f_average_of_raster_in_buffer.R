library(tidyverse)
library(sf)
library(terra)

### load in camera locations
camera_points <- sf::st_read("data/raw/muskox_data/Muskox_GPS.shp") %>%
  st_transform(32609)
camera_points <- camera_points[sample(nrow(camera_points),300),]

### create biologically relevent buffer around cameras
camera_buffer <- st_buffer(camera_points, 17000) %>%
  mutate(new_id = 1:n(),
         new_id = str_c("id_",new_id))

### load and crop raster data
elevation <- rast("data/raw/MRDEM/mrdtm.tif")
elevation_crop <- elevation %>%
  crop(camera_buffer %>% st_transform(crs(elevation))) %>%
  project("EPSG:32609", method = "near")

raster_average_camera_buffer <- function(camera_id){
  ### select camera buffer based on camera_id
  camera_buff_vect <- camera_buffer %>%
    filter(new_id == camera_id)
  ### crop raster data to iprove processing speed
  elevation_crop2 <- terra::crop(elevation_crop, camera_buff_vect)
  ### rasterize camera buffer vector
  camera_buffer_rast <- rasterize(camera_buff_vect %>% vect(), 
                                  elevation_crop2, 
                                  field = "new_id")
  ### use zonal statistics to calculate mean of raster in buffer
  averages <- zonal(elevation_crop2, camera_buffer_rast, "mean", wide = FALSE, na.rm = TRUE)
}

### run function for each camera buffer
lc_camera_means <- map(camera_buffer$new_id, function(x){
  raster_average_camera_buffer(x)
})

### transform output into dataframe
lc_camera_means_df <- map(lc_camera_means, function(x){
  x %>% as_tibble()
}) %>%
  list_rbind() %>%
  rename(elevation = value)

# saveRDS(lc_camera_means_df, "data/processed/lc_camera_means_df.rds")

