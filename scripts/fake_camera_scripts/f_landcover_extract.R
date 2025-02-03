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

### load and crop land cover raster data
lc_raster <- rast("data/raw/landcover/landcover-2010-classification.tif") 
lc_raster_crop <- lc_raster %>%
  crop(camera_buffer %>% st_transform(crs(lc_raster))) %>%
  project("EPSG:32609", method = "near")

landcover_proportions_camera_buffer <- function(camera_id){
  camera_buff_vect <- camera_buffer %>%
    filter(new_id == camera_id)
  lc_raster_crop2 <- terra::crop(lc_raster_crop, camera_buff_vect)
  camera_buffer_rast <- rasterize(camera_buff_vect %>% vect(), 
                                  lc_raster_crop2, 
                                  field = "new_id")
  ct <- crosstab(c(camera_buffer_rast, lc_raster_crop2))
}

lc_camera_props <- map(camera_buffer$new_id[1:10], function(x){
  landcover_proportions_camera_buffer(x)
})

lc_camera_props_df <- map(lc_camera_props, function(x){
  x %>% as_tibble()
}) %>%
  list_rbind() %>%
  group_by(new_id) %>%
  mutate(prop = round(n/sum(n),2)) %>%
  select(-n) %>%
  pivot_wider(names_from = "Canada2010", values_from = "prop", values_fill = 0)

         