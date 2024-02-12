######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Function to produce gifs of muskox movement across different land covers

library(tidyverse)

anim_function <- function(collar) {
  musk_collar_sub <- readRDS("data/processed/musk_collar.rds")  %>%
    filter(Id_Number == collar)
  
  lc_2010_crop <- readRDS("data/processed/lc_2010_crop.rds") %>%
    terra::project(y = "epsg:4326") %>%
    terra::crop(musk_collar_sub)
  
  points <- musk_collar_sub %>%
    arrange(datetime) %>%
    ggplot() +
    tidyterra::geom_spatraster(data = lc_2010_crop) +
    coord_sf(crs = st_crs(musk_collar_sub)) +
    geom_point(aes(x = Longitude, y = Latitude, colour = month),
               size = 3) +
    geom_path(aes(x = Longitude, y = Latitude, colour = month), 
              size = 1, alpha = 0.3) +
    labs(fill = "", colour = "Month") +
    scale_color_gradientn(
      colours = c("blue","blue","red", "red", "blue"),
      limits = c(1,12))
  
  anim <- points +
    gganimate::transition_reveal(along = datetime) +
    labs(title = 'Date: {frame_along}') +
    gganimate::ease_aes('linear')
  
  gganimate::animate(anim, fps = 5, nframes = round(nrow(musk_collar_sub)/3), 
                     height = 8, width = 10, units = "in", res = 150)
  gganimate::anim_save(str_c("output/anim/collar",collar,".gif"))
  
}