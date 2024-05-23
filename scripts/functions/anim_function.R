######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Function to produce gifs of muskox movement across different land covers

library(tidyverse)

anim_function <- function(collars) {
  musk_collar_sub <- readRDS("data/processed/musk_collar.rds")  %>%
    filter(Id_Number %in% collars)
  
  lc_2010_crop <- terra::rast("data/processed/lc_2010_proj.tif") %>%
    terra::crop(musk_collar_sub)
  
  lc_atts <- readRDS("data/processed/lc_atts.rds")
  cols <- as.character(lc_atts$hex)
  names(cols) <- as.character(lc_atts$Classification)
  
  points <- musk_collar_sub %>%
    arrange(datetime) %>%
    ggplot() +
    tidyterra::geom_spatraster(data = lc_2010_crop) +
    coord_sf(crs = sf::st_crs(musk_collar_sub)) +
    geom_point(aes(x = Longitude, y = Latitude, colour = month),
               size = 5) +
    geom_path(aes(x = Longitude, y = Latitude, colour = month), 
              size = 1.5, alpha = 1) +
    labs(fill = "", colour = "Month") +
    scale_fill_manual(values = cols) +
    scale_color_gradientn(
      colours = c("blue","blue","red", "red", "blue"),
      limits = c(1,12))
  
  anim <- points +
    gganimate::transition_reveal(along = datetime) +
    labs(title = 'Date: {frame_along}') +
    gganimate::ease_aes('linear')
  
  gganimate::animate(anim, fps = 5, nframes = round(nrow(musk_collar_sub)/21), 
                     height = 8, width = 10, units = "in", res = 150)
  dir.create("output/anim", showWarnings = FALSE)
  gganimate::anim_save(str_c("output/anim/collar",str_c(collars,"_"),".gif"))
  
}