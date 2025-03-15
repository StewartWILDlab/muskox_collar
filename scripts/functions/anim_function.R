######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Function to produce gifs of muskox movement across different land covers

library(tidyverse)

anim_function <- function(collars, years, frame_ndays) {
  musk_collar_sub <- readRDS("data/processed/musk_collar.rds")  %>%
    filter(Id_Number %in% collars,
           year %in% years) %>%
    sf::st_transform(32609) %>%
    mutate(x = sf::st_coordinates(geometry)[,1],
           y = sf::st_coordinates(geometry)[,2],
           new_date = format(datetime, "%B %d %Y"))
  musk_collar_sub <- musk_collar_sub[seq(1,nrow(musk_collar_sub), 3*frame_ndays),]
  
  lc_2010_crop <- terra::rast("data/processed/lc_2010_proj_simp.tif") %>%
    terra::crop(musk_collar_sub)
  
  dtm_crop <- terra::rast("data/processed/mrdtm_proj.tif") %>%
    terra::crop(musk_collar_sub)
  
  r_ext <- terra::ext(lc_2010_crop)
  aspect_ratio = (r_ext$ymax - r_ext$ymin)/(r_ext$xmax - r_ext$xmin)
  
  # lc_atts <- readRDS("data/processed/lc_atts.rds")
  # cols <- as.character(lc_atts$hex)
  # names(cols) <- as.character(lc_atts$Classification)
  
  points <- musk_collar_sub %>%
    arrange(datetime) %>%
    ggplot() +
    tidyterra::geom_spatraster(data = lc_2010_crop) +
    # tidyterra::geom_spatraster(data = dtm_crop,
    #                            aes(alpha = after_stat(value)),
    #                            fill = "black") +
    geom_point(aes(x = x,
                   # colour = month,
                   y = y),
               colour = "brown4",
               size = 5) +
    geom_path(aes(x = x,
                  # colour = month,
                  y = y),
              linewidth = 1.5, alpha = 0.5) +
    labs(fill = "Land cover", colour = "Month") +
    scale_alpha_continuous(range = c(0.7,0), breaks = 2) +
    guides(alpha = "none") +
    # scale_color_gradientn(
    #   colours = c("blue","blue","red", "red", "blue"),
    #   limits = c(1,12)) +
    theme_void() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.87,0.75),
          legend.background = element_rect(fill="white"),
          legend.margin = margin(c(5,5,5,5)))
  
  anim <- points +
    gganimate::transition_reveal(along = datetime) +
    labs(title = 'Date: {format(frame_along, "%B %Y")}') +
    gganimate::ease_aes('linear')
  
  gganimate::animate(anim, fps = 5, 
                     nframes = round(nrow(musk_collar_sub)), 
                     width = 8, height = 8*aspect_ratio, 
                     units = "in", res = 150)
  dir.create("output/anim", showWarnings = FALSE)
  gganimate::anim_save(str_c("output/anim/collar",str_c(collars,"_",years,"_"),".gif"))
  
}
