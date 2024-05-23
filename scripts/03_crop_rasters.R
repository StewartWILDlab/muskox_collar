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

### load attribute data for land cover values
lc_atts <- read_csv("data/raw/landcover/ClassIndex_IndiceDeClasse.csv",
                    col_names = c("Value", "Classification", "RGB"),
                    skip = 1) %>%
  separate(RGB,c("R","G","B"),"; ", convert = TRUE) %>%
  ### remove french classification and convert rgb to hex
  mutate(Classification = str_replace_all(Classification, "[^[:alnum:]^/^-]", " "),
    Classification = str_remove_all(Classification, "/.*"),
    Classification = fct_reorder(Classification, Value),
         hex = rgb(R,G,B,maxColorValue = 255))
saveRDS(lc_atts, "data/processed/lc_atts.rds")

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds")

### Load land cover Canada 2010 data
lc_2010 <- terra::rast("data/raw/landcover/landcover-2010-classification.tif")

### buffer collar data 1km
buffer <- musk_collar %>%
  sf::st_transform(terra::crs(lc_2010)) %>%
  sf::st_buffer(dist = 100000)

### crop land cover data to collar data buffer
lc_2010_crop <- terra::crop(lc_2010, buffer) %>%
  ### change numeric values to land classification
  subst(lc_atts$Value,lc_atts$Classification) 

## now project land cover to same crs as location data for plotting
lc_2010_proj <- lc_2010_crop %>%
  terra::project(y = "epsg:4326") %>%
  terra::crop(musk_collar %>% sf::st_buffer(dist = 10000))

writeRaster(lc_2010_crop, "data/processed/lc_2010_crop.tif", overwrite = TRUE)
writeRaster(lc_2010_proj, "data/processed/lc_2010_proj.tif", overwrite = TRUE)


  
