library(tidyverse)
library(sf)
library(terra)

### load attribute data for land cover values
lc_atts <- read_csv("data/raw/ClassIndex_IndiceDeClasse.csv",
                    col_names = c("Value", "Classification", "RGB"),
                    skip = 1) %>%
  separate(RGB,c("R","G","B"),"; ", convert = TRUE) %>%
  ### remove french classification and convert rgb to hex
  mutate(Classification = str_remove(Classification, "/.*"),
         hex = rgb(R,G,B,maxColorValue = 255))

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds")

### Load land cover Canada 2010 data
lc_2010 <- rast("data/raw/landcover-2010-classification.tif")

### buffer to collar data 1km
buffer <- musk_collar %>%
  st_transform(crs(lc_2010)) %>%
  st_buffer(dist = 10000)

### crop land cover data to collar data buffer
lc_2010_crop <- crop(lc_2010, buffer) %>%
  ### change numeric values to land classification
  subst(lc_atts$Value,lc_atts$Classification) 

saveRDS(lc_2010_crop, "data/processed/lc_2010_crop.rds")


  
