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
source("scripts/functions/raster_prep_functions.R")


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

cats <- data.frame(ids = lc_atts$Value, cover = lc_atts$Classification)
coltab <- data.frame(ids = lc_atts$Value, cols = lc_atts$hex)

### Load muskox collar data in Sahtu
musk_collar <- readRDS("data/processed/musk_collar.rds")

### Load land cover Canada 2010 data
lc_2010 <- terra::rast("data/raw/landcover/landcover-2010-classification.tif")
levels(lc_2010) <- cats
coltab(lc_2010) <- coltab

### resize raster
lc_2010_crop <- resize_raster_to_gps_points(
  lc_2010, 
  musk_collar, 
  buffer_dist = 100000)



## now project land cover to same crs as location data for plotting
## IMPORTANT: only use this for plotting as the numeric values will not match up anymore
lc_2010_proj <- crop_and_reproject_to_gps_points(
  lc_2010_crop,
  musk_collar,
  crs_epsg = 32609,
  buffer_dist = 10000
)

### create simplified landcover labels
simp_classes <- c("Needleleaf forest","Mixed forest", "Shrubland", "Grassland", 
                  "Wetland", "Cropland", "Barren","Urban", "Water", "Snow")
simp_cols <- c("#003D00", "#5C752B", "#B38A33", "#E0CF8A", "#6BA38A", "#E6AD66",
               "#A8ABAE", "#DC2126", "#4C70A3", "#FFFAFF")
simp_ids <- c(1,5,8,10, 14,15,16,17,18,19)
cats <- data.frame(ids = simp_ids, cover = simp_classes)
coltab <- data.frame(ids = simp_ids, cols = simp_cols)
lc_2010_proj_simp <- subst(lc_2010_proj, 
                           c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19),
                           c(1,1,5,5,8,10,8,10,16,14,15,16,17,18,19),
                           raw = TRUE)
levels(lc_2010_proj_simp) <- cats
coltab(lc_2010_proj_simp) <- coltab

writeRaster(lc_2010_crop, "data/processed/lc_2010_crop.tif", overwrite = TRUE)
writeRaster(lc_2010_proj, "data/processed/lc_2010_proj.tif", overwrite = TRUE)
writeRaster(lc_2010_proj_simp, "data/processed/lc_2010_proj_simp.tif", overwrite = TRUE)


  
