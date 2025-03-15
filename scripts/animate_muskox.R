######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviours below treeline

######################################################
### Script to create and save animations of each collared muskox

library(tidyverse)

musk_collar <- readRDS("data/processed/musk_collar.rds")

source("scripts/functions/anim_function.R")

anim_function(7012, 2011, 1)

for (collar in unique(musk_collar$Id_Number)) {
  anim_function(collar)
}


