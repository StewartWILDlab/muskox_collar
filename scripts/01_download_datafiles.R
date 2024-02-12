######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to download land cover data and associated metadata from 
### https://open.canada.ca/data/en/dataset/c688b87f-e85f-4842-b0e1-a8f79ebf1133

### download 2010 Land Cover of Canada .tif
tif_url <- "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/land/landcover/landcover-2010-classification.tif"
options(timeout = max(2000, getOption("timeout")))
download.file(tif_url, "data/raw/landcover/landcover-2010-classification.tif", mode = "wb")

### download metadata
meta_url <- "https://open.canada.ca/data/en/dataset/c688b87f-e85f-4842-b0e1-a8f79ebf1133.xml"
download.file(meta_url, "data/raw/landcover/landcover-2010-classification.tif2.xml")
