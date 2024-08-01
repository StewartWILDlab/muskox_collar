######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviours below treeline

######################################################
### Script to download land cover data and associated metadata from 
### https://open.canada.ca/data/en/dataset/c688b87f-e85f-4842-b0e1-a8f79ebf1133

### Create folders for data if they don't exist
dir.create("data/raw/landcover", showWarnings = FALSE)
dir.create("data/raw/nwt_names", showWarnings = FALSE)

### download 2010 Land Cover of Canada .tif
tif_url <- "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/land/landcover/landcover-2010-classification.tif"
options(timeout = max(2000, getOption("timeout")))
download.file(tif_url, "data/raw/landcover/landcover-2010-classification.tif", mode = "wb")

### download metadata
meta_url <- "https://open.canada.ca/data/en/dataset/c688b87f-e85f-4842-b0e1-a8f79ebf1133.xml"
download.file(meta_url, "data/raw/landcover/landcover-2010-classification.tif2.xml")

### download NWT place names
names_url <- "https://ftp.cartes.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_shp_eng/cgn_nt_shp_eng.zip"
download.file(names_url, "data/raw/nwt_names/nwt_names.zip")
unzip("data/raw/nwt_names/nwt_names.zip", exdir = "data/raw/nwt_names")


### download Medium Resolution Digital Elevation Model (MRDEM) from Govt of Canada
dtm_url <- "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/elevation/mrdem/mrdem-30/mrdem-30-dtm.tif"
options(timeout = max(2000, getOption("timeout")))
download.file(dtm_url, "data/raw/MRDEM/mrdtm.tif")
temp <- terra::rast(dtm_url)

