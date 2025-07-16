######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviours below treeline

######################################################
### Script to download land cover data and associated metadata from 
### https://open.canada.ca/data/en/dataset/c688b87f-e85f-4842-b0e1-a8f79ebf1133

library(tidyverse)

### Create folders for data if they don't exist
dir.create("data/raw/landcover", showWarnings = FALSE)
dir.create("data/raw/geography", showWarnings = FALSE)
dir.create("data/raw/MRDEM", showWarnings = FALSE)
dir.create("data/raw/SCANFI", showWarnings = FALSE)
dir.create("data/raw/fire", showWarnings = FALSE)
dir.create("data/raw/fire/NBAC", showWarnings = FALSE)
dir.create("data/raw/fire/NFD", showWarnings = FALSE)
dir.create("data/raw/fire/NTEMS", showWarnings = FALSE)
dir.create("data/raw/weather", showWarnings = FALSE)
dir.create("data/raw/weather/station_data", showWarnings = FALSE)

### download 2010 Land Cover of Canada .tif
tif_url <- "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/land/landcover/landcover-2010-classification.tif"
options(timeout = max(2000, getOption("timeout")))
download.file(tif_url, "data/raw/landcover/landcover-2010-classification.tif", mode = "wb")

### download landcover metadata
meta_url <- "https://open.canada.ca/data/en/dataset/c688b87f-e85f-4842-b0e1-a8f79ebf1133.xml"
download.file(meta_url, "data/raw/landcover/landcover-2010-classification.tif2.xml")

### download Medium Resolution Digital Elevation Model (MRDEM) from Govt of Canada
dem_url <- "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/store/elevation/mrdem/mrdem-30/mrdem-30-dtm.tif"
options(timeout = max(2000, getOption("timeout")))
download.file(dem_url, "data/raw/MRDEM/mrdtm.tif", mode = "wb")

### download DEM metadata
dem_meta_url <- "https://open.canada.ca/data/en/dataset/18752265-bda3-498c-a4ba-9dfe68cb98da.xml"
download.file(dem_meta_url, "data/raw/MRDEM/mrdtm.tif.xml")

### download NWT place names
names_url <- "https://ftp.cartes.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_shp_eng/cgn_nt_shp_eng.zip"
download.file(names_url, "data/raw/nwt_names/nwt_names.zip")
unzip("data/raw/nwt_names/nwt_names.zip", exdir = "data/raw/nwt_names")


### download SCANFI datasets
### Tree canopy cover
scanfi_pc_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_prcC_other_SW_2020_v1.2.tif"
download.file(scanfi_pc_url, "data/raw/SCANFI/scanfi_prcC.tif")
scanfi_pc_meta_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_prcC_other_SW_2020_v1.2.tif.aux.xml"
download.file(scanfi_pc_meta_url, "data/raw/SCANFI/scanfi_prcC.tif.xml")
scanfi_pc_ovr_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_prcC_other_SW_2020_v1.2.tif.ovr"
download.file(scanfi_pc_ovr_url, "data/raw/SCANFI/scanfi_prcC.tif.ovr")
### Biomass
scanfi_biomass_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_biomass_SW_2020_v1.2.tif"
download.file(scanfi_biomass_url, "data/raw/SCANFI/scanfi_biomass.tif")
scanfi_biomass_meta_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_biomass_SW_2020_v1.2.tif.aux.xml"
download.file(scanfi_biomass_meta_url, "data/raw/SCANFI/scanfi_biomass.tif.xml")
scanfi_biomass_ovr_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_biomass_SW_2020_v1.2.tif.ovr"
download.file(scanfi_biomass_ovr_url, "data/raw/SCANFI/scanfi_biomass.tif.ovr")
### Land cover
scanfi_lc_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif"
download.file(scanfi_lc_url, "data/raw/SCANFI/scanfi_lc.tif")
scanfi_lc_meta_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif.aux.xml"
download.file(scanfi_lc_meta_url, "data/raw/SCANFI/scanfi_lc.tif.xml")
scanfi_lc_ovr_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif.ovr"
download.file(scanfi_lc_ovr_url, "data/raw/SCANFI/scanfi_lc.tif.ovr")
scanfi_lc_cpg_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif.vat.cpg"
download.file(scanfi_lc_cpg_url, "data/raw/SCANFI/scanfi_lc.tif.vat.cpg")
scanfi_lc_dbf_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif.vat.dbf"
download.file(scanfi_lc_dbf_url, "data/raw/SCANFI/scanfi_lc.tif.vat.dbf")


### Download water body data
water_url <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/canvec_50K_NT_Hydro_shp.zip"
download.file(water_url, "data/raw/geography/waterbodies/nwt_water.zip")
unzip("data/raw/geography/waterbodies/nwt_names.zip", exdir = "data/raw/geography/waterbodies")


### Download historical fire polygons from NBAC (1972 - 2023)
fire_year_url <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_1972_2023_20240530_shp.zip"
fire_year_meta_url <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_1972_2023_20240530_shp_metadata.pdf"
download.file(fire_year_url, "data/raw/fire/NBAC/nbac_fire_year.shp.zip")
download.file(fire_year_meta_url, "data/raw/fire/NBAC/nbac_fire_year.pdf")
unzip("data/raw/fire/NBAC/nbac_fire_year.shp.zip", exdir = "data/raw/fire/NBAC")

### Download historical fire polygons from NFD (1965 - 2023)
fire_year_url <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"
download.file(fire_year_url, "data/raw/fire/NFD/nfd_fire_year.zip")
unzip("data/raw/fire/NFD/nfd_fire_year.zip", exdir = "data/raw/fire/NFD")


### Download historical fire data from NTEMS (1985 - 2020)
# fire_year_url <- "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Fire_1985-2020.zip"
fire_nbr_url <- "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Wildfire_dNBR_1985_2020.zip"
# download.file(fire_year_url, "data/raw/fire/NTEMS/ntems_fire_year.zip")
download.file(fire_nbr_url, "data/raw/fire/NTEMS/ntems_fire_nbr.zip")
# unzip("data/raw/fire/NTEMS/ntems_fire_year.zip", exdir = "data/raw/fire/NTEMS")
unzip("data/raw/fire/NTEMS/ntems_fire_nbr.zip", exdir = "data/raw/fire/NTEMS")


### Download climate station data
## climate station locations
climate_stations_url <- "https://dd.weather.gc.ca/climate/observations/climate_station_list.csv"
download.file(climate_stations_url, "data/raw/weather/climate_station_list.csv")
nwt_station_files_url <- "https://dd.weather.gc.ca/climate/observations/daily/csv/NT/"
nwt_station_files <- RCurl::getURL(nwt_station_files_url, 
                                   ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
  str_extract_all("(?<=>)climate_daily_NT.*?.csv(?=<)") %>%
  unlist() %>%
  as_tibble() %>%
  mutate(year = str_split_i(value, "_", 5),
         year = substr(year,1,4),
         year = as.numeric(year)) %>%
  filter(year>=2007, year<=2012)
for(i in 1:nrow(nwt_station_files)){
  download.file(str_c(nwt_station_files_url,nwt_station_files$value[i]), 
                str_c("data/raw/weather/station_data/",nwt_station_files$value[i]))
}


