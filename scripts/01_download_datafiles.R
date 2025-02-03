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
dir.create("data/raw/geography", showWarnings = FALSE)
dir.create("data/raw/MRDEM", showWarnings = FALSE)
dir.create("data/raw/SCANFI", showWarnings = FALSE)

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


### Download historical fire data

