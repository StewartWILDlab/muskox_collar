library(MODIStsp)
MODIStsp(gui             = FALSE, 
         out_folder      = "data/raw/MODIS_snow", 
         selprod         = "M*D10_A2",
         bandsel         = "Eight Day Snow Cover (bitfield)", 
         user            = "mstp_test" ,
         password        = "MSTP_test_01",
         start_date      = "2008.01.01", 
         end_date        = "2012.12.31", 
         verbose         = FALSE)
MODIStsp_get_prodlayers("M*D10_A2")
