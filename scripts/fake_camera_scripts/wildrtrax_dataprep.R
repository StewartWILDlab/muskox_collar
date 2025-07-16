# remotes::install_github("ABbiodiversity/wildrtrax")
# library(wildrtrax)
library(tidyverse)

#Authenticate in WildTrax
Sys.setenv(WT_USERNAME = 'niluymes', WT_PASSWORD = 'Ntu7bGdmYj89!7T')
wt_auth(force = TRUE)

# Download the tag report
images <- wt_download_report(project_id = 2431, sensor_id = 'CAM', reports = "main", weather_cols = F) %>%
  tibble::as_tibble()

# Remove non-wildlife tags
raw_data <- images %>%
  filter(species_common_name != "STAFF/SETUP", species_common_name != "NONE")

# Download the location report
cameras <- wt_download_report(project_id = 2431, sensor_id = 'CAM', reports = "location", weather_cols = F) %>%
  tibble::as_tibble() %>%
  drop_na(latitude)

#Download the image set
dep <- wt_download_report(project_id = 2431, sensor_id = 'CAM', reports = "image_set", weather_cols = F) %>%
  tibble::as_tibble()

### Determine independent detections (removes pictures taken within x minutes of each other)
ind_det <- wt_ind_detect(
  x = raw_data, 
  threshold = 30,
  units = "minutes",
  remove_human = TRUE, 
  remove_domestic = TRUE 
)

### summarise independent detections
summarised_month_detections <- wt_summarise_cam(
  # Supply your detection data
  detect_data = ind_det,
  # Supply your raw image data
  raw_data = raw_data,
  # Now specify the time interval you're interested in 
  time_interval = "month",
  # What variable are you interested in?
  variable = "detections",
  # Your desired output format (wide or long) 
  output_format = "wide")
