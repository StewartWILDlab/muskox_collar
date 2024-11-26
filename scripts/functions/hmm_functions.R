######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Functions to prepare and process collar data for hidden markov models of
### behavioural states

### identify the minimum year associated with each season (mainly important for
### winter)
minimum_year_season <- function(years, dates, seasons){
  year_min <- ifelse(seasons == "winter" & lubridate::month(dates)<8, years -1, years)
  return(year_min)
}

winter_start_date <- function(dates, snow_depths, depth_threshold){
  ### subset entries that match the criteria for the start of winter,
  ### i.e. consecutive days greater than snow depth threshold
  dates_winter <- ifelse(snow_depths>=depth_threshold&
                             lead(snow_depths,1)>=depth_threshold&
                             lead(snow_depths,2)>=depth_threshold&
                             month(dates)>=7,
                       dates,NA)
  winter_start <-  ifelse(any(!is.na(dates_winter)),min(dates_winter, na.rm = TRUE),NA)
  return(winter_start)
}

summer_start_date <- function(dates, snow_depths, depth_threshold){
  ### subset entries that match the criteria for the start of summer,
  ### i.e. consecutive days less than snow depth threshold
  dates_summer <- ifelse(snow_depths<=depth_threshold&
                           lead(snow_depths,1)<=depth_threshold&
                           lead(snow_depths,2)<=depth_threshold,
                         dates,NA)
  summer_start <-  ifelse(any(!is.na(dates_summer)),min(dates_summer, na.rm = TRUE),NA)
  return(summer_start)
}

season_delineation <- function(dates, snow_depths, depth_threshold){
  winter_start <- winter_start_date(dates,snow_depths,depth_threshold)
  summer_start <- summer_start_date(dates,snow_depths,depth_threshold)
  season <- dplyr::case_when(
    is.na(winter_start)&is.na(summer_start) ~ "winter",
    is.na(winter_start)&dates>=summer_start ~ "summer",
    dates<summer_start ~ "winter",
    dates>=winter_start ~ "winter",
    TRUE ~ "summer"
  )
  return(season)
}

### identify gaps in GPS data and assign unqiue burst IDs
burst_delineation <- function(time_diffs, gap_threshold){
  ### identify gaps in GPS data exceeding given threshold
  burst_positions <- c(1,which(time_diffs > gap_threshold),length(time_diffs)+1)
  ### determine number of data points between each data gap
  burst_lengths <- lead(burst_positions, 1) - burst_positions
  ### assign unique values to each data burst
  bursts <- rep(1:(length(burst_positions)-1), head(burst_lengths,-1))
  return(bursts)
}

### identify the number of missing location points for each burst 
burst_filter <- function(data, id, threshold){
  data_filter <- data %>%
    rename("ID":={{id}}) %>%
    group_by(ID) %>%
    filter(n()>=threshold) %>%
    rename({{id}} := ID) %>%
    ungroup() 
  return(data_filter)
}

### plot bursts for each individual and colour by a length threshold to identify
### which bursts are shorter than the given threshold
burst_plot <- function(data, id, length_threshold){
  plot_data <- data %>%
    rename("ID":={{id}}) %>%
    group_by(ID) %>%
    mutate(burst_length = n(),
           burst_flag = ifelse(burst_length >= length_threshold, 
                               str_c(">=", length_threshold," points"),
                               str_c(">", length_threshold," points"))) %>%
    ungroup()
  plot_data %>%
    ggplot(aes(x = datetime, colour = burst_flag, group = ID, y = snow_depth)) +
    geom_line() +
    geom_point(data = plot_data %>% 
                 group_by(ID) %>%
                 filter(datetime == min(datetime))) +
    facet_wrap(~Id_Number, scales = "free_x") +
    scale_colour_manual(values = c("black", "red")) +
    theme_bw()
}
