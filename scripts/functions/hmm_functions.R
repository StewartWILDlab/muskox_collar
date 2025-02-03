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

### The following two functions were pulled directly from momentuHMM package
### in order to use the viterbi function on the multiple imputation results

formatmiSum <- function(miSum){
  miSum$mle <- lapply(miSum$Par$real,function(x) x$est)
  miSum$mle$beta <- miSum$Par$beta$beta$est
  miSum$mle[["pi"]] <- miSum$Par$real[["pi"]]$est
  miSum$mle$delta <- miSum$Par$real$delta$est
  miSum$mod <- list()
  if(!is.null(miSum$conditions$recharge)){
    nbRecovs <- ncol(stats::model.matrix(miSum$conditions$recharge$g0,miSum$g0covs))-1
    nbG0covs <- ncol(stats::model.matrix(miSum$conditions$recharge$theta,miSum$reCovs))-1
    miSum$mle$g0 <- c(miSum$Par$beta$g0$est)
    names(miSum$mle$g0) <- colnames(miSum$Par$beta$g0$est)
    miSum$mle$theta <- c(miSum$Par$beta$theta$est)
    names(miSum$mle$theta) <- colnames(miSum$Par$beta$theta$est)
  } else nbRecovs <- nbG0covs <- 0
  miSum$mod$estimate <- expandPar(miSum$MIcombine$coefficients,miSum$conditions$optInd,unlist(miSum$conditions$fixPar),miSum$conditions$wparIndex,miSum$conditions$betaCons,miSum$conditions$deltaCons,length(miSum$stateNames),ncol(miSum$covsDelta)-1,miSum$conditions$stationary,nrow(miSum$Par$beta$beta$est)/miSum$conditions$mixtures-1,nbRecovs+nbG0covs,miSum$conditions$mixtures,ncol(miSum$covsPi)-1)
  miSum$mod$wpar <- miSum$MIcombine$coefficients
  miSum
}

expandPar <- function(optPar,optInd,fixPar,wparIndex,betaCons,deltaCons,nbStates,nbCovsDelta,stationary,nbCovs,nbRecovs=0,mixtures=1,nbCovsPi=0){
  if(length(optInd)){
    wpar <- numeric(length(fixPar))
    wpar[-optInd] <- optPar
    if(length(wparIndex)) wpar[wparIndex] <- fixPar[wparIndex]
    if(nbStates>1){
      if(!is.null(betaCons)){
        foo <- length(wpar)-(nbCovsDelta+1)*(nbStates-1)*(!stationary)*mixtures-ifelse(nbRecovs,nbRecovs+2,0)-(nbCovsPi+1)*(mixtures-1)-((nbCovs+1)*nbStates*(nbStates-1)*mixtures-1):0
        wpar[foo] <- wpar[foo][betaCons]
      }
      if(!is.null(deltaCons)){
        foo <- length(wpar)-ifelse(nbRecovs,nbRecovs+2,0)-((nbCovsDelta+1)*(nbStates-1)*mixtures-1):0
        wpar[foo] <- wpar[foo][deltaCons]
      }
    }
  } else {
    wpar <- optPar
  }
  wpar
}

### function to extract states from fitted MIhmm model
extract_hmm_states <- function(model, data){
  mod_formatted <- formatmiSum(model$miSum)
  class(mod_formatted) <- append("momentuHMM",class(mod_formatted))
  states <- viterbi(mod_formatted)
  
  data_with_states <- data %>%
    left_join(
      model$miSum$data %>%
        as_tibble() %>%
        mutate(state = states) %>%
        rename(burst_id = ID) %>%
        select(burst_id, datetime, state) 
    )
  return(data_with_states)
}

plot_hmm_states <- function(data){
  lines <- data %>%
    drop_na(state) %>%
    group_by(burst_id) %>%
    arrange(datetime) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  plot <- data %>% 
    drop_na(state) %>%
    ggplot() +
    geom_sf(data = lines) +
    geom_sf(aes(colour = factor(state))) +
    theme_bw()
  
  return(plot)
}
