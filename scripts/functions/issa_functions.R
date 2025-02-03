######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Functions to automate data/results prep for ISSA models

### Determine predictor varaibles used in model
find_predictor_vars <- function(issa_model){
  vars <- colnames(issa_model$frame) %>%
    str_remove("scale\\(") %>%
    str_remove("\\)")
  r_vars <- unique(issa_model$modelInfo$grpVar)
  p_vars <- vars[which(!(vars%in%c(r_vars)))][-1]
  return(p_vars)
}

### Create predictions for a covariate range based on a fitted ISSA model
generate_data_for_issa_predictions <- function(issa_model, data, covariate = NA){
  p_vars <- find_predictor_vars(issa_model)
  ### remove covariate of interest
  p_vars <- p_vars[which(!(p_vars%in%c(covariate)))]
  new_data <- data %>%
    ### calculate variable means
    reframe(across(p_vars,~ if(length(unique(.x))<=2) 0 else mean(.x))) %>%
    expand(!!!syms(p_vars), 
           strat_id = NA, 
           id = unique(data$id))
  ### calculate range for variable of interest
  if(!is.na(covariate)){
    new_data <- new_data %>%
      expand(!!!syms(names(new_data)),
             newvar = data %>% 
               pull(covariate) %>% 
               modelr::seq_range(20)) %>%
      rename(!!covariate := newvar)
  }
  return(new_data)
}

generate_issa_predictions <- function(issa_model, data, covariate = NA){
  newdata <- generate_data_for_issa_predictions(issa_model, data, covariate = NA)
  preds <- predict(issa_model,
                   newdata = newdata,
                   type = "link",
                   re.form = NULL,
                   allow.new.levels=TRUE)
  output <- newdata %>%
    mutate(pred = preds)
  return(output)
}

generate_issa_predictions(m0_summer_glmmfit, issa_data_summer_comb, NA)  
