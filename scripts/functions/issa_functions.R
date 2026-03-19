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

## Tidy model results and identify model terms
extract_model_coefs <- function(model){
  coefficients <- broom.mixed::tidy(model) %>%
    select(-group, -statistic, -p.value) %>%
    mutate(term = str_remove(term, "sd__")) %>%
    pivot_wider(names_from = effect, values_from = c(estimate, std.error)) %>%
    filter(term != "(Intercept)") %>%
    mutate(variable = str_remove_all(term, "scale\\("),
           variable = str_remove_all(variable, "\\)"),
           variable = str_replace(variable, ":", "*"))
  return(coefficients)
}
  
## Convert model terms back to variable names used in datasets
extract_model_variables <- function(coefficients){
  vars <- tibble(par = coefficients$term) %>%
    mutate(variable = str_split(par, ":")) %>%
    unnest(variable) %>%
    mutate(scale = str_detect(variable, "scale"),
           variable = str_remove_all(variable, "scale\\("),
           variable = str_remove_all(variable, "\\)")) %>%
    select(variable, scale) %>%
    distinct()
  return(vars)
}

## Scale relevant variables
scale_data <- function(data, variables){
  new_data <- data %>%
    select(id, step_id_, case_, variables$variable) %>%
    mutate(across(variables %>% filter(scale == TRUE) %>% pull(variable), ~scale(.x)[,1]))
  return(new_data)
}  

generate_uhc_predictions <- function(model, data, test_collars, nsim = 100) {
  coefs <- extract_model_coefs(model)
  vars <- extract_model_variables(coefs)
  new_data <- scale_data(data, vars) %>%
    filter(id %in% test_collars)

  preds = list()
  preds[[1]] <- new_data %>%
    mutate(data = "observed") %>%
    filter(case_ == TRUE)
  for(i in 2:(nsim+1)){
    new_coefs <- coefs %>%
      mutate(new_est = rnorm(n(), estimate_fixed, std.error_fixed),
             new_est_r = rnorm(n(), new_est, estimate_ran_pars),
             formula = str_c(new_est_r, "*", variable))
    formula = str_c(new_coefs$formula, collapse = " + ")
    variables = str_c(vars$variable, collapse = ", ")
    func_string = str_c("function(",variables,"){", formula,"}")
    func = eval(parse(text = func_string))
    preds[[i]] <- new_data %>%
      mutate(pred = pmap_dbl(new_data %>% select(vars$variable), func),
             sim = i,
             data = "simulation") %>%
      group_by(id, step_id_) %>%
      mutate(pred_standard = exp(pred)/sum(exp(pred), na.rm = TRUE)) %>%
      slice_sample(n = 1, weight_by = pred_standard)
  }
  boot_preds <- bind_rows(preds)
  return(boot_preds)
}
  
    

