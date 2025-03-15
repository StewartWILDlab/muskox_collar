data <- summarised_month_detections %>%
  left_join(covariate_data, by = "camera_id_column")

### looking for outliers in predictor variables
issa_data_summer_comb |> 
  select(c(mrdtm, dtm_centered, fire_year, open_land)) %>%
  pivot_longer(cols = c(mrdtm, dtm_centered, fire_year, open_land)) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(issa_data_summer_comb), each = 4))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()
  
issa_data_summer_comb |> 
  ggplot() +
  geom_boxplot(aes(y = mrdtm, x = id)) +
  theme_bw()

issa_data_summer_comb |> 
  select(c(nbr, mrdtm, dtm_centered, fire_year, open_land)) %>%
  cor()

mod <- lm(case_ ~ nbr + mrdtm + dtm_centered + fire_year + open_land,
          data = issa_data_summer_comb)
car::vif(mod)

Z <- as.vector(as.matrix(issa_data_summer_comb[1:10000, c("mrdtm", "dtm_centered",
                                       "fire_year", "open_land")]))
Y10 <- rep(issa_data_summer_comb$nbr[1:10000], 4)
MyNames <- names(issa_data_summer_comb[1:10000,c("mrdtm", "dtm_centered", "fire_year",
                              "open_land")])
ID10 <- rep(MyNames, each = length(issa_data_summer_comb$nbr[1:10000]))
temp <- tibble(response = Y10, vars = Z, varnames = ID10) %>%
  mutate(response_binned = case_when(
    response == 0 ~ "0",
    response <= 50 ~ "0-50",
    TRUE ~ ">50"
  ))

temp %>%
  ggplot(aes(x = vars, y = response_binned)) +
  geom_boxplot() +
  facet_wrap(~varnames, scales = "free")


