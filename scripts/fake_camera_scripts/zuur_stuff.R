data <- summarised_month_detections %>%
  left_join(covariate_data, by = "camera_id_column")

### looking for outliers in predictor variables
issa_data_summer_comb |> 
  select(c(scanfi_prcC_end, mrdtm, dtm_centered, fire_year, open_land)) %>%
  pivot_longer(cols = c(scanfi_prcC_end, mrdtm, dtm_centered, fire_year, open_land)) |> 
  ggplot() +
  geom_point(aes(x = value, y = rep(1:nrow(issa_data_summer_comb), each = 5))) +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Value of the variable",
       y = "Order of the data") +
  theme_bw()
  
issa_data_summer_comb |> 
  ggplot() +
  geom_boxplot(aes(y = mrdtm, x = id)) +
  theme_bw()

issa_data_summer_comb |> 
  select(c(scanfi_prcC_end, mrdtm, dtm_centered, fire_year, open_land)) %>%
  cor()

mod <- lm(case_ ~ scanfi_prcC_end + mrdtm + dtm_centered + fire_year + open_land,
          data = issa_data_summer_comb)
car::vif(mod)
