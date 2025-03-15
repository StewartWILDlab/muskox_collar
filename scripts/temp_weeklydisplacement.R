musk_collar_filt <- readRDS(here("data/processed/musk_collar_filt.rds")) %>%
  sf::st_transform(32609)

musk_collar_filt2 <- musk_collar_filt %>%
  mutate(geometry_week = geometry) %>%
  sf::st_drop_geometry()

temp <- musk_collar_filt %>%
  inner_join(musk_collar_filt2, by = "Id_Number", 
             suffix = c("", ".week"),
             relationship = "many-to-many") %>%
  mutate(time_diff_days = as.numeric(difftime(datetime.week, datetime, units = "days"))) %>%
  filter(time_diff_days >= 6.9, time_diff_days <= 7.1)

temp2 <- temp %>%
  mutate(displacement = st_distance(geometry, geometry_week, by_element = TRUE, which = "Euclidean"),
         season = ifelse(month>=5&month<=10,"summer","winter")) %>%
  group_by(season) %>%
  summarise(mdisplace = median(displacement))

