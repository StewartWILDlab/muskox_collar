######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to produce tables and figures for CRRC presentation

library(tidyverse)
library(here)
library(terra)
library(sf)
library(glmmTMB)

### set factor levels and colours 
ids <- c("706", "708", "7010", "7011", "7012", "7013", "7080")
id_cols <- RColorBrewer::brewer.pal(7,"Set1")
names(id_cols) <- ids
id_colscale <- scale_fill_manual(values = id_cols)
seas <- c("Summer", "Winter", "Calving")

### set theme
theme_proj <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### Load data
lc_2010_proj <- terra::rast("data/processed/lc_2010_proj.tif")
lc_2010_proj_simp <- terra::rast("data/processed/lc_2010_proj_simp.tif")
water_data_crop <- readRDS("data/processed/large_water_data_crop.rds")
fire_data_subset <- readRDS(here("data/processed/fire_data_subset.rds"))
mrdtm_proj <- terra::rast(here("data/processed/mrdtm_proj.tif"))
musk_season_mcp <- readRDS("output/musk_season_mcp.rds") %>%
  sf::st_as_sf() %>%
  mutate(year = str_split_i(id,"_",2),
         season = str_split_i(id,"_",3),
         id = as.character(str_split_i(id,"_",1)),
         id = factor(id, levels = ids))
hr_landcover <- readRDS("output/hr_landcover.rds")
place_names <- sf::read_sf("data/raw/geography/cgn_nt_shp_eng.shp") %>%
  filter(CATEGORY == "Populated Place",
         GEONAME != "Canol") %>%
  st_transform(crs(mrdtm_proj))

### Load ISSA data and models
issa_data_summer_comb <- readRDS(here("data/processed/issa_data_summer_comb.rds"))
m0_summer_glmmfit <- readRDS(here("output/m0_summer_glmmfit.rds"))
issa_data_winter_comb <- readRDS(here("data/processed/issa_data_winter_comb.rds"))
m0_winter_glmmfit <- readRDS(here("output/m0_winter_glmmfit.rds"))

### Fire RSS plot ----

fireyear_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = mean(mrdtm),
                    water = 0,
                    avg_max_temp = mean(avg_max_temp),
                    fire_cat = c("fire_10_low", "fire_10_mod", "fire_10_high",
                                 "fire_20_low", "fire_20_mod", "fire_20_high",
                                 "fire_30", "fire_40"),
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) %>%
  mutate(fire_cat2 = fire_cat) %>%
  pivot_wider(names_from = fire_cat, values_from = value, values_fill = 0)


fireyear_summer_pred <- predict(m0_summer_glmmfit, 
                         newdata = fireyear_summer_data,
                         type = "link",
                         re.form = NULL,
                         allow.new.levels=TRUE,
                         se.fit = TRUE)

fireyear_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = mean(mrdtm),
                    water = 0,
                    snow_depth = mean(snow_depth),
                    avg_min_temp = mean(avg_min_temp),
                    fire_cat = c("fire_10_20", "fire_30", "fire_40"),
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) %>%
  mutate(fire_cat2 = fire_cat) %>%
  pivot_wider(names_from = fire_cat, values_from = value, values_fill = 0)

fireyear_winter_pred <- predict(m0_winter_glmmfit, 
                         newdata = fireyear_winter_data,
                         type = "link",
                         re.form = NULL,
                         allow.new.levels=TRUE,
                         se.fit = TRUE)

fireyear_rss <- fireyear_summer_data %>%
  mutate(pred = fireyear_summer_pred$fit,
         se = fireyear_summer_pred$se.fit) %>%
  mutate(rss = pred,
         season = "Summer") %>%
  bind_rows(fireyear_winter_rss <- fireyear_winter_data %>%
              mutate(pred = fireyear_winter_pred$fit,
                     se = fireyear_winter_pred$se.fit) %>%
              mutate(rss = pred,
                     season = "Winter")) %>%
  mutate(
    fire_year = case_when(
      fire_cat2 %in% c("fire_10_low", "fire_10_mod", "fire_10_high") ~ "1 - 10",
      fire_cat2 %in% c("fire_20_low", "fire_20_mod","fire_20_high") ~ "11 - 20",
      fire_cat2 %in% c("fire_30") ~ "21 - 30",
      fire_cat2 %in% c("fire_40") ~ "31 - 40",
      fire_cat2 %in% c("fire_10_20") ~"1 - 20"),
    fire_severity = case_when(
      fire_cat2 %in% c("fire_10_low", "fire_20_low") ~ "Low",
      fire_cat2 %in% c("fire_10_mod", "fire_20_mod") ~ "Mod",
      fire_cat2 %in% c("fire_10_high", "fire_20_high") ~ "High",
      TRUE ~ "NA"
    ),
    fire_severity = factor(fire_severity, levels = c("Low", "Mod", "High", "NA")),
    rss.high = rss + 1.96*se,
    rss.low = rss - 1.96*se
  ) %>%
  group_by(fire_year, season) %>%
  mutate(width = 0.1*length(unique(fire_severity)),
         width = ifelse(season == "Winter", width*3/4, width)) %>%
  ungroup()

ggplot(data = fireyear_rss %>% filter(id == "newid"), 
       aes(x = fire_year, y = rss,
           colour = fire_severity)) +
  geom_errorbar(aes(ymin = rss.low, ymax = rss.high, width = width),
                position = position_dodge(width = 0.7)) +
  geom_point(size = 3,position = position_dodge(width = 0.7)) +
  # geom_point(aes(group = fire_severity, colour = "individual-level estimates"), 
  #             size = 1,
  #            position = position_dodge(width = 0.7),
  #            data = fireyear_rss %>% filter(id != "newid")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~season, nrow = 1, scales = "free_x") +
  xlab("Years since fire") + 
  ylab("Relative selection strength") +
  labs(colour = "Fire severity") +
  guides(colour = guide_legend(position = "inside")) +
  scale_colour_manual(values = c("#fcae91", "#fb6a4a","#cb181d", "black", "darkgrey"))+
  theme_proj +
  theme(legend.position.inside = c(0.75,0.75))

ggsave("output/figures/fireyear_rss.png", width = 6, height = 4)


### TPI RSS plot ----

tpi_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = modelr::seq_range(tpi, 30, trim = 0.2),
                    mrdtm = mean(mrdtm),
                    water = 0,
                    Wetland = 0,
                    avg_max_temp = mean(avg_max_temp),
                    fire_10_low = 0,
                    fire_10_mod = 0,
                    fire_10_high = 0,
                    fire_20_low = 0,
                    fire_20_mod = 0,
                    fire_20_high = 0,
                    fire_30 = 0,
                    fire_40 = 0,
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) 


tpi_summer_pred <- predict(m0_summer_glmmfit, 
                                newdata = tpi_summer_data,
                                type = "link",
                                re.form = NULL,
                                allow.new.levels=TRUE,
                                se.fit = TRUE)

tpi_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = modelr::seq_range(tpi, 30, trim = 0.2),
                    mrdtm = mean(mrdtm),
                    water = 0,
                    Wetland = 0,
                    snow_depth = mean(snow_depth),
                    avg_min_temp = mean(avg_min_temp),
                    fire_10_20 = 0,
                    fire_30 = 0,
                    fire_40 = 0,                   
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) 

tpi_winter_pred <- predict(m0_winter_glmmfit, 
                                newdata = tpi_winter_data,
                                type = "link",
                                re.form = NULL,
                                allow.new.levels=TRUE,
                                se.fit = TRUE)

tpi_rss <- tpi_summer_data %>%
  mutate(pred = tpi_summer_pred$fit,
         se = tpi_summer_pred$se.fit) %>%
  mutate(rss = pred,
         season = "Summer") %>%
  bind_rows(tpi_winter_data %>%
              mutate(pred = tpi_winter_pred$fit,
                     se = tpi_winter_pred$se.fit) %>%
              mutate(rss = pred,
                     season = "Winter")) %>%
  mutate(
    rss.high = rss + 1.96*se,
    rss.low = rss - 1.96*se
  ) 

ggplot(data = tpi_rss %>% filter(id == "newid"), 
       aes(x = tpi, y = rss, 
           # colour = "population-level estimate"
           )) +
  # geom_line(aes(group = id, colour = "individual-level estimates"),
  #            data = tpi_rss %>% filter(id != "newid")) +
  geom_ribbon(aes(ymin = rss.low, ymax = rss.high), 
              alpha = 0.3, colour = NA) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~season, nrow = 2) +
  xlab("Topographic Position Index") + 
  ylab("Relative selection strength") +
  # scale_colour_manual(values = c("red", "black")) +
  labs(colour = "") +
  theme_proj

ggsave("output/figures/tpi_rss.png", width = 3, height = 4)




### TRI RSS plot ----

tri_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = modelr::seq_range(mrdtm, 30),
                    water = 0,
                    avg_max_temp = mean(avg_max_temp),
                    fire_10_low = 0,
                    fire_10_mod = 0,
                    fire_10_high = 0,
                    fire_20_low = 0,
                    fire_20_mod = 0,
                    fire_20_high = 0,
                    fire_30 = 0,
                    fire_40 = 0,
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) 


tri_summer_pred <- predict(m0_summer_glmmfit, 
                           newdata = tri_summer_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE,
                           se.fit = TRUE)

tri_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = modelr::seq_range(mrdtm, 30),
                    water = 0,
                    snow_depth = mean(snow_depth),
                    avg_min_temp = mean(avg_min_temp),
                    fire_10_20 = 0,
                    fire_30 = 0,
                    fire_40 = 0,                   
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) 

tri_winter_pred <- predict(m0_winter_glmmfit, 
                           newdata = tri_winter_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE,
                           se.fit = TRUE)

tri_rss <- tri_summer_data %>%
  mutate(pred = tri_summer_pred$fit,
         se = tri_summer_pred$se.fit) %>%
  mutate(rss = pred,
         season = "Summer") %>%
  bind_rows(tri_winter_data %>%
              mutate(pred = tri_winter_pred$fit,
                     se = tri_winter_pred$se.fit) %>%
              mutate(rss = pred,
                     season = "Winter")) %>%
  mutate(
    rss.high = rss + 1.96*se,
    rss.low = rss - 1.96*se
  ) 

ggplot(data = tri_rss %>% filter(id == "newid"), 
       aes(x = mrdtm, y = rss, 
           # colour = "population-level estimate"
           )) +
  # geom_line(aes(group = id, colour = "individual-level estimates"),
  #           data = tri_rss %>% filter(id != "newid")) +
  geom_ribbon(aes(ymin = rss.low, ymax = rss.high), 
              alpha = 0.3, colour = NA) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~season, nrow = 2) +
  xlab("Terrain Ruggedness Index") + 
  ylab("Relative selection strength") +
  # scale_colour_manual(values = c("red", "black")) +
  labs(colour = "") +
  theme_proj

ggsave("output/figures/tri_rss.png", width = 3, height = 4)




### Temperature RSS plot ----

temper_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = modelr::seq_range(log_sl,30),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = mean(mrdtm),
                    water = 0,
                    avg_max_temp = c(0, 10, 20),
                    fire_10_low = 0,
                    fire_10_mod = 0,
                    fire_10_high = 0,
                    fire_20_low = 0,
                    fire_20_mod = 0,
                    fire_20_high = 0,
                    fire_30 = 0,
                    fire_40 = 0,
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) 


temper_summer_pred <- predict(m0_summer_glmmfit, 
                           newdata = temper_summer_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE,
                           se.fit = TRUE)

temper_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = modelr::seq_range(log_sl, 30),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = mean(mrdtm),
                    water = 0,
                    snow_depth = mean(snow_depth),
                    avg_min_temp = c(-40, -20, 0),
                    fire_10_20 = 0,
                    fire_30 = 0,
                    fire_40 = 0,                   
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) 

temper_winter_pred <- predict(m0_winter_glmmfit, 
                           newdata = temper_winter_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE,
                           se.fit = TRUE)

temper_rss <- temper_summer_data %>%
  mutate(pred = temper_summer_pred$fit,
         se = temper_summer_pred$se.fit) %>%
  mutate(rss = pred,
         season = "Summer") %>%
  bind_rows(temper_winter_data %>%
              mutate(pred = temper_winter_pred$fit,
                     se = temper_winter_pred$se.fit) %>%
              mutate(rss = pred,
                     season = "Winter")) %>%
  mutate(
    rss.high = rss + 1.96*se,
    rss.low = rss - 1.96*se,
    temp_cat = factor(ifelse(is.na(avg_max_temp),avg_min_temp, avg_max_temp))
  ) 

ggplot(data = temper_rss %>% filter(id == "newid"), 
       aes(x = exp(log_sl), y = rss, colour = "population-level estimate")) +
  geom_line(aes(group = id, colour = "individual-level estimates"),
            data = temper_rss %>% filter(id != "newid")) +
  geom_ribbon(aes(ymin = rss.low, ymax = rss.high), 
              alpha = 0.5, colour = NA) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~season + temp_cat, nrow = 2, scales = "free_x") +
  xlab("Step Length") + 
  ylab("Relative selection strength") +
  scale_colour_manual(values = c("red", "black")) +
  labs(colour = "") +
  theme_proj

ggsave("output/figures/temper_rss.png", width = 8, height = 6)




### Summer range selection ----
musk_sum_lc <- readRDS("output/musk_sum_lc.rds")

musk_sum_lc %>%
  mutate(group2 = ifelse(group == "true", 
                         "Occupied summer range",
                         "Available summer range")) %>%
  ggplot(aes(x = group2, y = for_per, fill = group2)) +
  geom_boxplot() +
  scale_fill_viridis_d(begin = 0.3) +
  xlab("") +
  ylab("Proportion grassland/shrubland") +
  guides(fill = FALSE) +
  theme_proj

ggsave("output/figures/summerrange_boxplot.png", width = 5, height = 3)

### Winter range selection ----
musk_wint_tpi <- readRDS("output/musk_wint_tpi.rds")

musk_wint_tpi %>%
  mutate(group2 = ifelse(group == "true", 
                         "Occupied winter range",
                         "Available winter range")) %>%
  ggplot(aes(x = group2, y = value, fill = group2)) +
  geom_boxplot() +
  scale_fill_viridis_d(begin = 0.3) +
  xlab("") +
  ylab("Topographic position index") +
  guides(fill = FALSE) +
  theme_proj

ggsave("output/figures/winterrange_boxplot.png", width = 5, height = 3)

