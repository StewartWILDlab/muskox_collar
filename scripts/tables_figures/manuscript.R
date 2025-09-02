######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to produce tables and figures for manuscripts
library(tidyverse)
library(ctmm)
library(glmmTMB)

### set factor levels 
ids <- c("706", "708", "7010", "7011", "7012", "7013", "7080")
seas <- c("Summer", "Winter", "Calving")

### set theme
theme_proj <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### set colours

lc_atts <- readRDS("data/processed/lc_atts.rds") %>%
  mutate(Classification2 = str_replace_all(Classification, "-", " "))


### load files
issa_data_summer_comb <- readRDS("data/processed/issa_data_summer_comb.rds")
m0_summer_glmmfit <- readRDS("output/m0_summer_glmmfit.rds")
issa_data_winter_comb <- readRDS("data/processed/issa_data_winter_comb.rds")
m0_winter_glmmfit_lc <- readRDS("output/m0_winter_glmmfit_lc.rds")

### Home Range Size ----
akde <- readRDS("output/akde.rds") %>%
  map(function(x){as_tibble(summary(x)$CI)}) %>%
  list_rbind() %>%
  set_names(c("lower","mean","upper")) %>%
  mutate(id = as.character(names(readRDS("output/akde.rds"))),
         id = factor(id, levels = ids),
         area = round(mean),
         lower = round(lower),
         upper = round(upper),
         type = factor("Lifetime", levels = c("Lifetime", "Annual"))) 
mcp <- readRDS("output/musk_mcp.rds") %>%
  as_tibble() %>%
  mutate(year = str_split_i(id,"_",2),
         id = as.character(str_split_i(id,"_",1)),
         id = factor(id, levels = ids),
         type = factor("Annual", levels = c("Lifetime", "Annual")))
seas <- readRDS("output/musk_seasrange.rds") %>%
  mutate(id = as.character(Id_Number),
         year = as.character(year_min),
         type = "Seasonal",
         area = range_95, 
         group = "95% home range") %>%
  bind_rows(
    readRDS("output/musk_seasrange.rds") %>%
      mutate(id = as.character(Id_Number),
             year = as.character(year_min),
             type = "Seasonal",
             area = range_50, 
             group = "50% core range") 
  )

akde %>%
  ggplot(aes(x = id, y = area)) +
  geom_point(colour = "black", size = 3) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(fill = year), data = mcp, size = 3, shape = 21) +
  facet_wrap(~type) +
  scale_y_continuous(limits = c(0,NA)) +
  scale_fill_grey(start = 0, end = 1) +
  labs(fill = "Year") +
  xlab("Muskox ID") +
  ylab(bquote('Area'~(km^2))) +
  theme_proj +
  theme(legend.position = c(0.9,0.7)) 
ggsave("output/figures/homerange_size.png", width = 6, height = 5)

seas %>%
  mutate(group = factor(group, levels = c("95% home range", "50% core range"))) %>%
  filter(group == "95% home range") %>%
  ggplot(aes(x = season, y = area)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::label_comma()) +
  xlab("Season") +
  ylab(bquote('Area'~(km^2))) +
  theme_proj 
ggsave("output/figures/searange_size.png", width = 4, height = 4)

seas %>% group_by(season, group) %>%
  summarise(mean = mean(area))


### Range Overlap ----

musk_overlap <- readRDS("output/musk_overlap.rds")

p1 <- musk_overlap %>%
  filter(metric == "Percent Overlap",
         Id_Number == Id_Number_compare) %>%
  mutate(year_diff = year_min_compare - year_min) %>%
  ggplot(aes(x = factor(year_diff), y = value)) +
  geom_boxplot() +
  facet_grid(season~season_compare) +
  ylab("Percent overlap of home ranges") +
  xlab("Year lag") +
  theme_proj
p1
# p2 <- musk_overlap %>%
#   filter(metric == "Volume of UD",
#          Id_Number == Id_Number_compare) %>%
#   mutate(year_diff = year_min_compare - year_min) %>%
#   ggplot(aes(x = factor(year_diff), y = value)) +
#   geom_boxplot() +
#   facet_grid(season~season_compare) +
#   ylab("Volume index") +
#   xlab("Year lag") +
#   theme_proj
# 
# ggpubr::ggarrange(p1, p2, nrow = 2, labels = c("A", "B"))
ggsave("output/figures/searange_overlap.png", width = 6, height = 4)



### Home range selection Coefficient plots ----

### size models

# params <- c("Intercept", "Snow depth", "Season length",
#             "Muskox ID", "Residual")
# mod_coef <- readRDS("output/mod_coef.rds") %>%
#   mutate(Parameter = case_when(
#     term == "(Intercept)" ~ "Intercept",
#     term == "scale(snow_depth)" ~ "Snow depth",
#     term == "scale(seas_length)" ~ "Season length",
#     group == "Id_Number" ~ "Muskox ID",
#     group == "Residual" ~ "Residual"
#   ),
#   Parameter = factor(Parameter, levels = rev(params)),
#   Effect = ifelse(effect == "fixed", "Fixed", "Random"),
#   season = factor(season, levels = seas),
#   label = ifelse(season == "Winter"&Parameter == "Snow depth","*",""))
# mod_coef %>%
#   ggplot(aes(x = Parameter, y = estimate, 
#              colour = Effect, shape = contour)) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
#                   position = position_dodge(width = 0.5)) +
#   geom_text(aes(label = label), 
#             position = position_dodge(width = 0.5),
#             vjust = -0.2, colour = "black") +
#   facet_wrap(~season) +
#   coord_flip() +
#   ylab("Estimate") +
#   labs(shape = "Home range contour") +
#   scale_colour_grey(start = 0, end = 0.6) +
#   theme_proj
# ggsave("output/figures/hrsize_mod.png", width = 9, height = 5)
# 
# 
# ### buffer models - no longer using
# 
# params2 <- c("Intercept", "Home range buffer",
#             "Muskox ID", "Year", "Residual")
# mod_coef2 <- readRDS("output/mod_coef2.rds") %>%
#   mutate(Parameter = case_when(
#     term == "(Intercept)" ~ "Intercept",
#     term == "groupbuffer" ~ "Home range buffer",
#     group == "Id_Number" ~ "Muskox ID",
#     group == "year_min:Id_Number" ~ "Year",
#     group == "Residual" ~ "Residual"
#   ),
#   Parameter = factor(Parameter, levels = rev(params2)),
#   Effect = ifelse(effect == "fixed", "Fixed", "Random"),
#   season_group = ifelse(season == "Summer", 
#                         "Proportion \ngrassland/shrubland",
#                         "Snow depth"),
#   season = factor(season, levels = seas),
#   label = ifelse(Parameter == "Home range buffer","*","")) %>%
#   group_by(season) %>%
#   mutate(
#     buffer_cat = case_when(
#       contour == min(contour) ~ "small",
#       contour == max(contour) ~ "large",
#       TRUE ~ "medium"),
#     buffer_cat = factor(buffer_cat, levels = c("small", "medium", "large"))
#   )
# 
# mod_coef2 %>%
#   filter(season == "Summer") %>%
#   ggplot(aes(x = Parameter, y = estimate, 
#              colour = Effect, shape = buffer_cat)) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
#                   position = position_dodge(width = 0.5)) +
#   geom_text(aes(label = label), 
#             position = position_dodge(width = 0.5),
#             vjust = -0.2, colour = "black") +
#   coord_flip() +
#   ylab("Estimate") +
#   labs(shape = "Realtive buffer size") +
#   scale_colour_grey(start = 0, end = 0.6) +
#   theme_proj
# ggsave("output/figures/sumbuff_mod.png", width = 5, height = 5)
# 
# mod_coef2 %>%
#   filter(season != "Summer") %>%
#   ggplot(aes(x = Parameter, y = estimate, 
#              colour = Effect, shape = buffer_cat)) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
#                   position = position_dodge(width = 0.5)) +
#   geom_text(aes(label = label), 
#             position = position_dodge(width = 0.5),
#             vjust = -0.2, colour = "black") +
#   coord_flip() +
#   ylab("Estimate") +
#   facet_wrap(~season, scales = "free_x") +
#   labs(shape = "Realtive buffer size") +
#   scale_colour_grey(start = 0, end = 0.6) +
#   theme_proj
# ggsave("output/figures/elevbuff_mod.png", width = 8, height = 6.5)
# 


### HR selection Coefficient estimates ----
params3 <- c("Intercept", "Proportion \ngrassland/shrubland", 
             "TPI","sd Muskox ID")
readRDS("output/select_mod_coefs.rds") %>%
  mutate(Parameter = factor(Parameter, levels = rev(params3)),
         Season = factor(Season, level = seas)) %>%
  ggplot(aes(x = Parameter, y = `50%`, 
             colour = Effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  coord_flip() +
  ylab("Estimate") +
  facet_wrap(~Season) +
  scale_colour_grey(start = 0, end = 0.6) +
  theme_proj
ggsave("output/figures/select_mod.png", width = 8, height = 4)

### ISSA coefficient plot ----
par_lut <- tibble(
  mod_par = c('scale(log_sl)', 'scale(cos_ta)', 'scale(mrdtm)', 
              'scale(log_fire_dist)', 'scale(tpi)', 'water',
              'Wetland', 'fire_10_low', 'fire_10_mod', 'fire_10_high', 
              'fire_20_low', 'fire_20_mod', 'fire_20_high', 'fire_30', 
              'fire_40', 'scale(log_sl):scale(cos_ta)', 
              'scale(log_fire_dist):fire_10', 'scale(log_fire_dist):fire_20',
              'scale(log_fire_dist):fire_30', 'scale(log_fire_dist):fire_40',
              'scale(log_sl):scale(avg_max_temp)',
              'scale(avg_max_temp):scale(log_sl)',
              'Sub_polar_taiga_needleleaf_forest', 
              'Temperate_or_sub_polar_shrubland', 
              'Temperate_or_sub_polar_grassland', 'Barren_Lands',
              'scale(log_sl):scale(avg_min_temp)',
              'scale(log_sl):scale(snow_depth)',
              'scale(snow_depth):scale(log_sl)',
              'scale(avg_min_temp):scale(log_sl)',
              'scale(log_water_dist)',
              'Sub_polar_taiga_needleleaf_forest:calving', 
              'Temperate_or_sub_polar_shrubland:calving', 
              'Temperate_or_sub_polar_grassland:calving', 'Barren_Lands:calving',
              'scale(mrdtm):calving', 
              'scale(log_fire_dist):calving', 'scale(tpi):calving', 'water:calving', 
              'Wetland:calving', 'scale(log_sl):calving', 'calving:scale(log_sl)',
              'scale(log_water_dist):calving'),
  name = c("Step length", "Cosine turning angle",
           "TRI", "Distance to fire edge", "TPI", "Waterbody",
           "Wetland", "1-10 year old fire - low severity", 
           "1-10 year old fire - moderate severity", 
           "1-10 year old fire - high severity",
           "11-20 year old fire - low severity", 
           "11-20 year old fire - moderate severity", 
           "11-20 year old fire - high severity", "21-30 year old fire",
           "31-40 year old fire", "Step length:Cosine turning angle",
           "Distance to fire edge:1-10 year old fire",
           "Distance to fire edge:11-20 year old fire",
           "Distance to fire edge:21-30 year old fire",
           "Distance to fire edge:31-40 year old fire",
           "Step length:Max daily temperature",
           "Step length:Max daily temperature",
           "Sub-polar taiga needleleaf forest", 
           "Temperate or sub-polar shrubland", 
           "Temperate or sub-polar grassland", "Barren Lands",
           "Step length:Min daily temperature",
           "Step length:Snow depth","Step length:Snow depth",
           "Step length:Min daily temperature",
           "Distance to waterbody",
           "Sub-polar taiga needleleaf forest:Calving season", 
           "Temperate or sub-polar shrubland:Calving season", 
           "Temperate or sub-polar grassland:Calving season", 
           "Barren Lands:Calving season","TRI:Calving season", 
           "Distance to fire edge:Calving season","TPI:Calving season",
           "Waterbody:Calving season", "Wetland:Calving season",
           "Step length:Calving season", "Step length:Calving season",
           "Distance to waterbody:Calving season")
)

par_order <- c("TRI", "TPI", "Waterbody", "Distance to waterbody", 
                "1-10 year old fire - low severity", 
               "1-10 year old fire - moderate severity", 
               "1-10 year old fire - high severity",
               "11-20 year old fire - low severity", 
               "11-20 year old fire - moderate severity", 
               "11-20 year old fire - high severity", "21-30 year old fire",
               "31-40 year old fire", "Distance to fire edge",
               "Distance to fire edge:1-10 year old fire",
               "Distance to fire edge:11-20 year old fire",
               "Distance to fire edge:21-30 year old fire",
               "Distance to fire edge:31-40 year old fire",
               "Sub-polar taiga needleleaf forest", 
               "Temperate or sub-polar shrubland", 
               "Temperate or sub-polar grassland", "Barren Lands", "Wetland",
               "TRI:Calving season", "TPI:Calving season", 
               "Waterbody:Calving season", "Distance to waterbody:Calving season",
               "Sub-polar taiga needleleaf forest:Calving season", 
               "Temperate or sub-polar shrubland:Calving season", 
               "Temperate or sub-polar grassland:Calving season",
               "Barren Lands:Calving season", "Wetland:Calving season",
               "Step length", "Step length:Calving season", "Cosine turning angle",
               "Step length:Cosine turning angle",
               "Step length:Max daily temperature",
               "Step length:Min daily temperature",
               "Step length:Snow depth")


summ_pars <- broom.mixed::tidy(m0_summer_glmmfit, conf.int = TRUE, conf.level = 0.95) %>%
  mutate(mod_par = str_remove(term, "sd__")) %>%
  left_join(par_lut) %>%
  mutate(season = "Summer")
wint_pars <- broom.mixed::tidy(m0_winter_glmmfit_lc, conf.int = TRUE, conf.level = 0.95) %>%
  mutate(mod_par = str_remove(term, "sd__")) %>%
  left_join(par_lut) %>%
  mutate(season = "Winter")

summ_pars %>%
  bind_rows(wint_pars) %>%
  drop_na(name) %>%
  mutate(name = factor(name, levels = rev(par_order)),
         effect = ifelse(effect == "fixed", "Fixed effect", "Between-individual variation"),
         effect = factor(effect, levels = c("Fixed effect", "Between-individual variation"))) %>%
  ggplot(aes(x = name, y = estimate, colour = season)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
  coord_flip() +
  ylab("Estimate") +
  xlab("") +
  facet_wrap(~effect, scales = "free_x") +
  scale_colour_manual(values = c("#4daf4a", "#377eb8")) +
  theme_proj
ggsave("output/figures/issa_summer_mod.png", width = 8, height = 5)

wint_pars %>%
  drop_na(name) %>%
  mutate(name = factor(name, levels = rev(par_order)),
         effect = ifelse(effect == "fixed", "Fixed effect", "Between-individual variation"),
         effect = factor(effect, levels = c("Fixed effect", "Between-individual variation"))) %>%
  ggplot(aes(x = name, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  ylab("Estimate") +
  xlab("") +
  facet_wrap(~effect, scales = "free_x") +
  scale_colour_grey(start = 0, end = 0.6) +
  theme_proj
ggsave("output/figures/issa_winter_mod.png", width = 8, height = 4)

summ_pars %>%
  bind_rows(wint_pars) %>%
  drop_na(name) %>%
  filter(name != "Wetland") %>%
  mutate(name = factor(name, levels = par_order),
         estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         conf.low = round(conf.low, 2),
         conf.high = round(conf.high, 2)) %>%
  select(season, name, effect, estimate, std.error, conf.low, conf.high) %>%
  arrange(name) %>%
  pivot_wider(names_from = effect, 
              values_from = c(estimate, std.error, conf.low, conf.high),
              names_vary = "slowest") %>%
  select(-std.error_ran_pars, -conf.low_ran_pars, -conf.high_ran_pars) %>%
  arrange(season, name) %>%
  write.csv("output/issa_mod_output.csv")

### Fire RSS plot ----

fireyear_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    log_fire_dist = mean(log_fire_dist),
                    tpi = mean(tpi),
                    mrdtm = mean(mrdtm),
                    water = 0,
                    Wetland = 0,
                    avg_max_temp = mean(avg_max_temp),
                    fire_cat = c("fire_10_low", "fire_10_mod", "fire_10_high",
                                 "fire_20_low", "fire_20_mod", "fire_20_high",
                                 "fire_30", "fire_40"),
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) %>%
  mutate(fire_cat2 = fire_cat) %>%
  pivot_wider(names_from = fire_cat, values_from = value, values_fill = 0) %>%
  mutate(fire_10_dup = fire_10_low + fire_10_mod + fire_10_high,
         fire_20_dup = fire_20_low + fire_20_mod + fire_20_high,
         fire_30_dup = fire_30,
         fire_40_dup = fire_40) %>%
  arrange(id)


fireyear_summer_pred_id <- predict(m0_summer_glmmfit, 
                                newdata = fireyear_summer_data %>%
                                  filter(id != "newid"),
                                type = "link",
                                re.form = NULL,
                                allow.new.levels=TRUE,
                                se.fit = TRUE)
fireyear_summer_pred_newid <- predict(m0_summer_glmmfit, 
                                   newdata = fireyear_summer_data %>%
                                     filter(id == "newid"),
                                   type = "link",
                                   re.form = NA,
                                   allow.new.levels=TRUE,
                                   se.fit = TRUE)

fireyear_rss <- fireyear_summer_data %>%
  mutate(pred = c(fireyear_summer_pred_id$fit,
                  fireyear_summer_pred_newid$fit),
         se = c(fireyear_summer_pred_id$se.fit,
                fireyear_summer_pred_newid$se.fit)) %>%
  mutate(rss = pred) %>%
  mutate(
    fire_year = case_when(
      fire_cat2 %in% c("fire_10_low", "fire_10_mod", "fire_10_high") ~ "1 - 10",
      fire_cat2 %in% c("fire_20_low", "fire_20_mod","fire_20_high") ~ "11 - 20",
      fire_cat2 %in% c("fire_30") ~ "21 - 30",
      fire_cat2 %in% c("fire_40") ~ "31 - 40"),
    fire_severity = case_when(
      fire_cat2 %in% c("fire_10_low", "fire_20_low") ~ "Low",
      fire_cat2 %in% c("fire_10_mod", "fire_20_mod") ~ "Mod",
      fire_cat2 %in% c("fire_10_high", "fire_20_high") ~ "High",
      TRUE ~ "NA"
    ),
    fire_severity = factor(fire_severity, levels = c("Low", "Mod", "High", "NA")),
    rss.high = exp(rss + 1.96*se),
    rss.low = exp(rss - 1.96*se),
    rss.exp = exp(rss)
  ) %>%
  group_by(fire_year) %>%
  mutate(width = 0.1*length(unique(fire_severity))) %>%
  ungroup()

ggplot(data = fireyear_rss %>% filter(id == "newid"), 
       aes(x = fire_year, y = rss.exp,
           colour = fire_severity)) +
  geom_errorbar(aes(ymin = rss.low, ymax = rss.high, width = width),
                position = position_dodge(width = 0.7)) +
  geom_point(size = 3,position = position_dodge(width = 0.7)) +
  geom_point(aes(group = fire_severity),
              size = 2, alpha = 0.5,
             position = position_dodge(width = 0.7),
             data = fireyear_rss %>% filter(id != "newid")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Years since fire") + 
  ylab("Relative selection strength") +
  labs(colour = "Fire severity") +
  guides(colour = guide_legend(position = "inside")) +
  scale_y_log10() +
  scale_colour_manual(values = c("#fcae91", "#fb6a4a","#cb181d", "black", "darkgrey"))+
  theme_proj +
  theme(legend.position.inside = c(0.75,0.75))

ggsave("output/figures/fireyear_rss.png", width = 6, height = 4)




### Fire distance RSS plot ----

firedist_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    log_fire_dist = seq(0,4, length.out = 30),
                    tpi = mean(tpi),
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
                    fire_cat = c("fire_10_dup","fire_20_dup",
                                 "fire_30_dup", "fire_40_dup", "unburned"),
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) %>%
  mutate(fire_cat2 = fire_cat) %>%
  pivot_wider(names_from = fire_cat, values_from = value, values_fill = 0) %>%
  arrange(id)


firedist_summer_pred_id <- predict(m0_summer_glmmfit, 
                                      newdata = firedist_summer_data %>% 
                                        filter(id != "newid"),
                                      type = "link",
                                      re.form = NULL,
                                      allow.new.levels=TRUE,
                                      se.fit = TRUE)
firedist_summer_pred_newid <- predict(m0_summer_glmmfit, 
                                      newdata = firedist_summer_data %>% 
                                        filter(id == "newid"),
                                      type = "link",
                                      re.form = NA,
                                      allow.new.levels=TRUE,
                                      se.fit = TRUE)

firedist_rss <- firedist_summer_data %>%
  mutate(pred = c(firedist_summer_pred_id$fit,
                  firedist_summer_pred_newid$fit),
         se = c(firedist_summer_pred_id$se.fit,
                firedist_summer_pred_newid$se.fit)) %>%
  mutate(rss = pred) %>%
  mutate(
    fire_year = case_when(
      fire_cat2 %in% c("fire_10_dup", "fire_10_mod", "fire_10_high") ~ "1 - 10 years",
      fire_cat2 %in% c("fire_20_dup", "fire_20_mod","fire_20_high") ~ "11 - 20 years",
      fire_cat2 %in% c("fire_30_dup") ~ "21 - 30 years",
      fire_cat2 %in% c("fire_40_dup") ~ "31 - 40 years",
      fire_cat2 %in% c("unburned") ~ ">40 years/unburned"),
    fire_year = factor(fire_year, levels = c("1 - 10 years", "11 - 20 years",
                                             "21 - 30 years", "31 - 40 years",
                                             ">40 years/unburned")),
    rss.high = exp(rss + 1.96*se),
    rss.low = exp(rss - 1.96*se),
    rss.exp = exp(rss),
    fire_dist = 30*(exp(log_fire_dist)-1)
  ) 

ggplot(data = firedist_rss %>% filter(id == "newid"), 
       aes(x = fire_dist, y = rss.exp)) +
  geom_ribbon(aes(ymin = rss.low, ymax = rss.high), alpha = 0.5) +
  geom_line(size = 1) +
  geom_line(data = firedist_rss %>% filter(id != "newid"),
            aes(group = id), linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Distance to nearest disturbance boundary (m)") + 
  ylab("Relative selection strength") +
  facet_wrap(~fire_year) +
  scale_y_log10() +
  coord_cartesian(ylim = c(0.5,3)) +
  theme_proj +
  theme(legend.position.inside = c(0.75,0.75))

ggsave("output/figures/firedist_rss.png", width = 6, height = 4)




### Landcover RSS plot ----

lc_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = mean(mrdtm),
                    snow_depth = mean(snow_depth),
                    avg_min_temp = mean(avg_max_temp),
                    lc_cat = c('Sub_polar_taiga_needleleaf_forest', 
                                 'Temperate_or_sub_polar_shrubland', 
                                 'Temperate_or_sub_polar_grassland', 
                                 'Wetland', 'Barren_Lands'),
                    fire_10_20_30 = 0,
                    fire_40 = 0,
                    log_fire_dist = mean(log_fire_dist),
                    calving = 0,
                    value = 1,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) %>%
  mutate(lc_cat2 = lc_cat) %>%
  pivot_wider(names_from = lc_cat, values_from = value, values_fill = 0) %>%
  arrange(id)


lc_winter_pred_id <- predict(m0_winter_glmmfit_lc, 
                          newdata = lc_winter_data %>%
                            filter(id != "newid"),
                          type = "link",
                          re.form = NULL,
                          allow.new.levels=TRUE,
                          se.fit = TRUE)
lc_winter_pred_newid <- predict(m0_winter_glmmfit_lc, 
                             newdata = lc_winter_data %>%
                               filter(id == "newid"),
                             type = "link",
                             re.form = NA,
                             allow.new.levels=TRUE,
                             se.fit = TRUE)

lc_rss <- lc_winter_data %>%
  mutate(pred = c(lc_winter_pred_id$fit,
                  lc_winter_pred_newid$fit),
         se = c(lc_winter_pred_id$se.fit,
                lc_winter_pred_newid$se.fit)) %>%
  mutate(rss = pred) %>%
  mutate(
    lc_class = str_replace_all(lc_cat2,"_", " "),
    lc_class = ifelse(lc_class == "water", "Water", lc_class),
    lc_class = factor(lc_class, levels = lc_atts$Classification2),
    rss.high = exp(rss + 1.96*se),
    rss.low = exp(rss - 1.96*se),
    rss.exp = exp(rss)
  )

ggplot(data = lc_rss %>% filter(id == "newid"), 
       aes(x = lc_class, y = rss.exp, colour = lc_class)) +
  geom_errorbar(aes(ymin = rss.low, ymax = rss.high), width = 0.25) +
  geom_point(size = 3) +
  geom_point(aes(group = id),
              size = 2, alpha = 0.5,
             data = lc_rss %>% filter(id != "newid")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Land cover") + 
  ylab("Relative selection strength") +
  scale_y_log10() +
  scale_colour_manual(values = lc_atts %>%
                        filter(Classification2 %in% lc_rss$lc_class) %>%
                        pull(hex)) +
  scale_x_discrete(labels = str_wrap(c("Sparse needleleaf forest",
                                       "Shrubland", "Grassland",
                                       "Wetland", "Barren Lands"), width = 15)) +
  guides(colour = "none") +
  theme_proj +
  theme(legend.position.inside = c(0.75,0.8),
        # axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank()
        )

ggsave("output/figures/lc_rss.png", width = 6, height = 4)





### Step length RSS plot ----

summer_base_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    log_fire_dist = mean(log_fire_dist),
                    tpi = mean(tpi),
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
                    fire_10_dup = 0,
                    fire_20_dup = 0,
                    fire_30_dup = 0,
                    fire_40_dup = 0,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  ) 

steplen_summer_data <- summer_base_data %>%
  select(-avg_max_temp, -log_sl) %>%
  expand_grid(
    log_sl = modelr::seq_range(issa_data_summer_comb$log_sl,30),
    avg_max_temp = c(0, 10, 20)) %>%
  mutate(condition = "Daily temperature") %>%
  arrange(id)

steplen_summer_pred_id <- predict(m0_summer_glmmfit, 
                                  newdata = steplen_summer_data %>%
                                    filter(id != "newid"),
                                  type = "link",
                                  re.form = NULL,
                                  allow.new.levels=TRUE,
                                  se.fit = TRUE)
steplen_summer_pred_newid <- predict(m0_summer_glmmfit, 
                                newdata = steplen_summer_data %>%
                                  filter(id == "newid"),
                                type = "link",
                                re.form = NA,
                                allow.new.levels=TRUE,
                                se.fit = TRUE)

winter_base_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    log_water_dist = mean(log_water_dist),
                    tpi = mean(tpi),
                    mrdtm = mean(mrdtm),
                    snow_depth = mean(snow_depth),
                    avg_min_temp = mean(avg_min_temp),
                    Sub_polar_taiga_needleleaf_forest = 0,
                    Temperate_or_sub_polar_shrubland = 0,
                    Temperate_or_sub_polar_grassland = 0,
                    Barren_Lands = 0,
                    Mixed_forest = 0,
                    Wetland = 0,
                    water = 0,
                    calving = 0,
                    strat_id = NA,
                    id = c(as.character(id),"newid")
  )

steplen_winter_data <- winter_base_data %>%
  select(-avg_min_temp, -log_sl) %>%
  expand_grid(
    log_sl = modelr::seq_range(issa_data_winter_comb$log_sl,30),
    avg_min_temp = c(-40, -20, 0)) %>%
  mutate(condition = "Daily temperature") %>%
  bind_rows(
    winter_base_data %>%
      select(-snow_depth, -log_sl) %>%
      expand_grid(
        log_sl = modelr::seq_range(issa_data_winter_comb$log_sl,30),
        snow_depth = c(0, 0.2, 0.4)) %>%
      mutate(condition = "Snow depth")
  ) %>%
  arrange(id)


steplen_winter_pred_id <- predict(m0_winter_glmmfit_lc, 
                             newdata = steplen_winter_data %>%
                               filter(id != "newid"),
                             type = "link",
                             re.form = NULL,
                             allow.new.levels=TRUE,
                             se.fit = TRUE)
steplen_winter_pred_newid <- predict(m0_winter_glmmfit_lc, 
                                newdata = steplen_winter_data %>%
                                  filter(id == "newid"),
                                type = "link",
                                re.form = NA,
                                allow.new.levels=TRUE,
                                se.fit = TRUE)

steplen_rss <- steplen_winter_data %>%
  mutate(pred = c(steplen_winter_pred_id$fit,
                  steplen_winter_pred_newid$fit),
         se = c(steplen_winter_pred_id$se.fit,
                steplen_winter_pred_newid$se.fit),
         season = "Winter") %>%
  bind_rows(
    steplen_summer_data %>%
      mutate(pred = c(steplen_summer_pred_id$fit,
                      steplen_summer_pred_newid$fit),
             se = c(steplen_summer_pred_id$se.fit,
                    steplen_summer_pred_newid$se.fit),
             season = "Summer")
  ) %>%
  mutate(rss = pred) %>%
  mutate(
    rss.high = exp(rss + 1.96*se),
    rss.low = exp(rss - 1.96*se),
    rss.exp = exp(rss),
    step_length = exp(log_sl),
    snow_depth = str_c(snow_depth*100, " cm"),
    avg_min_temp = factor(str_c(avg_min_temp, "°C"),
                          levels = c("-40°C", "-20°C", "0°C")),
    avg_max_temp = str_c(avg_max_temp, "°C")
                          
  )

wint_snow <- ggplot(data = steplen_rss %>% filter(id == "newid",
                                     season == "Winter",
                                     condition == "Snow depth"), 
       aes(x = step_length, y = rss.exp)) +
  geom_ribbon(aes(ymin = rss.low, ymax = rss.high), alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_line(aes(group = id),
            linewidth = 1, alpha = 0.5, linetype = "dotted",
              data = steplen_rss %>% filter(id != "newid",
                                       season == "Winter",
                                       condition == "Snow depth")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~snow_depth) +
  xlab("Step length") + 
  ylab("Relative selection strength") +
  ggtitle("Winter - Snow depth") +
  scale_y_log10(labels = scales::comma,
                limits = c(0.01,1000)) +
  theme_proj 

wint_temp <- ggplot(data = steplen_rss %>% filter(id == "newid",
                                     season == "Winter",
                                     condition == "Daily temperature"), 
       aes(x = step_length, y = rss.exp)) +
  geom_ribbon(aes(ymin = rss.low, ymax = rss.high), alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_line(aes(group = id),
            linewidth = 1, alpha = 0.5, linetype = "dotted",
            data = steplen_rss %>% filter(id != "newid",
                                          season == "Winter",
                                          condition == "Daily temperature")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~avg_min_temp) +
  xlab("Step length") + 
  ylab("Relative selection strength") +
  ggtitle("Winter - Daily temperature") +
  scale_y_log10(labels = scales::comma,
                limits = c(0.01,500)) +
  theme_proj 

summ_temp <- ggplot(data = steplen_rss %>% filter(id == "newid",
                                     season == "Summer",
                                     condition == "Daily temperature"), 
       aes(x = step_length, y = rss.exp)) +
  geom_ribbon(aes(ymin = rss.low, ymax = rss.high), alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_line(aes(group = id),
            linewidth = 1, alpha = 0.5, linetype = "dotted",
            data = steplen_rss %>% filter(id != "newid",
                                          season == "Summer",
                                          condition == "Daily temperature")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~avg_max_temp) +
  xlab("Step length") + 
  ylab("Relative selection strength") +
  ggtitle("Summer - Daily temperature") +
  scale_y_log10() +
  theme_proj 

ggpubr::ggarrange(summ_temp, wint_temp, wint_snow, nrow = 3,
                  labels = c("A", "B", "C"))

ggsave("output/figures/step_rss.png", width = 6, height = 8)




