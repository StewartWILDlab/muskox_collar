library(tidyverse)
library(sf)
library(boral)

camera_clusters <- readRDS("D:/Downloads-HDD/camera_clusters.rds")
sampled_summarised_month_detections <- readRDS("D:/Downloads-HDD/sampled_summarized_month_detections.rds")
sampled_summarised_detections <- sampled_summarised_month_detections %>%
  mutate(season = ifelse(month %in% c("March", "April", "November", "October", "December", "January", "February"), "spring", "summer")) %>%
  group_by(plotID, season) %>%
  summarise(fox_sum = sum(`Red Fox`),
            marten_sum = sum(`American Marten`),
            hare_sum = sum(`Snowshoe Hare`),
            n_days_effort_sum = sum(n_days_effort),
            pr_coniferous = mean(percent_coniferous),
            pr_mixed_deciduous = mean(percent_deciduous+percent_mixed),
            pr_disturbance = mean(`Disturbance – Non and Sparse Woody`+`Disturbance – Treed and / or Shrub`),
            pr_wetland = mean(percent_wetland),
            pr_water = mean(percent_water),
            closure_average = mean(mean_closure),
            geometry = st_centroid(geometry.x[1])) %>%
  mutate(fox_per_day = fox_sum/n_days_effort_sum,
         marten_per_day = marten_sum/n_days_effort_sum,
         hare_per_day = hare_sum/n_days_effort_sum)

sampled_summarised_detections_spr <- sampled_summarised_detections %>%
  ungroup() %>%
  filter(season == "spring") 
sampled_summarised_detections_sum <- sampled_summarised_detections %>%
  ungroup() %>%
  filter(season == "summer")

data = sampled_summarised_detections_spr
### BORAL
bor_mod <- boral(y = as.matrix(data %>%
                                 select(fox_sum, marten_sum, hare_sum)),
                 X = scale(as.matrix(data %>%
                                       select(pr_mixed_deciduous,
                                              pr_wetland,
                                              closure_average))),
                 offset = matrix(rep(log(data$n_days_effort_sum),3), ncol = 3),
                 family = "negative.binomial",
                 save.model = TRUE,
                 lv.control = list(num.lv = 2),
                 mcmc.control = list(n.burnin = 20000, n.iteration = 80000))

summary(bor_mod)
get.residual.cor(bor_mod)
get.enviro.cor(bor_mod)
coefsplot("pr_mixed_deciduous",bor_mod)
coefsplot("pr_wetland",bor_mod)
coefsplot("closure_average",bor_mod)



### spOccupancy
# y = t(as.matrix(data %>%
#                 mutate(fox_occ = ifelse(fox_sum>0,1,0),
#                        mart_occ = ifelse(marten_sum>0,1,0),
#                        hare_occ = ifelse(hare_sum>0,1,0)) %>%
#                 select(fox_occ, mart_occ, hare_occ)))
# 
# sp_mod <- lfJSDM(formula = ~ scale(pr_mixed_deciduous) + scale(pr_wetland) + scale(closure_average),
#        data = list(y=y,
#                    covs = as.matrix(data %>%
#                                       select(pr_mixed_deciduous,
#                                              pr_wetland,
#                                              closure_average)),
#                    coords = as.matrix(data %>%
#                                       select(x,y))),
#        n.factors = 1,
#        n.samples = 10000)
# summary(sp_mod)
