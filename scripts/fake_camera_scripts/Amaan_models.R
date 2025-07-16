library(glmmTMB)
library(tidyverse)
library(sf)
library(piecewiseSEM)
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
         hare_per_day = hare_sum/n_days_effort_sum,
         x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])

sampled_summarised_detections_spr <- sampled_summarised_detections %>%
  ungroup() %>%
  filter(season == "spring") 
sampled_summarised_detections_sum <- sampled_summarised_detections %>%
  ungroup() %>%
  filter(season == "summer")

mod_fox <- glmmTMB(fox_sum ~ scale(marten_per_day) +
                     scale(hare_per_day) +
                     scale(pr_mixed_deciduous) +
                     scale(pr_wetland) +
                     # scale(pr_disturbance) +
                     scale(closure_average) +
                     # scale(pr_water) +
                     offset(log(n_days_effort_sum)),
                   data = sampled_summarised_detections_spr,
                   family = nbinom2)
summary(mod_fox)
anova(mod_fox2, mod_fox)
### These are tests for overdispersion and zero-inflation (non-significance is 
### good for these tests)
sim_res <- DHARMa::simulateResiduals(mod_fox)
plot(sim_res)
DHARMa::testDispersion(sim_res)
DHARMa::testZeroInflation(sim_res)

mod_marten <- glmmTMB(marten_sum ~ scale(hare_per_day) +
                        scale(pr_mixed_deciduous) +
                        scale(pr_wetland) +
                        # scale(pr_disturbance) +
                        scale(closure_average) +
                        # scale(pr_water) +
                        offset(log(n_days_effort_sum)),
                      data = sampled_summarised_detections_spr,
                      # ziformula = ~1,
                      family = nbinom2)
summary(mod_marten)
sim_res <- DHARMa::simulateResiduals(mod_marten)
plot(sim_res)
DHARMa::testDispersion(sim_res)
DHARMa::testZeroInflation(sim_res)
DHARMa::testQuantiles(sim_res)

psem_mod <- psem(
  glmmTMB(marten_sum ~ hare_per_day +
            pr_mixed_deciduous +
            pr_wetland +
            closure_average +
            (1|plotID),
          data = sampled_summarised_detections_spr,
          # ziformula = ~1,
          family = poisson),
  glmmTMB(fox_sum ~ marten_sum +
            hare_per_day +
            pr_mixed_deciduous +
            pr_wetland +
            closure_average +
            (1|plotID),
          data = sampled_summarised_detections_spr,
          family = poisson),
  data = sampled_summarised_detections_spr
)
psem_mod
basisSet(psem_mod)
dSep(psem_mod, .progressBar = FALSE)
summary(psem_mod)

### example plot
new_data_hare <- camera_clusters2 %>%
  modelr::data_grid(fox_per_day = mean(fox_per_day),
                    marten_per_day = mean(marten_per_day),
                    hare_per_day = modelr::seq_range(hare_per_day, 20),
                    pr_mixed_deciduous = mean(pr_mixed_deciduous),
                    pr_coniferous = mean(pr_coniferous),
                    closure_average = mean(closure_average),
                    n_days_effort_sum = 30)

pred_hare <- new_data_hare %>%
  mutate(pred_fox = predict(mod_fox, new_data_hare, 
                            type = "link", se.fit = TRUE)$fit,
         pred_fox_sd = predict(mod_fox, new_data_hare, 
                               type = "link", se.fit = TRUE)$se.fit,
         pred_fox_mean = exp(pred_fox),
         pred_fox_low = exp(pred_fox-pred_fox_sd),
         pred_fox_high = exp(pred_fox+pred_fox_sd),
         pred_marten = predict(mod_marten, new_data_hare, 
                               type = "link", se.fit = TRUE)$fit,
         pred_marten_sd = predict(mod_marten, new_data_hare, 
                               type = "link", se.fit = TRUE)$se.fit,
         pred_marten_mean = exp(pred_marten),
         pred_marten_low = exp(pred_marten-pred_marten_sd),
         pred_marten_high = exp(pred_marten+pred_marten_sd))

ggplot(data = pred_hare, aes(x = hare_per_day)) +
  geom_line(aes(y = pred_fox_mean, colour = "fox")) + 
  geom_ribbon(aes(ymin = pred_fox_low,
                     ymax = pred_fox_high,
                     fill = "fox"),alpha = 0.3) +
  geom_line(aes(y = pred_marten_mean, colour = "marten"))+ 
  geom_ribbon(aes(ymin = pred_marten_low,
                     ymax = pred_marten_high,
                     fill = "marten"),alpha = 0.3) +
  guides(fill = "none") +
  xlab("Hare detections per day") +
  ylab("Detections per month") +
  labs(colour = "") +
  theme_bw()

sampled_summarised_detections %>%
  ungroup() %>%
  select(marten_per_day, fox_per_day, hare_per_day, pr_mixed_deciduous, pr_coniferous,
         pr_wetland, pr_water, closure_average, pr_disturbance) %>%
  cor()


