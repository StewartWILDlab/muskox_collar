######################################################
############ Muskox Collar Project ############ 
######################################################
### Using muskox collar data from the Sahtu region, NWT to investigate
### muskox habitat, home range, and movement behaviour below treeline

######################################################
### Script to produce tables and figures for manuscripts
library(tidyverse)

### set factor levels 
ids <- c("706", "708", "7010", "7011", "7012", "7013", "7080")
seas <- c("Summer", "Winter", "Calving")

### set theme
theme_proj <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### Home Range Size
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
  ggplot(aes(x = season, y = area)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::label_comma()) +
  facet_wrap(~group) +
  xlab("Season") +
  ylab(bquote('Area'~(km^2))) +
  theme_proj 
ggsave("output/figures/searange_size.png", width = 6, height = 4)

seas %>% group_by(season, group) %>%
  summarise(mean = mean(area))


### Range Overlap

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

p2 <- musk_overlap %>%
  filter(metric == "Volume of UD",
         Id_Number == Id_Number_compare) %>%
  mutate(year_diff = year_min_compare - year_min) %>%
  ggplot(aes(x = factor(year_diff), y = value)) +
  geom_boxplot() +
  facet_grid(season~season_compare) +
  ylab("Volume index") +
  xlab("Year lag") +
  theme_proj

ggpubr::ggarrange(p1, p2, nrow = 2, labels = c("A", "B"))
ggsave("output/figures/searange_overlap.png", width = 6, height = 8)



### Coefficient plots

### size models

params <- c("Intercept", "Snow depth", "Season length",
            "Muskox ID", "Residual")
mod_coef <- readRDS("output/mod_coef.rds") %>%
  mutate(Parameter = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "scale(snow_depth)" ~ "Snow depth",
    term == "scale(seas_length)" ~ "Season length",
    group == "Id_Number" ~ "Muskox ID",
    group == "Residual" ~ "Residual"
  ),
  Parameter = factor(Parameter, levels = rev(params)),
  Effect = ifelse(effect == "fixed", "Fixed", "Random"),
  season = factor(season, levels = seas),
  label = ifelse(season == "Winter"&Parameter == "Snow depth","*",""))
mod_coef %>%
  ggplot(aes(x = Parameter, y = estimate, 
             colour = Effect, shape = contour)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.5),
            vjust = -0.2, colour = "black") +
  facet_wrap(~season) +
  coord_flip() +
  ylab("Estimate") +
  labs(shape = "Home range contour") +
  scale_colour_grey(start = 0, end = 0.6) +
  theme_proj
ggsave("output/figures/hrsize_mod.png", width = 9, height = 5)


### buffer models - no longer using

params2 <- c("Intercept", "Home range buffer",
            "Muskox ID", "Year", "Residual")
mod_coef2 <- readRDS("output/mod_coef2.rds") %>%
  mutate(Parameter = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "groupbuffer" ~ "Home range buffer",
    group == "Id_Number" ~ "Muskox ID",
    group == "year_min:Id_Number" ~ "Year",
    group == "Residual" ~ "Residual"
  ),
  Parameter = factor(Parameter, levels = rev(params2)),
  Effect = ifelse(effect == "fixed", "Fixed", "Random"),
  season_group = ifelse(season == "Summer", 
                        "Proportion \ngrassland/shrubland",
                        "Snow depth"),
  season = factor(season, levels = seas),
  label = ifelse(Parameter == "Home range buffer","*","")) %>%
  group_by(season) %>%
  mutate(
    buffer_cat = case_when(
      contour == min(contour) ~ "small",
      contour == max(contour) ~ "large",
      TRUE ~ "medium"),
    buffer_cat = factor(buffer_cat, levels = c("small", "medium", "large"))
  )

mod_coef2 %>%
  filter(season == "Summer") %>%
  ggplot(aes(x = Parameter, y = estimate, 
             colour = Effect, shape = buffer_cat)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.5),
            vjust = -0.2, colour = "black") +
  coord_flip() +
  ylab("Estimate") +
  labs(shape = "Realtive buffer size") +
  scale_colour_grey(start = 0, end = 0.6) +
  theme_proj
ggsave("output/figures/sumbuff_mod.png", width = 5, height = 5)

mod_coef2 %>%
  filter(season != "Summer") %>%
  ggplot(aes(x = Parameter, y = estimate, 
             colour = Effect, shape = buffer_cat)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5)) +
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.5),
            vjust = -0.2, colour = "black") +
  coord_flip() +
  ylab("Estimate") +
  facet_wrap(~season, scales = "free_x") +
  labs(shape = "Realtive buffer size") +
  scale_colour_grey(start = 0, end = 0.6) +
  theme_proj
ggsave("output/figures/elevbuff_mod.png", width = 8, height = 6.5)


### home range selection models

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



