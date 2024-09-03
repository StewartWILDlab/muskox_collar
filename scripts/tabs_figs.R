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
