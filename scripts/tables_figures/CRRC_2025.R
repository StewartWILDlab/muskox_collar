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
water_data_crop <- readRDS("data/processed/water_data_crop.rds")
fire_data_subset <- readRDS(here("data/processed/fire_data_subset.rds"))
mrdtm_proj <- terra::rast(here("data/processed/mrdtm_proj.tif"))
musk_season_mcp <- readRDS("output/musk_season_mcp.rds") %>%
  sf::st_as_sf() %>%
  mutate(year = str_split_i(id,"_",2),
         season = str_split_i(id,"_",3),
         id = as.character(str_split_i(id,"_",1)),
         id = factor(id, levels = ids))
place_names <- sf::read_sf("data/raw/geography/cgn_nt_shp_eng.shp") %>%
  filter(CATEGORY == "Populated Place",
         GEONAME != "Canol") %>%
  st_transform(crs(mrdtm_proj))

### Seasonal home range polygons using MCP ----
bbox = sf::st_bbox(musk_season_mcp)

ggplot() +
  geom_sf(data = water_data_crop, fill = "lightblue") +
  geom_sf(data = musk_season_mcp %>%
            filter(season != "Calving"), 
          aes(fill = id, alpha = season)) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax),
           ylim = c(bbox$ymin, bbox$ymax),
           expand = FALSE) +
  id_colscale +
    labs(fill = "Muskox ID")+ 
  facet_wrap(~season) +
  scale_alpha_manual(values = c(0.5,1), guide = FALSE) +
  theme(plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.93,0.6)) + 

  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    text_cex = 1, text_col = "black"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(2, "cm"), width = unit(2, "cm"),
    pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_col = "grey20",
      text_col = "black",
      text_size = 14
    )
  )

ggsave("output/figures/mcps.png", width = 8, height = 6)

  

### Land cover and fires map ----
r_ext <- ext(lc_2010_proj_simp)
aspect_ratio = (r_ext$ymax - r_ext$ymin)/(r_ext$xmax - r_ext$xmin)

(lc_map <- ggplot() +
  tidyterra::geom_spatraster(data = lc_2010_proj_simp) +
  labs(fill = "Land cover 2010") +
  coord_sf(xlim = c(r_ext$xmin, r_ext$xmax),
           ylim = c(r_ext$ymin, r_ext$ymax),
           expand = FALSE) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.2),
        legend.background = element_rect(fill="white"),
        legend.margin = margin(c(5,5,5,5))) + 
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    text_cex = 1, text_col = "white"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(3, "cm"), width = unit(3, "cm"),
    pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_col = "grey20",
      text_col = "white",
      text_size = 14
    )
  )
)
ggsave("output/figures/fire_lc1.png", width = 7, height = 7*aspect_ratio)

lc_map +
  geom_sf(data = fire_data_subset %>%
            st_crop(lc_2010_proj) %>%
            filter(fireyear<=2010,
                   fireyear>=1985) %>%
            nngeo::st_remove_holes(),
          colour = "pink", fill = NA, linewidth = 1) +
  coord_sf(xlim = c(r_ext$xmin, r_ext$xmax),
           ylim = c(r_ext$ymin, r_ext$ymax),
           expand = FALSE)

ggsave("output/figures/fire_lc2.png", width = 7, height = 7*aspect_ratio)


### Study region elevation ----
r_ext <- ext(mrdtm_proj)
aspect_ratio = (r_ext$ymax - r_ext$ymin)/(r_ext$xmax - r_ext$xmin)

ggplot() +
  tidyterra::geom_spatraster(data = mrdtm_proj) +
  geom_sf(data = place_names, colour = "white") +
  geom_sf_text(data = place_names, aes(label = GEONAME),
                colour = "white", 
               nudge_x = -12000, nudge_y = 4000) +
  labs(fill = "Elevation (m)") + 
  coord_sf(xlim = c(r_ext$xmin, r_ext$xmax),
           ylim = c(r_ext$ymin, r_ext$ymax),
           expand = FALSE) +
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.2),
        legend.background = element_rect(fill="white"),
        legend.margin = margin(c(5,5,5,5))) + 
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    text_cex = 1, text_col = "white"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(3, "cm"), width = unit(3, "cm"),
    pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_col = "grey20",
      text_col = "white",
      text_size = 14
    )
  )
  
ggsave("output/figures/elevation.png", width = 7, height = 7*aspect_ratio)
  

### Summer RSS predictions ----
issa_data_summer_comb <- readRDS(here("data/processed/issa_data_summer_comb.rds"))
m0_summer_glmmfit <- readRDS(here("output/m0_summer_glmmfit.rds"))

ref_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = 0,
                    log_fire_year = log(25),
                    strat_id = NA,
                    id = id
  )

fireyear_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = 0,
                    log_fire_year = log(2:45),
                    strat_id = NA,
                    id = id
  )

tri_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = modelr::seq_range(mrdtm,20),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = 0,
                    log_fire_year = log(25),
                    strat_id = NA,
                    id = id
  )

openlc_summer_data <- issa_data_summer_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = c(0,1),
                    log_fire_year = log(25),
                    strat_id = NA,
                    id = id
  )

ref_summer_pred <- predict(m0_summer_glmmfit,
                    newdata = ref_summer_data,
                    type = "link",
                    re.form = NULL,
                    allow.new.levels=TRUE)

fireyear_summer_pred <- predict(m0_summer_glmmfit, 
                         newdata = fireyear_summer_data,
                         type = "link",
                         re.form = NULL,
                         allow.new.levels=TRUE)

tri_summer_pred <- predict(m0_summer_glmmfit, 
                         newdata = tri_summer_data,
                         type = "link",
                         re.form = NULL,
                         allow.new.levels=TRUE)

openlc_summer_pred <- predict(m0_summer_glmmfit, 
                           newdata = openlc_summer_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE)

fireyear_summer_rss <- fireyear_summer_data %>%
  mutate(pred = fireyear_summer_pred,
         fire_year = exp(log_fire_year)) %>%
  left_join(
    ref_summer_data %>%
      mutate(ref_pred = ref_summer_pred) %>%
      dplyr::select(id, ref_pred)
  ) %>%
  mutate(rss = pred - ref_pred,
         season = "Summer")

tri_summer_rss <- tri_summer_data %>%
  mutate(pred = tri_summer_pred,
         tri = mrdtm) %>%
  left_join(
    ref_summer_data %>%
      mutate(ref_pred = ref_summer_pred) %>%
      dplyr::select(id, ref_pred)
  ) %>%
  mutate(rss = pred - ref_pred,
         season = "Summer")

openlc_summer_rss <- openlc_summer_data %>%
  mutate(pred = openlc_summer_pred) %>%
  left_join(
    ref_summer_data %>%
      mutate(ref_pred = ref_summer_pred) %>%
      dplyr::select(id, ref_pred)
  ) %>%
  mutate(rss = pred - ref_pred,
         season = "Summer")

### Winter RSS predictions ----
issa_data_winter_comb <- readRDS(here("data/processed/issa_data_winter_comb.rds"))
m0_winter_glmmfit <- readRDS(here("output/m0_winter_glmmfit.rds"))

ref_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    snow_depth = mean(snow_depth),
                    open_land = 0,
                    log_fire_year = log(25),
                    strat_id = NA,
                    id = id
  )

snow_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = log(modelr::seq_range(1:200,20)),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = 0,
                    log_fire_year = log(25),
                    snow_depth = c(0.04,0.4,0.7),
                    strat_id = NA,
                    id = id
  )

ref_snow_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = 0,
                    log_fire_year = log(25),
                    snow_depth = c(0.04,0.4,0.7),
                    strat_id = NA,
                    id = id
  )

fireyear_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = 0,
                    log_fire_year = log(15:45),
                    snow_depth = mean(snow_depth),
                    strat_id = NA,
                    id = id
  )

tri_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = modelr::seq_range(mrdtm,20),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = 0,
                    log_fire_year = log(25),
                    snow_depth = mean(snow_depth),
                    strat_id = NA,
                    id = id
  )

openlc_winter_data <- issa_data_winter_comb %>%
  modelr::data_grid(log_sl = mean(log_sl),
                    cos_ta = mean(cos_ta),
                    scanfi_prcC_end = mean(scanfi_prcC_end),
                    mrdtm = mean(mrdtm),
                    dtm_diff = mean(dtm_diff),
                    log_water_dist = mean(log_water_dist),
                    open_land = c(0,1),
                    log_fire_year = log(25),
                    snow_depth = mean(snow_depth),
                    strat_id = NA,
                    id = id
  )


ref_winter_pred <- predict(m0_winter_glmmfit,
                           newdata = ref_winter_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE)

snow_pred <- predict(m0_winter_glmmfit, 
                                newdata = snow_data,
                                type = "link",
                                re.form = NULL,
                                allow.new.levels=TRUE)

ref_snow_pred <- predict(m0_winter_glmmfit, 
                     newdata = ref_snow_data,
                     type = "link",
                     re.form = NULL,
                     allow.new.levels=TRUE)

fireyear_winter_pred <- predict(m0_winter_glmmfit, 
                         newdata = fireyear_winter_data,
                         type = "link",
                         re.form = NULL,
                         allow.new.levels=TRUE)

tri_winter_pred <- predict(m0_winter_glmmfit, 
                           newdata = tri_winter_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE)

openlc_winter_pred <- predict(m0_winter_glmmfit, 
                           newdata = openlc_winter_data,
                           type = "link",
                           re.form = NULL,
                           allow.new.levels=TRUE)

snow_rss <- snow_data %>%
  mutate(pred = snow_pred,
         step_length = exp(log_sl)) %>%
  left_join(
    ref_snow_data %>%
      mutate(ref_pred = ref_snow_pred) %>%
      dplyr::select(id, ref_pred, snow_depth)
  ) %>%
  mutate(rss = pred - ref_pred,
         snow_cat = ifelse(snow_depth == 0.04, "Shallow (0 cm)",
                           ifelse(snow_depth == 0.4, 
                                  "Moderate (40 cm)","Deep (80 cm)")),
         snow_cat = factor(snow_cat, levels = c("Shallow (0 cm)", "Moderate (40 cm)", "Deep (80 cm)")))


fireyear_winter_rss <- fireyear_winter_data %>%
  mutate(pred = fireyear_winter_pred,
         fire_year = exp(log_fire_year)) %>%
  left_join(
    ref_winter_data %>%
      mutate(ref_pred = ref_winter_pred) %>%
      dplyr::select(id, ref_pred)
  ) %>%
  mutate(rss = pred - ref_pred,
         season = "Winter")

tri_winter_rss <- tri_winter_data %>%
  mutate(pred = tri_winter_pred,
         tri = mrdtm) %>%
  left_join(
    ref_winter_data %>%
      mutate(ref_pred = ref_winter_pred) %>%
      dplyr::select(id, ref_pred)
  ) %>%
  mutate(rss = pred - ref_pred,
         season = "Winter")

openlc_winter_rss <- openlc_winter_data %>%
  mutate(pred = openlc_winter_pred) %>%
  left_join(
    ref_winter_data %>%
      mutate(ref_pred = ref_winter_pred) %>%
      dplyr::select(id, ref_pred)
  ) %>%
  mutate(rss = pred - ref_pred,
         season = "Winter")

### RSS plots ----
fireyear_rss <- fireyear_summer_rss %>%
  bind_rows(fireyear_winter_rss)

tri_rss <- tri_summer_rss %>%
  bind_rows(tri_winter_rss)

openlc_rss <- openlc_summer_rss %>%
  bind_rows(openlc_winter_rss) %>%
  mutate(land_cover = ifelse(open_land == 1, "Shrubland/grassland", "Forest"))

ggplot(data = fireyear_rss, aes(x = fire_year, y = rss)) +
  geom_line(aes(group = id), colour = "darkgrey", linetype = "dotted") +
  geom_smooth(method = "loess", formula = y~x) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~season, nrow = 1) +
  xlab("Years since fire") + 
  ylab("Relative selection strength") +
  theme_proj

ggsave("output/figures/fireyear_rss.png", width = 6, height = 4)


ggplot(data = tri_rss, aes(x = tri, y = rss)) +
  geom_line(aes(group = id), colour = "darkgrey", linetype = "dotted") +
  geom_smooth(method = "loess", formula = y~x) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~season, nrow = 1) +
  xlab("Terrain ruggedness index") + 
  ylab("Relative selection strength") +
  theme_proj
  

ggsave("output/figures/tri_rss.png", width = 6, height = 3)

ggplot(data = openlc_rss, aes(x = land_cover, y = rss)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~season, nrow = 1) +
  xlab("") + 
  ylab("Relative selection strength") +
  theme_proj

ggsave("output/figures/openlc_rss.png", width = 6, height = 4)

ggplot(data = snow_rss, aes(x = step_length, y = rss)) +
  geom_line(aes(group = id), colour = "darkgrey", linetype = "dotted") +
  geom_smooth(method = "loess", formula = y~x) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~snow_cat) +
  xlab("Step length (m)") + 
  ylab("Relative selection strength") +
  theme_proj

ggsave("output/figures/snow_rss.png", width = 6, height = 4)

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

