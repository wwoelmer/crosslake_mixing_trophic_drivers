library(tidyverse)
library(readxl)
library(rLakeAnalyzer)


ctd <- read.csv('./data/processed_data/BoP_ctd_2003_2022.csv')

#################################################################
# calculate thermocline depth and schmidt stability

# read in bathy estimates
bty <- read_excel('./data/raw_data/Rotlakes_bathymetry.xls', skip = 1) 
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 
                   'planar_sa_m2', 'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)

# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m

##################################################################################

t_metrics <- ctd %>% 
  select(lake, site, date, depth_m, temp_C) %>% 
  filter(!is.na(temp_C)) %>% 
  distinct(lake, site, date, depth_m, .keep_all = TRUE) %>% 
  group_by(lake, site, date) %>% 
  mutate(thermo_depth = thermo.depth(temp_C, depth_m, seasonal = FALSE, mixed.cutoff = 1.5),
         thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
         schmidt_stability = schmidt.stability(temp_C, 
                                               depth_m, 
                                               bthA = bty$model_sd_m2, 
                                               bthD = bty$depth_m),
         strat = ifelse(thermo_depth > 0, 1, 0)) %>% 
  select(date, everything()) %>% 
  distinct(date, lake, site, .keep_all = TRUE)

ggplot(t_metrics, aes(x = as.Date(date), y = thermo_depth, 
                      color = site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake)

t_metrics <- t_metrics %>% 
  select(date, lake, site, thermo_depth, schmidt_stability, strat)

#######################################################################
# combine and write file
write.csv(t_metrics, './data/processed_data/BoP_thermalmetrics_2003_2022.csv', row.names = FALSE)
