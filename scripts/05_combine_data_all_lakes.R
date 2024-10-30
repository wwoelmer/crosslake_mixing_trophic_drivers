# combine all data into one
library(tidyverse)
library(zoo)


lakes <- c('Okaro', 'Tarawera', 'Rotorua', 'Rotoehu', 'Okareka', 'Rerewhakaaitu')

################################################################################
# water quality monitoring data
wq <- read.csv('./data/processed_data/BoP_WQ_2000_2021.csv')

unique(paste0(wq$lake, wq$site))
# should we use rotorua site 2 or 5? go with 5?

wq <- wq %>% 
  filter(lake %in% lakes) %>% 
  filter((lake=='Rotoiti' & site==3)|
           (lake=='Rotorua' & site==2)|
           (lake=='Rotomahana' & site==2)|
           !(lake %in% c('Rotoiti', 'Rotorua', 'Rotomahana')))


ggplot(wq, aes(x = as.Date(date), y = DRP_mgm3_bottom, color = site)) +
  geom_point() +
  facet_wrap(~lake)

#select the variables I want to keep for the moving window analysis
wq <- wq %>% 
  select(date, lake, DRP_mgm3_bottom, NH4_mgm3_bottom, NNN_mgm3_bottom, 
         secchi_m, TN_mgm3_top, TP_mgm3_top, chla_mgm3_top) ## keep the TLI variables

wq$date <- as.Date(wq$date)

################################################################################
# ctd data
ctd <- read.csv('./data/processed_data//BoP_ctd_2003_2022.csv')

ctd <- ctd %>% 
  filter(lake %in% lakes) %>% 
  filter((lake=='Rotoiti' & site==3)|
           (lake=='Rotorua' & site==2)|
           (lake=='Rotomahana' & site==2)|
           !(lake %in% c('Rotoiti', 'Rotorua', 'Rotomahana'))) %>% 
  select(lake, date, depth_m, DO_sat, PAR_umolm2s, spcond_uScm, temp_C)

# calculate top and bottom values
ctd_long <- ctd %>% 
  pivot_longer(DO_sat:temp_C, names_to = 'variable', values_to = 'value') %>% 
  group_by(lake, date) %>% 
  mutate(max_depth = max(depth_m)) %>% 
  group_by(lake, date, variable) %>% 
  mutate(bottom = value[which.max(depth_m)],
         top = value[which.min(depth_m) + 1]) %>% 
  distinct(lake, date, variable, .keep_all = TRUE)

ctd_tb <- ctd_long %>% 
  select(-depth_m, -value, -max_depth) %>% 
  pivot_wider(names_from = 'variable', values_from = c('bottom', 'top'),
              names_glue = "{variable}_{.value}")
ctd_tb$date <- as.Date(ctd_tb$date)

ggplot(ctd_tb, aes(x = as.Date(date), y = temp_C_bottom, color = lake)) +
  geom_point() +
  facet_wrap(~lake)

## combine ctd and wq
df <- left_join(wq, ctd_tb, by = c('lake', 'date'))


#################################################################################
## add in mixing metrics
mix <- read.csv('./data/processed_data/BoP_thermalmetrics_2003_2022.csv')
mix$date <- as.Date(mix$date)

# get rid of site 2 for rotorua and only site 3 for rotoiti
mix <- mix %>% 
  filter(lake %in% lakes) %>% 
  filter((lake=='Rotoiti' & site==3)|
           (lake=='Rotorua' & site==2)|
           (lake=='Rotomahana' & site==2)|
           !(lake %in% c('Rotoiti', 'Rotorua', 'Rotomahana')))
unique(paste0(mix$lake, mix$site))

mix <- mix %>% 
  select(-site)

df2 <- left_join(df, mix, by = c('date', 'lake'))

#################################################################################
## add in Kd
kd <- read.csv('./data/processed_data/BoP_lightextinction_2003_2022.csv')
kd$date <- as.Date(kd$date)

kd <- kd %>% 
  filter(lake %in% lakes) %>% 
  filter((lake=='Rotoiti' & site==3)|
           (lake=='Rotorua' & site==2)|
           (lake=='Rotomahana' & site==2)|
           !(lake %in% c('Rotoiti', 'Rotorua', 'Rotomahana'))) %>% 
  select(-site)

df3 <- left_join(df2, kd)

################################################################################
# remove NA's and interpolate?????
# interpolate missing data
df3 <- df3 %>% 
  arrange(lake, date)

df3 <- df3 %>% 
  group_by(lake) %>% 
  mutate(DRP_mgm3_bottom = na.approx(DRP_mgm3_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(NH4_mgm3_bottom = na.approx(NH4_mgm3_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(NNN_mgm3_bottom = na.approx(NNN_mgm3_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(secchi_m = na.approx(secchi_m, na.rm = FALSE, rule = 2, maxgap = 15)) %>%   
  mutate(TN_mgm3_top = na.approx(TN_mgm3_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>%   
  mutate(TP_mgm3_top = na.approx(TP_mgm3_top, na.rm = FALSE, rule = 2, maxgap = 15))   %>% 
  mutate(chla_mgm3_top = na.approx(chla_mgm3_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(DO_sat_bottom = na.approx(DO_sat_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(PAR_umolm2s_bottom = na.approx(PAR_umolm2s_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(spcond_uScm_bottom = na.approx(spcond_uScm_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(temp_C_bottom = na.approx(temp_C_bottom, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(DO_sat_top = na.approx(DO_sat_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(PAR_umolm2s_top = na.approx(PAR_umolm2s_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(spcond_uScm_top = na.approx(spcond_uScm_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(temp_C_top = na.approx(temp_C_top, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(schmidt_stability = na.approx(schmidt_stability, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(thermo_depth = na.approx(thermo_depth, na.rm = FALSE, rule = 2, maxgap = 15)) %>% 
  mutate(strat = na.approx(strat, na.rm = FALSE, rule = 2, maxgap = 15)) 

#################################################################################
### Calculate the TLI
# calculate monthly TLI
source('./scripts/functions/tli_fx.R')

df4 <- df3 %>%
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, year, lake) %>%
  mutate(tli_monthly = tli_fx(chl = chla_mgm3_top, TN = TN_mgm3_top, TP = TP_mgm3_top, secchi = secchi_m)) %>% 
  group_by(year, lake) %>%
  mutate(tli_annual = tli_fx(chl = chla_mgm3_top, TN = TN_mgm3_top, TP = TP_mgm3_top, secchi = secchi_m))

hist(df4$tli_monthly)
tli <- ggplot(df4, aes(x = as.Date(date), y = tli_monthly,  color = lake)) +
  geom_point(size = 1.2) +
  #  facet_wrap(~lake) +
  geom_line(aes(x = as.Date(date), y = tli_annual), size = 2) +
  theme_bw()
tli

df4 %>% 
  distinct(year, tli_annual, .keep_all = TRUE) %>% 
  ggplot(aes(x = as.Date(date), y = tli_annual)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lake) +
  ylim(2, 6) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  xlab('Date') +
  ylab('Annual TLI')

#################################################################################
# write to csv
write.csv(df4, './data/all_lakes_TLI_drivers.csv', row.names = FALSE)


##################################################################################
# calculate normalized drivers for comparison across lakes
df_norm <- df4 %>% 
  select(-c(strat, month, year)) %>% 
  pivot_longer(DRP_mgm3_bottom:Kd, names_to = 'variable', values_to = 'value') %>% 
  group_by(lake, variable) %>% 
  mutate(min = min(value, na.rm = TRUE),
         max = max(value, na.rm = TRUE),
         value_norm = (value - min)/(max - min))

ggplotly(ggplot(df_norm, aes(x = as.Date(date), y = value_norm, color = lake)) +
  geom_point() +
  facet_wrap(~variable))

df_wide <- df_norm %>% 
  select(-value, -min, -max) %>% 
  pivot_wider(names_from = 'variable', values_from = 'value_norm')

write.csv(df_wide, './data/all_lakes_TLI_normalized_drivers.csv', row.names = FALSE)
