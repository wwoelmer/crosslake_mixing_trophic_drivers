# script to process WQ data from LAWA: https://www.lawa.org.nz/download-data

#install.packages('zoo')
#install.packages('plotly')
library(readxl)
library(tidyverse)
library(plotly)
library(zoo)

multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

# specifying the path name
path <- './data/raw_data/lawa-lake-monitoring-data-2004-2023_statetrendtli-results_sep2024.xlsx'
dat <- multiplesheets(path)

# create dataframes from list
list2env(dat, envir = .GlobalEnv)

########################### WQ Data #################################
# rename something easier to type
wqdata <- `Monitoring Dataset (2004-2023)`

# clean up and filter the data
wqdata <- wqdata %>% 
  filter(Region=='bay of plenty') %>% 
  select(SiteID, LTypeMixingPattern, Indicator, SampleDateTime, Value, Units) %>% 
  rename(site = SiteID,                        # rename variables
         mixing_state = LTypeMixingPattern,
         variable = Indicator,
         date = SampleDateTime,
         value = Value,
         units = Units) %>% 
  select(site, date, mixing_state, variable, value, units) # reorder columns

wqdata <- wqdata %>% 
  mutate(original_site = site) %>% 
  separate(site, c('blank', 'lake', 'blank2', 'blank3', 'site', 'blank4')) %>% 
  select(-c('blank', 'blank2', 'blank4'))

# fix Okawa Bay issue
wqdata <- wqdata %>% 
  mutate(site = ifelse(blank3!='Site', blank3, site)) %>% 
  select(-c('blank3', 'original_site'))

# remove dups
wqdata <- wqdata %>% 
  distinct(lake, mixing_state, site, date, variable, .keep_all = TRUE)

ggplot(wqdata, aes(x = as.Date(date), y = value, color = lake)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

# wide format
wq_wide <- wqdata %>% 
  pivot_wider(names_from = variable, values_from = value) 

# add units to col names of values
colnames(wq_wide) <- c('lake', 'site', 'laketype', 'date', 'chla_ugL_INT', 'NH4_mgL', 'pH',
                       'secchi_m', 'TN_mgm3', 'TP_mgm3')

wq_wide$depth_m <- '0.1' # really this is an integrated sample, but for ease of dataframe combining we will call it 0.1
wq_wide$method <- 'integrated'


###############################################################################################
# interpolate missing data
wq_wide <- wq_wide[order(wq_wide$date),]

wq_wide <- wq_wide %>% 
  group_by(lake, site) %>% 
  mutate(chla_ugL_INT = na.approx(chla_ugL_INT, na.rm = FALSE, rule = 2, maxgap = 15),
         NH4_mgL = na.approx(NH4_mgL, na.rm = FALSE, rule = 2, maxgap = 15),
         TN_mgm3 = na.approx(TN_mgm3, na.rm = FALSE, rule = 2, maxgap = 15),
         TP_mgm3 = na.approx(TP_mgm3, na.rm = FALSE, rule = 2, maxgap = 15),
         secchi_m = na.approx(secchi_m, na.rm = FALSE, rule = 2, maxgap = 15),
         pH = na.approx(pH, na.rm = FALSE, rule = 2, maxgap = 15))

###############################################################################################
# creat TN:TP ratio
wq_wide <- wq_wide %>% 
  mutate(TN_TP = TN_mgm3/TP_mgm3)

ggplot(wq_wide, aes(x = date, y = TN_TP)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free')

wq_wide %>% 
  mutate(month = month(date)) %>% 
  filter(lake=='Rotoehu') %>% 
  ggplot(aes(x = date, y = TN_TP)) +
  geom_point() +
  geom_smooth(method = 'lm')

wq_wide %>% 
  mutate(month = month(date)) %>% 
  filter(lake=='Rotoehu') %>% 
  ggplot(aes(x = date, y = TN_TP)) +
  geom_line() +
  geom_point(aes(x = date, y = TN_TP, color = as.factor(month)))

wq_wide %>% 
  mutate(month = month(date)) %>% 
  #filter(lake=='Rotoehu') %>% 
  ggplot(aes(x = as.factor(month), y = TN_TP, fill = as.factor(month))) +
  geom_boxplot(varwidth = TRUE) +
  #geom_jitter() +
  facet_wrap(~lake, scales = 'free')

ggplot(wq_wide, aes(x = TN_TP)) +
  geom_histogram() +
  facet_wrap(~lake, scales = 'free_y')

ggplot(wq_wide, aes(y = TN_TP, color = paste0(lake, site))) +
  geom_boxplot() +
  ylim(0, 100)

summ_tntp <- wq_wide %>% 
  group_by(lake) %>% 
  summarise_at(vars(TN_TP), list(min = min, mean = mean,  max = max), na.rm = TRUE) %>% 
  arrange(mean)
summ_tntp

###############################################################################################
# save file
write.csv(wq_wide, './data/processed_data/BoP_wq_2007_2021.csv', row.names = FALSE)

# select just Rotoehu data
rotoehu <- wqdata %>% 
  filter(lake=='Rotoehu')

# plot some data
ggplot(rotoehu, aes(x = date, y = value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y') +
  theme_bw()

ggplot(wqdata, aes(x = date, y = value, color = lake, shape = site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = 'free') +
  theme_bw()

######################## Lake State data ######################################
# rename something easier to work with
lstate <- `Lake State`

# clean up and filter the data
lstate <- lstate %>% 
  filter(Region=='bay of plenty') %>% 
  select(SiteID, `LakeType-Mixing`:`Maximum_numeric attribute statistic`)

colnames(lstate) <- c('Site', 'LakeType', 'HydroYear', 'variable', 'units', 'median_display', 'attribute_band', 
                      'median_attr_statistic', 'maximum_attr_statistic')

# select just Rotoehu data
lstate <- lstate %>% 
  filter(grepl("Lake Rotoehu", Site)) 

# format and remove NA's
lstate <- lstate %>% 
  mutate(median_display = as.numeric(median_display)) %>% 
  select(Site:median_display)

lstate <- na.omit(lstate)

# plot some data
ggplot(lstate, aes(x = variable, y = median_display, fill = variable)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~variable, scales= 'free') +
  theme_bw() +
  geom_text(aes(label = median_display), vjust = 'top') +
  ggtitle('2021 Lake State, Lake Rotoehu')

# only 2021 lake states...

##################### TLI Data ############################################
tli <- LakeTLI

# clean up and filter the data
tli <- tli %>% 
  filter(Region=='bay of plenty') %>% 
  select(lake, `Hydrological Year (Jul - Jun)`, TLI)
colnames(tli) <- c('Site', 'HydroYear', 'TLI')

# select just Rotoehu data
tli <- tli %>% 
  filter(grepl("Lake Rotoehu", Site))

ggplot(tli, aes(x = HydroYear, y = TLI)) +
  geom_line() +
  geom_point(size = 2) +
  geom_hline(yintercept = 3.9) +
  ylim(0, 6)
