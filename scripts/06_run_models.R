# run simple AR model for TLI + one covariate from set of potential driver variables


library(tidyverse)
library(plotly)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(statcomp)
'%notin%' <- Negate('%in%')

# read in data
#dat <- read.csv('./data/processed_data/BoP_wq_2007_2021.csv')
dat <- read.csv('./data/all_lakes_TLI_normalized_drivers.csv')

# subset to when the CTD data starts
dat <- dat %>% 
  filter(date > as.Date('2003-03-17'))

#######################################################
# run the ar model simulation
source('./scripts/functions/run_ar.R')

# this set of variables was determined based on theoretical within-lake drivers of TLI
test_vars <-  c( "DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom", 
                 "DO_sat_bottom", "Kd", "PAR_umolm2s_top",
                 "temp_C_top", 
                 "temp_C_bottom",
                 "schmidt_stability", 
                 "none") 

lakes <- unique(dat$lake)
id_var <- "tli_monthly"
window_length <- 100

obs_length <- dat %>% 
  count(lake)
n_iter <- seq(1, min(obs_length$n) - window_length)  

out <- data.frame()

for(i in 1:length(test_vars)){
  for(k in 1:length(lakes)){
    # subset to driver variable
    if(test_vars[i]=='none'){
      dat_ar <- dat %>% 
        ungroup() %>% 
        select(date, lake, id_var)  
    }else{
      dat_ar <- dat %>% 
        ungroup() %>% 
        select(date, lake, id_var, test_vars[i])  
    }
    print(test_vars[i])
    # subset to the right lake 
    
    dat_ar <- dat_ar %>% 
      dplyr::filter(lake==lakes[k])
    print(lakes[k])
    
    for(j in 1:length(n_iter)){
      print(n_iter[j])
      # subset to the 100 observations in the iteration
      start <- j
      end <- j + window_length
      dat_sub <- dat_ar[start:end,]
      dat_sub <- na.omit(dat_sub)
      opd <-  weighted_ordinal_pattern_distribution(x = dat_sub$tli_monthly, ndemb = 4)
      pe <- permutation_entropy(opd) 
      
      # run the model
      d <- run_ar(data = dat_sub, 
                  id_var = id_var, 
                  id_covar = test_vars[i], 
                  window_length = window_length)
      d$iter_start <- start
      d$iter_end <- end
      d$start_date <- min(dat_sub$date)
      d$end_date <- max(dat_sub$date)
      d$n <- nrow(dat_sub)
      d$pe <- pe
      d$lake <- lakes[k]
      out <- rbind(out, d)
      print(paste0(test_vars[i], " ", lakes[k], " ", n_iter[j]))
    }  
  }
  
}

write.csv(out, './data/model_output_all_lakes.csv', row.names = FALSE)

################################################################################
# assign an order to the lakes factor

# add TLI to out
lt_tli <- data.frame(lake = c('Okareka',
                              'Okaro',
                              'Rerewhakaaitu',
                              'Rotoehu',
                              'Rotorua',
                              'Tarawera'),
                     tli = c(3.16,
                             4.88,
                             3.54,
                             4.37,
                             4.43,
                             2.75))
lt_tli

out <- left_join(out, lt_tli)

out$lake <- factor(out$lake, levels = c('Rotorua', 'Rotoehu', 'Rerewhakaaitu', 'Okaro', 'Okareka', 'Tarawera'))

################################################################################
# look at PE results
out %>% 
  filter(id_covar!='meta_bot',
         id_covar!='meta_top',
         id_covar!='hypo_temp') %>% 
  ggplot(aes(x = pe, y = r2, color = lake)) +
  geom_point() +
  facet_wrap(~lake, scales = 'free') +
  geom_smooth(method = 'lm') +
  theme_bw()

out %>% 
  filter(id_covar!='meta_bot',
         id_covar!='meta_top',
         id_covar!='hypo_temp') %>% 
  ggplot(aes(x = pe, y = r2)) +
  geom_point() +
  #  facet_wrap(~lake, scales = 'free') +
  geom_smooth(method = 'lm') +
  theme_bw()





################################################################################
# calculate the difference across variables
out_prop <- out %>% 
  distinct(lake, id_covar, iter_start, .keep_all = TRUE) %>% 
  select(lake, id_covar:iter_end, start_date, end_date, r2, tli) %>% 
  group_by(iter_start, lake) %>% 
  mutate(diff_from_best = max(r2) - r2,
         rank = dense_rank(desc(r2)),
         r2_none = r2[id_covar=='none'],
         diff_from_none = r2 - r2_none)

diff_r2 <- ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_best, color = id_covar)) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~lake) +
  ylab('Difference from Best Performing Model') +
  xlab('Start of Iteration') +
  labs(color = 'Driver') +
  theme(text=element_text(size=18))
diff_r2
ggplotly(diff_r2)

##################################################################################################
#### look at difference from none model
# positive values indicate that driver model was better than the none model (better than autoregression alone)
ggplotly(ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
           geom_point(size = 2) +
           theme_bw() +
           facet_wrap(~lake) +
           ylab('Difference from AR only (none) model') +
           xlab('Start of Iteration') +
           labs(color = 'Covariate') +
           theme(text=element_text(size=18)))

out_prop %>% 
  ggplot(aes(x = as.Date(start_date), y = diff_from_none, color = lake)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~id_covar, scales = 'free_y') +
  geom_hline(yintercept = 0) +
  #scale_color_gradient(low = 'royalblue', high = 'orange') +
  ylab('Improvement over AR model alone') +
  xlab('Start of Iteration') +
  labs(color = 'Lake') +
  theme(text=element_text(size=18))

out_prop %>% 
  ggplot(aes(x = lake, y = diff_from_none, fill = lake)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~id_covar, scales = 'free_y') +
  geom_hline(yintercept = 0) +
  #scale_color_manual(values = c('#B22222', '#ED9121', 'seagreen','royalblue')) +
  scale_fill_manual(values = c('#B22222', '#D05A22', '#ED9121', '#C9F2C7', 'seagreen','royalblue')) +
  ylab('Improvement in R2 over AR model alone') +
  #xlab('Start of Iteration') +
  labs(color = 'Lake') +
  theme(text=element_text(size=18),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

out_prop$lake <- factor(out_prop$lake, levels = c('Okaro', 'Rotorua', 'Rotoehu', 'Rerewhakaaitu', 'Okareka', 'Tarawera'))
out_prop <- out_prop %>% 
  mutate(mixing_state = ifelse(lake %in% c('Rotorua', 'Rotoehu', 'Rerewhakaaitu'), 'polymictic', 'monomictic'))

out_prop %>% 
  filter(id_covar %notin% c('none')) %>% 
  ggplot(aes(x = lake, y = diff_from_none, fill = id_covar)) +
  geom_boxplot() +
  theme_bw() +
  # facet_wrap(~fct_rev(mixing_state), scales = 'free_x') +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(palette = 'Set3') +
  ylab('Improvement over AR model (R2)') +
  #  stat_compare_means(label = "p.signif", label.y.npc = 0.75, size = 4) + 
  stat_compare_means(aes(group = paste0(lake, id_covar)), 
                     label = "p.format", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5) +
  #xlab('Start of Iteration') +
  labs(fill = 'Driver') +
  theme(text=element_text(size=16),
        axis.text.x = element_text(angle = 25, vjust = 0.5))

out_prop %>% 
  filter(id_covar %notin% c('none'),
         lake %notin% 'Okaro') %>% 
  ggplot(aes(x = mixing_state, y = diff_from_none, fill = id_covar)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Set3') +
  facet_wrap(~id_covar, scales = 'free') +
  theme_bw() +
  stat_compare_means(aes(group = mixing_state),
                     label = "p.signif", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5)

out_prop %>% 
  filter(id_covar %notin% c('none')) %>% 
  ggplot(aes(x = mixing_state, y = diff_from_none, fill = lake)) +
  geom_boxplot() +
  scale_fill_manual(values = c('#B22222', '#D05A22', '#ED9121', '#C9F2C7', 'seagreen','royalblue')) +
  facet_wrap(~id_covar, scales = 'free') +
  theme_bw() +
  stat_compare_means(aes(group = mixing_state),
                     label = "p.signif", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5)

out_prop %>% 
  filter(id_covar=='temp_C_bottom') %>% 
  ggplot(aes(x = mixing_state, y = diff_from_none, fill = lake)) +
  geom_boxplot() +
  theme_bw() +
  # facet_wrap(~fct_rev(mixing_state), scales = 'free_x') +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(palette = 'Set3') +
  ylab('Improvement over AR model (R2)') +
  #  stat_compare_means(label = "p.signif", label.y.npc = 0.75, size = 4) + 
  stat_compare_means(aes(group = mixing_state), 
                     label = "p.format", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5) +
  #xlab('Start of Iteration') +
  labs(fill = 'Driver') +
  theme(text=element_text(size=16),
        axis.text.x = element_text(angle = 25, vjust = 0.5))

out_prop %>% 
  filter(id_covar=='schmidt_stability') %>% 
  ggplot(aes(x = mixing_state, y = diff_from_none, fill = lake)) +
  geom_boxplot() +
  theme_bw() +
  # facet_wrap(~fct_rev(mixing_state), scales = 'free_x') +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(palette = 'Set3') +
  ylab('Improvement over AR model (R2)') +
  #  stat_compare_means(label = "p.signif", label.y.npc = 0.75, size = 4) + 
  stat_compare_means(aes(group = mixing_state), 
                     label = "p.format", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5) +
  #xlab('Start of Iteration') +
  labs(fill = 'Driver') +
  theme(text=element_text(size=16),
        axis.text.x = element_text(angle = 25, vjust = 0.5))

out_prop %>% 
  filter(id_covar=='PAR_umolm2s_top') %>% 
  ggplot(aes(x = mixing_state, y = diff_from_none, fill = lake)) +
  geom_boxplot() +
  theme_bw() +
  # facet_wrap(~fct_rev(mixing_state), scales = 'free_x') +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(palette = 'Set3') +
  ylab('Improvement over AR model (R2)') +
  #  stat_compare_means(label = "p.signif", label.y.npc = 0.75, size = 4) + 
  stat_compare_means(aes(group = mixing_state), 
                     label = "p.format", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5) +
  #xlab('Start of Iteration') +
  labs(fill = 'Driver') +
  theme(text=element_text(size=16),
        axis.text.x = element_text(angle = 25, vjust = 0.5))

############################################################################
# look at parameter values
params <- out %>% 
  filter(covar %in% test_vars) %>% 
  mutate(mixing_state = ifelse(lake %in% c('Rotorua', 'Rotoehu', 'Rerewhakaaitu'), 'polymictic', 'monomictic'))


params$id_covar <- factor(params$id_covar, levels = 
                            c('epi_temp', 'Kd', 'PAR_umolm2s_top',
                              'NH4_mgm3_bottom', 'DRP_mgm3_bottom',
                              'NNN_mgm3_bottom', 'temp_C_bottom', 'DO_sat_bottom',
                              'schmidt_stability'))

# order lake by trophic level
params$lake <- factor(params$lake, levels = c('Okaro', 
                                              'Rotorua', 
                                              'Rotoehu', 
                                              'Rerewhakaaitu',
                                              'Okareka',
                                              'Tarawera'))

ggplot(params, aes(x = as.Date(start_date), y = value, color = lake)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  scale_color_manual(values = c('#C9F2C7', '#B22222', '#D05A22', '#ED9121', 'seagreen','royalblue')) +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  labs(color = 'Covariate') +
  theme(text=element_text(size=15))

ggplot(params, aes(x = value, y = lake, fill = lake)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, size = 1) +
  scale_fill_manual(values = c('#C9F2C7', '#B22222', '#D05A22', '#ED9121', 'seagreen','royalblue')) +
  facet_wrap(~id_covar) +
  theme_bw()

ggplot(params, aes(x = mixing_state, y = value, color = lake, fill = lake)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  #facet_grid(id_covar~mixing_state, scales = 'free') +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  scale_color_manual(values = c('#C9F2C7', '#B22222', '#D05A22', '#ED9121', 'seagreen','royalblue')) +
  scale_fill_manual(values = c('#C9F2C7', '#B22222', '#D05A22', '#ED9121', 'seagreen','royalblue')) +
  xlab('Start of Iteration') +
  ylab('Parameter Value') +
  theme(text=element_text(size=15),
        axis.text.x = element_text(angle = 65, vjust = 0.50))


ggplot(params, aes(x = mixing_state, y = value, fill = mixing_state)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  #facet_grid(id_covar~mixing_state, scales = 'free') +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  xlab('Start of Iteration') +
  scale_fill_manual(values = c('#1f77b4', '#ff7f0e')) +
  stat_compare_means(aes(group = mixing_state),
                     label = "p.signif", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5) +
  ylab('Parameter Value') +
  theme(text=element_text(size=15),
        axis.text.x = element_text(angle = 65, vjust = 0.50))


out %>% 
  filter(covar %in% test_vars) %>% 
  ggplot(aes(y = value, color = lake, fill = lake)) +
  geom_density(alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  scale_color_manual(values = c('#B22222', '#D05A22', '#ED9121', '#C9F2C7', 'seagreen','royalblue')) +
  scale_fill_manual(values = c('#B22222', '#D05A22', '#ED9121', '#C9F2C7', 'seagreen','royalblue')) +
  theme(text=element_text(size=15))

out %>% 
  filter(covar %in% test_vars) %>% 
  ggplot(aes(x = lake, y = value, color = id_covar, fill = id_covar)) +
  geom_boxplot() +
  #  geom_hline(yintercept = 0) +
  #facet_wrap(~id_covar, scales = 'free_y') +
  theme_bw() +
  theme(text=element_text(size=15))


################################################################################
## plot R2 on x and parameter value on y with color by window, facet by variable
out_stdized <- out %>% 
  filter(id_covar %in% c('epi_temp', 'temp_C_bottom', 'PAR_umolm2s_top',
                         'DRP_mgm3_bottom', 'NH4_mgm3_bottom', 'NNN_mgm3_bottom',
                         'schmidt_stability', 'DO_sat_bottom', 'none')) %>%  # get rid of met variables for now
  select(lake, covar, value, id_covar, r2, iter_start) %>% 
  group_by(lake, iter_start) %>% 
  mutate(r2_none = ifelse(id_covar=='none', r2, r2[id_covar=='none']),
         diff_from_none = r2 - r2_none)

out_stdized$covar <- factor(out_stdized$covar, levels = c("epi_temp", "PAR_umolm2s_top", 
                                                          "DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom",
                                                          "temp_C_bottom", "DO_sat_bottom", "schmidt_stability"))

out_stdized %>% 
  filter(covar %in% c('epi_temp', 'temp_C_bottom', 'PAR_umolm2s_top',
                      'DRP_mgm3_bottom', 'NH4_mgm3_bottom', 'NNN_mgm3_bottom',
                      'schmidt_stability', 'DO_sat_bottom', 'none')) %>%  # get rid of met variables for now
  ggplot(aes(x = diff_from_none, y = abs(value), color = lake)) +
  #  scale_color_manual(values = c('#B22222', '#ED9121', 'seagreen','royalblue')) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  geom_point(size = 2) +
  facet_wrap(~covar, scales = 'free') +
  theme_bw() +
  xlab('Relative increase in R2') +
  ylab('Parameter Value') +
  theme(text=element_text(size=15))


