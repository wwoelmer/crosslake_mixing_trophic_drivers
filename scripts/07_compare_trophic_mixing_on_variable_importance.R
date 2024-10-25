# look at the delta AICc across lakes and drivers
library(tidyverse)
'%notin%' <- Negate('%in%')

# read in original data to get continuous estimates of trophic state (mean TLI) or mixing dynamics (mean schmidt stability?)
cat <- read.csv('./data/all_lakes_TLI_drivers.csv')
cat <- cat %>% 
  select(lake, schmidt_stability, tli_annual)

cat <- cat %>% 
  group_by(lake) %>% 
  summarise(mean_TLI = mean(tli_annual, na.rm = TRUE),
            mean_stability = mean(schmidt_stability, na.rm = TRUE))

ggplot(cat, aes(x = reorder(lake, mean_TLI), y = mean_TLI, color = lake)) +
  geom_point(size = 5) +
  theme_bw()

ggplot(cat, aes(x = reorder(lake, mean_stability), y = mean_stability, color = lake)) +
  geom_point(size = 5) +
  theme_bw()

# read in model output
out <- read.csv('./data/model_output_all_lakes.csv')
out$lake <- factor(out$lake, levels = c('Rotorua', 'Rotoehu', 'Rerewhakaaitu', 'Okaro', 'Okareka', 'Tarawera'))
out <- out %>% 
  select(lake, covar, value, id_covar, aic, r2, iter_start:end_date)

# combine output with trophic state and mixing categories
out <- full_join(out, cat)

################################################################################
# calculate the difference across variables
out_prop <- out %>% 
  distinct(lake, id_covar, iter_start, .keep_all = TRUE) %>% 
  group_by(iter_start, lake) %>% 
  mutate(diff_from_best = max(aic) - aic,
         rank = dense_rank(desc(aic)),
         aic_none = aic[id_covar=='none'],
         diff_from_none = aic_none - aic)

ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~lake) +
  ylab('Difference from AR only') +
  xlab('Start of Iteration') +
  labs(color = 'Driver') +
  theme(text=element_text(size=18))

ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = lake)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~id_covar, scales = 'free_y') +
  geom_hline(yintercept = 0) +
  #scale_color_gradient(low = 'royalblue', high = 'orange') +
  ylab('Improvement over AR model alone') +
  xlab('Start of Iteration') +
  labs(color = 'Lake') +
  theme(text=element_text(size=18))

ggplot(out_prop, aes(x = lake, y = diff_from_none, fill = lake)) +
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
  filter(id_covar %notin% c('none', 'Kd')) %>% 
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
  ggplot(aes(x = mixing_state, y = diff_from_none, fill = mixing_state)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Set2') +
  facet_wrap(~id_covar, scales = 'free') +
  theme_bw() +
  stat_compare_means(aes(group = mixing_state),
                     label = "p.signif", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5)


nut_cat <- data.frame(lake = c('Okareka',
                               'Okaro',
                               'Rerewhakaaitu',
                               'Rotoehu',
                               'Rotorua',
                               'Tarawera'),
                      nutrient_cat = c('low',
                                       'low',
                                       'high',
                                       'high',
                                       'high',
                                       'low'))
out_prop <- full_join(out_prop, nut_cat)
out_prop %>% 
  filter(id_covar %notin% c('none', 'Kd')) %>% 
  ggplot(aes(x = as.factor(nutrient_cat), y = diff_from_none, fill = nutrient_cat)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~id_covar, scales = 'free') +
  theme_bw() +
  stat_compare_means(aes(group = nutrient_cat),
                     label = "p.signif", label.y.npc = 0.95,
                     size = 4,
                     label.x.npc = 0.5)

#####################################################################################
# run generalized linear models on the two
TS <- glm(diff_from_none ~ nutrient_cat, data = out_prop, family = gaussian())
mix <- glm(diff_from_none ~ mixing_state, data = out_prop, family = gaussian())
summary(mix)
summary(TS)

tli <- glm(diff_from_none ~ mean_TLI, data = out_prop, family = gaussian())
stability <- glm(diff_from_none ~ mean_stability, data = out_prop, family = gaussian())
summary(tli)
summary(stability)


####################################################################################
# also look at the time series of AIC
ggplotly(ggplot(out_prop, aes(x = as.Date(start_date), y = diff_from_none, color = id_covar)) +
           geom_point(size = 2) +
           theme_bw() +
           facet_wrap(~lake) +
           ylab('Difference from AR only (none) model') +
           xlab('Start of Iteration') +
           labs(color = 'Covariate') +
           theme(text=element_text(size=18)))