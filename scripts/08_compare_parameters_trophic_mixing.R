#  compare the difference in parameter values between mixing regime and trophic state
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

a <- ggplot(cat, aes(x = reorder(lake, mean_TLI), y = mean_TLI, color = lake)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab('Lake') +
  ylab('Long-term TLI')

b <- ggplot(cat, aes(x = reorder(lake, mean_stability), y = mean_stability, color = lake)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab('Lake') +
  ylab('Long-term Schmidt stability')

ggarrange(a, b, labels = 'auto', common.legend = TRUE)

ggplot(cat, aes(x = mean_stability, y = mean_TLI, color = lake)) +
  geom_point(size = 5) +
  theme_bw()

ggplot(cat, aes(x = mean_TLI, y = mean_stability, color = lake)) +
  geom_point(size = 5) +
  theme_bw()

# read in model output
out <- read.csv('./data/model_output_all_lakes.csv')
#out$lake <- factor(out$lake, levels = c('Rotorua', 'Rotoehu', 'Rerewhakaaitu', 'Okaro', 'Okareka', 'Tarawera'))
out <- out %>% 
  select(lake, covar, value, id_covar, r2, iter_start:end_date)

# combine output with trophic state and mixing categories
out <- full_join(out, cat)

# filter parameters to just the driver coefficient
out2 <- out %>% 
  filter(covar==id_covar)

# run generalized linear models on the two
tli <- glm(value ~ mean_TLI, data = out2, family = gaussian())
stability <- glm(value ~ mean_stability, data = out2, family = gaussian())
summary(tli)
summary(stability)

ss_res <- sum(residuals(stability)^2)
ss_tot <- sum((out2$value - mean(out2$value))^2)
r2 <- 1 - (ss_res / ss_tot)
r2


ss_res <- sum(residuals(tli)^2)
ss_tot <- sum((out2$value - mean(out2$value))^2)
r2 <- 1 - (ss_res / ss_tot)
r2


ggplot(out2, aes(x = mean_TLI, y = value, color = id_covar)) +
  geom_point() +
  facet_wrap(~id_covar, scales = 'free')

ggplot(out2, aes(x = mean_stability, y = value, color = id_covar)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~id_covar, scales = 'free')


################################################################################
## look at predictability vs parameter vaues
test_vars <-  c( "DRP_mgm3_bottom", "NH4_mgm3_bottom", "NNN_mgm3_bottom", 
                 "DO_sat_bottom", "Kd", "PAR_umolm2s_top",
                 "temp_C_top", 
                 "temp_C_bottom",
                 "schmidt_stability") 

out_stdized <- out %>% 
  select(lake, covar, value, id_covar, r2, iter_start) %>% 
  group_by(lake, iter_start) %>% 
  mutate(r2_none = ifelse(id_covar=='none', r2, r2[id_covar=='none']),
         diff_from_none = r2 - r2_none)

out_stdized %>% 
  filter(covar %in% test_vars) %>%  
  ggplot(aes(x = diff_from_none, y = (value), color = lake)) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  geom_point(size = 2) +
  facet_wrap(~covar, scales = 'free') +
  theme_bw() +
  xlab('Relative increase in R2') +
  ylab('Parameter Value') +
  theme(text=element_text(size=15))

lm_out <- out_stdized %>% 
  filter(covar %in% test_vars) %>%  
  group_by(lake, covar) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(value ~ diff_from_none, data = .)),
         tidy_output = map(model, broom::tidy)) %>%
  unnest(tidy_output) %>%
  select(lake, covar, term, estimate) 


lm_out %>% 
  filter(term=='diff_from_none') %>% 
  ggplot(aes(x = lake, y = abs(estimate), color = estimate >=0)) +
  geom_point(size = 2) +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw() +
  geom_hline(yintercept = 0) +
  theme(text=element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Slope (sensitivity to variable)')


lm_out %>% 
  filter(term=='(Intercept)') %>% 
  ggplot(aes(x = lake, y = abs(estimate), color = estimate >=0)) +
  geom_point(size = 2) +
  facet_wrap(~covar, scales = 'free_y') +
  theme_bw() +
  geom_hline(yintercept = 0) +
  theme(text=element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Intercept (added value of variable if number is low)')

