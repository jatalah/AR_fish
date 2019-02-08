library(tidyverse)
library(readxl)
library(rfishbase)
library(vegan)

# read data------------
abundance <- read_excel('ar_fish_data.xlsx', 1)

# biomass data
biomass <- read_excel('ar_fish_data.xlsx', 2)

# score data
scores <-
  read_excel('ar_fish_data.xlsx', 3) %>%
  dplyr::select(Species, local_imp)

# factors-----
factores <-  read_excel('ar_fish_data.xlsx', 4)

# get fishase ecological parameters--------
fisbase_data <-
  scores %>%
  mutate(estimates = map(Species, rfishbase::estimate)) %>%
  select(estimates) %>%
  flatten_df() %>%
  select(Species, MaxLengthTL, Troph, a , K) %>%
  write_csv('fishbase_parameters.csv')

# check taxonomy------
# library(worrms)
# worm_names <-
#   scores %>%
#   mutate(estimates = map(Species, wm_records_taxamatch))


# 2 Indices calculations#

# Abundace----------
source('index_calculator_func.R')

index_abundance <- index_calculator(abundance)
# plots ----------------
breaks_abundance <-
  data.frame(y = quantile(index_abundance$imp_index, probs = seq(0, 1, 0.25)))


ggplot(index_abundance, aes(x = Season, y = imp_index)) +
  geom_boxplot() +
  theme_bw() +
  geom_point(aes(color= cat_index), size = 3, position = position_jitter()) +
  geom_hline(aes(yintercept = y),breaks_abundance, lty = 2) +
  scale_color_viridis_d(name = 'Importance \n category') +
  labs(y = "Abundance index of importance")


ggplot(index_abundance, aes(x = Year, y = imp_index, color = Season)) +
  geom_smooth() +
  geom_point(size = 3, position = position_jitter()) +
  geom_hline(aes(yintercept = y),breaks_abundance, lty = 2) +
  scale_color_discrete(name = 'Importance \n category') +
  theme_bw() +
  labs(y = "Abundance index of importance")

# Biomass ----------------
index_biomass <- index_calculator(biomass)

breaks_biomass <-
  data.frame(y = quantile(index_biomass$imp_index, probs = seq(0, 1, 0.25)))

ggplot(index_biomass, aes(x = Season, y = imp_index)) +
  geom_boxplot() +
  theme_bw() +
  geom_point(aes(color= cat_index), size = 3, position = position_jitter()) +
  geom_hline(aes(yintercept = y),breaks_biomass, lty = 2) +
  scale_color_viridis_d(name = 'Importance \n category') +
  labs(y = "Biomass index of importance")


ggplot(index_biomass, aes(x = Year, y = imp_index, color = Season)) +
  geom_smooth(se = F) +
  geom_point(size = 3, position = position_jitter()) +
  geom_hline(aes(yintercept = y),breaks_biomass, lty = 2) +
  scale_color_discrete(name = 'Importance \n category') +
  theme_bw() +
  labs(y = "Biomass index of importance")

ggplot(index_biomass, aes(x = Year, y = imp_index, color = Season)) +
  stat_summary(fun.data = 'mean_se')


