# load libraries
library(tidyverse)
library(readxl)
library(vegan)
rm(list = ls())

# 01 read data------------
abundance <- read_excel('ar_fish_data.xlsx', sheet = 'abundance')

# biomass data
biomass <- read_excel('ar_fish_data.xlsx', 2)

# score data
scores_fishing <-
  read_excel('ar_fish_data.xlsx', sheet = 'scores') %>%
  dplyr::select(Species, fish_resource)

# score vulnerability
scores_vul <-
  read_excel('ar_fish_data.xlsx', sheet = 'scores') %>%
  dplyr::select(Species, vuner)

# trophic level data
trophic_level <- 
  read_excel('ar_fish_data.xlsx', sheet = 'scores') %>%
  dplyr::select(Species, troph)
  
# factors
factores <-  read_excel('ar_fish_data.xlsx', 4)

# load index calculator functions-----
source('index_calculator_func.R')

# 2 Indices calculations-----------

# 2.1 Community structure index-------
cc_index <- index_calculator_cs(abundance)

# 2.2 Trophic level--------------
TL_index <- index_calculator_TL(biomass)

# 2.3 Vulnerability index-------
vuln_index <- index_calculator_vul(abundance)

# 2.4 Fishing resource index----------
fishing_resource_index <- index_calculator_fishing(abundance)

# 03 Combine all indices and save as .csv file  ---------
all_indices <-
  full_join(cc_index, TL_index, by = "Cod") %>%
  full_join(vuln_index, by = "Cod") %>%
  full_join(fishing_resource_index, by = "Cod") %>%
  mutate(ARI =  (cc_index*.25) + (TL_index*.25) + (vul_index*.25) + (fishing_index*.25)) %>%
  print(n = 10) %>% 
  write_csv('indices.csv')

