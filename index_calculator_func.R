# rescale variables between 0 and 1
range_0_1 <-
  function(x) {
    (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) * 1
  }


# 01 CS  index calculator---
index_calculator_cs <-
  function(data) {
    data %>%
      gather(Species, abund, -Cod) %>%
      group_by(Cod) %>%
      mutate(N = sum(abund),
             H = diversity(abund),
             S = specnumber(abund)) %>%
      left_join(biomass %>%
                  gather(Species, biomass, -Cod) %>%
                  group_by(Cod) %>%
                  mutate(B = sum(biomass)),
                by = 'Cod') %>%
      group_by(Cod) %>%
      summarise(
        N = mean(N),
        S = mean(S),
        B = mean(B),
        H = mean(H)
      ) %>%
      ungroup() %>%
      mutate(
        N_std = range_0_1(N),
        S_std = range_0_1(S),
        B_std = range_0_1(B),
        H_std = range_0_1(H)
      ) %>%
      transmute(Cod = Cod,
                cc_index = ((N_std + S_std + B_std + H_std) / 4) * 100)
  }

# 02 Trophic level index calculator --------------
index_calculator_TL <-
  function(data) {
    data %>%
      gather(Species, biomass, -Cod) %>%
      group_by(Cod) %>%
      left_join(trophic_level, by = 'Species') %>%
      group_by(Cod) %>%
      mutate(troph_biomass = troph * biomass,
             tot_biomass = sum(biomass)) %>%
      summarise(TL_index = sum(troph_biomass) / first(tot_biomass))
  }

# 03 vulnerability index-----
index_calculator_vul <-
  function(data) {
    data %>%
      gather(Species, abund, -Cod) %>%
      left_join(scores_vul, by = 'Species') %>%
      group_by(Cod) %>%
      mutate(N = sum(abund)) %>%
      group_by(Cod, vuner) %>%
      mutate(prop_n = sum(abund) / N) %>%
      summarise(n = mean(prop_n, na.rm = T) * 100) %>%
      gather(key, value, n) %>%
      unite(vuner_comb, vuner, key) %>%
      spread(vuner_comb, value) %>%
      transmute(vul_index   = (0 * I_n + 1.5 * II_n + 3 * III_n) / 100)
  }


# 04 fishing resource index calculator----------
index_calculator_fishing <-
  function(data) {
    data %>%
      gather(Species, abund, -Cod) %>%
      left_join(scores_fishing, by = 'Species') %>%
      group_by(Cod) %>%
      mutate(N = sum(abund)) %>%
      group_by(Cod, fish_resource) %>%
      mutate(prop_n = sum(abund) / N) %>%
      summarise(n = mean(prop_n, na.rm = T) * 100) %>%
      gather(key, value, n) %>%
      unite(fish_resource_comb, fish_resource, key) %>%
      spread(fish_resource_comb, value) %>%
      transmute(fishing_index   = (0 * I_n + 1 * II_n + 2 * III_n + 3 * IV_n) / 100) 
    # full_join(factores, by = 'Cod') %>%
    # mutate(cat_index = cut(
    #   fishing_index,
    #   breaks = quantile(.$fishing_index, probs = seq(0, 1, 0.25)),
    #   labels = c('Low', 'Medium', 'High', 'Very high'),
    #   include.lowest = T
    # ))
  }
