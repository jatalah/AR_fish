index_calculator <-
  function(data) {
    data %>%
      gather(Species, abund, -Cod) %>%
      left_join(scores, by = 'Species') %>%
      group_by(Cod) %>%
      mutate(N = sum(abund)) %>%
      group_by(Cod, local_imp) %>%
      mutate(prop_n = sum(abund) / N) %>%
      summarise(n = mean(prop_n, na.rm = T) * 100) %>%
      gather(key, value, n) %>%
      unite(local_imp_comb, local_imp, key) %>%
      spread(local_imp_comb, value) %>%
      mutate(imp_index   = (1.5 * I_n + 3 * II_n + 4.5 * III_n + 6 * IV_n) / 100) %>% 
      full_join(factores, by = 'Cod') %>%
      mutate(cat_index = cut(
        imp_index,
        breaks = quantile(.$imp_index, probs = seq(0, 1, 0.25)),
        labels = c('Low', 'Medium', 'High', 'Very high'),
        include.lowest = T
      ))
  }
