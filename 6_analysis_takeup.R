
dat <- readRDS("output/6_takeup.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  filter(scaling_eff_dis == 0.73,
         max_coverage == 1)

df <- dat %>%
  mutate(deaths_averted_phase1 = deaths_averted_phase1/vaccine_n_phase1) %>%
  select(target_group_stop, deaths_averted_phase1, takeup_under_65, takeup_over_65)# %>%
  #pivot_wider(names_from = target_group_stop, values_from = deaths_averted_phase1) %>%
  #mutate(relative_increase = `11`)

ggplot(data = df, aes(x = takeup_over_65, y = takeup_under_65, fill = deaths_averted_phase1)) +
geom_tile() +
  facet_grid(~target_group_stop) +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())
