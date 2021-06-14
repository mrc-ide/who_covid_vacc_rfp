dat <- readRDS("output/3_timing_vaccination.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC")))

pd3 <- dat %>%
  select(R0, Rt1, Rt2, max_coverage, `Income group`, output, date_start, target_pop, vaccine_start_date, target_group_stop) %>%
  unnest(cols = output) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(compartment == "deaths",
         date >= min(vaccine_start_date),
         date <= "2022-06-30")


pd3a <- dat %>%
  select(R0, Rt1, Rt2, max_coverage, `Income group`, output, date_start, target_pop, vaccine_start_date, target_group_stop) %>%
  unnest(cols = output) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(compartment == "deaths",
         date >= min(vaccine_start_date),
         date <= "2022-06-30",
         max_coverage != 0,
         target_group_stop == 7)

pd3b <- dat %>%
  select(R0, Rt1, Rt2, max_coverage, `Income group`, date_start, target_pop, vaccine_start_date, target_group_stop, deaths_averted_phase1) %>%
  filter(max_coverage != 0) %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a)))

g3 <- ggplot(data = pd3a, aes(x = t, y = value / target_pop * 1e6, col = factor(vaccine_start_date))) +
  geom_line() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_color_viridis_d() +
  labs(x = "Time (days)", y = "Deaths per million per day", col = "Vaccination start date")
g3

ggsave("plots/fig_timing.png", plot = g3, height = 5, width = 10)



scalefact <- 2000
g3b <- ggplot() +
  geom_bar(data = pd3b, aes(x = as.Date(vaccine_start_date), y = deaths_averted_phase1, fill  = `Age.target`), stat = "identity") +
  geom_line(data = filter(pd3, max_coverage == 0, vaccine_start_date == as.Date("2021-03-01")),
            aes(x = date, y = value / target_pop * 1e6 * scalefact)) +
  scale_y_continuous("Deaths averted per million total population", 
    sec.axis = sec_axis(~ . / scalefact, name = "Deaths per million per day (counterfactual)")) +
  facet_wrap( ~`Age.target`, ncol = 1) +
scale_fill_manual(values = c(col1, col2, col3, col4)) +
  scale_x_continuous(labels = seq(0,486,100), breaks = seq(18687,(18687+487-1),100)) +
  labs(x = "Time (days)", fill = "Age coverage \ntarget (years)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())
  
g3b

ggsave("plots/fig3_timing.png", plot = g3b, height = 8, width = 6)

