dat <- readRDS("output/4_voc_matrix.rds")
dat4 <- dat %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a))) %>%
  mutate(`Transmission` = if_else(Rt2 == 3.5, "Default transmission", "Higher VOC transmission"),
         `Vaccine efficacy` = if_else(efficacy_infection == 0.63, "Default efficacy", "Lower VOC efficacy"))

g4 <- ggplot(data = dat4, aes(x = `Age.target`, y = (deaths_averted_phase1 / 50e6 * 1e6), fill = `Age.target`)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(`Transmission` ~ `Vaccine efficacy`) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g4

ggsave("plots/fig4.png", plot = g4, height = 6, width = 10)


dat4b <- dat4 %>%
  unnest(cols = output) %>%
  filter(compartment == "deaths",
         Rt1 == 1.2) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(date >= as.Date(vaccine_start_date), date <= as.Date("2022-06-30")) %>%
  mutate(t = t - as.numeric(as.Date(vaccine_start_date) - as.Date(date_start)) + 1)

g4b <- ggplot() +
  geom_line(data = filter(dat4b, max_coverage == 0),
            aes(x = t, y = value / target_pop * 1e6, linetype = factor(max_coverage)), col = "black") +
  geom_line(data = filter(dat4b, date > as.Date("2021-01-01"), max_coverage != 0), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = factor(max_coverage))) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 122, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c(col1, col2, col3, col4)) +
  facet_grid(`Transmission` ~ `Vaccine efficacy`) +  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Time (days)", y = "Deaths per million per day", linetype = "Intervention", col = "Age coverage \ntarget (years)") +
  annotate(geom = "text", x = 0, label="\nvaccination start", y=80, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122, label="vaccination stop\n", y=80, colour="darkgrey", angle=90)

g4b
ggsave("plots/fig4b.png", plot = g4b, height = 6, width = 10)
