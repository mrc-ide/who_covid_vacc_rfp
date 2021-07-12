
dat <- readRDS("output/6_takeup_scenarios.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  filter(scaling_eff_dis == 0.73,
         max_coverage == 1) %>%
  mutate(Scenario = if_else(takeup_over_65 == 0.85 & takeup_under_65 == 0.7,
                            "Default",
                            if_else(takeup_over_65 == 0.95 & takeup_under_65 == 0.7,
                                    "Optimistic elderly",
                                    if_else(takeup_over_65 == 0.95 & takeup_under_65 == 0.5,
                                            "Optimistic elderly + pessimistic younger",
                                            if_else(takeup_over_65 == 0.7 & takeup_under_65 == 0.5, "Pessimistic elderly and younger", "0"))))) %>%
  filter(Scenario != "0") %>%
  mutate(strategy = if_else(max_coverage == 0, "None", "Vaccine")) %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a)))

# figure 6: bar plots
dat6 <- dat %>%
  filter(max_coverage != 0,
         Rt1 == 1.2) %>%
  pivot_longer(c("deaths_averted_phase1", "hospitalisations_averted_phase1", "infections_averted_phase1", "deaths_averted_phase2", "hospitalisations_averted_phase2", "infections_averted_phase2"), names_to = "Event", values_to = "value") %>%
  mutate(name = Event) %>%
  mutate(Event = if_else(Event %in% c("deaths_averted_phase1", "deaths_averted_phase2"), "Deaths", Event),
         Event = if_else(Event %in% c("hospitalisations_averted_phase1", "hospitalisations_averted_phase2"), "Hospitalisations", Event),
         Event = if_else(Event %in% c("infections_averted_phase1", "infections_averted_phase2"), "Infections", Event)) %>%
  mutate(Period = if_else(name %in% c("infections_averted_phase1", "deaths_averted_phase1", "hospitalisations_averted_phase1"), "Period 1 (2021-22)", "Period 2 (2022-23)")) %>%
  mutate(Period = factor(Period, levels = c("Period 2 (2022-23)", "Period 1 (2021-22)"), labels = c("Period 2 (2022-23)", "Period 1 (2021-22)")))

dat6_permill <- dat6 %>%
  group_by(Event, Age.target, income_group, Scenario) %>%
  mutate(yax = sum(value)) %>%
  group_by(Event, Scenario) %>%
  mutate(yax_max = max(yax))

dat6_perFVP <- dat6 %>%
  group_by(Event, Age.target, income_group, Scenario) %>%
  mutate(yax = sum(value)/vaccine_n_phase1*100) %>%
  group_by(Event, Scenario) %>%
  mutate(yax_max = max(yax))

g6_plotfunc <- function(data, event){
  ggplot(data = filter(data, Event == event), aes(x = `Age.target`, y = (value / 50e6 * 1e6), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_grid(`Income group` ~ Scenario) +
  labs(x = "Age coverage target (years)", y = paste0(event, " averted per million total population"), fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())
}
g6_deaths <- g6_plotfunc(dat6_permill, "Deaths")
g6_deaths
ggsave("plots/fig6_deaths.png", plot = g6_deaths, height = 8, width = 10)

g6_hospitalisations <- g6_plotfunc(dat6_permill, "Hospitalisations")
ggsave("plots/fig6_hospitalisations.png", plot = g6_hospitalisations, height = 8, width = 10)

g6_infections <- g6_plotfunc(dat6_permill, "Infections")
ggsave("plots/fig6_infections.png", plot = g6_infections, height = 8, width = 10)

