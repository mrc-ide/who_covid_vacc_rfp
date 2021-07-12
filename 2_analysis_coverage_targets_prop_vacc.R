# Author: AB Hogan
# 2 June 2021
# Runs of individual trajectories

### Load packages ##############################################################
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)

source("R/plotting_specifics.R")

j <- as.numeric(as.Date(vaccine_start_date)) - as.numeric(as.Date(date_start))
k <- as.numeric(as.Date("2022-06-30")) - as.numeric(as.Date(date_start)) + 1
l <- as.numeric(as.Date("2023-06-30")) - as.numeric(as.Date(date_start)) + 1

################################################################################

dat <- readRDS("output/2_coverage_targets.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  mutate(strategy = if_else(max_coverage == 0, "None", "Vaccine")) %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a))) %>%
  filter(hs_constraints == "Present",
         Rt1 == 1.2)

x <- dat %>%
  select(target_group_stop, `Income group`, strategy, deaths_phase1, deaths_phase2, output_new, `Age.target`) %>%
  mutate_if(is.list, map, as_tibble) %>% 
  unnest(output_new) %>%
  filter(t %in% c(j,k,l))

x$deaths_cumu <- round(rowSums(x[,7:40]))

df <- x %>%
  select(`Income group`, strategy, deaths_phase1, deaths_phase2, t, deaths_cumu, `Age.target`) %>%
  mutate(Age.target = if_else(strategy == "None", "None", as.character(`Age.target`))) %>%
  unique() %>%
  pivot_wider(names_from = t, values_from = deaths_cumu) %>%
  mutate(deaths_vacc_phase1 = `852` - `365`,
         deaths_vacc_phase2 = `1217` - `852`,
         deaths_phase1 = deaths_phase1 - deaths_vacc_phase1,
         deaths_phase2 = deaths_phase2 - deaths_vacc_phase2) %>%
  select(-c(`365`, `852`, `1217`)) %>%
  pivot_longer(contains("deaths")) %>%
  filter(!((name == "deaths_vacc_phase1" | name == "deaths_vacc_phase2") & strategy == "None")) %>%
  mutate(phase = if_else(name %in% c("deaths_phase1", "deaths_vacc_phase1"), "phase 1", "phase 2")) %>%
  mutate(outcome = if_else(name %in% c("deaths_vacc_phase1", "deaths_vacc_phase2"), "vaccinated deaths", "unvaccinated deaths")) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = c("None", "50+", "20+", "12+", "0+"))) %>%
  mutate(Scenario = if_else(strategy == "Vaccine", paste0("Target ", Age.target, " ", outcome), "No vaccine")) %>%
  select(`Income group`, Scenario, phase, value, Age.target) %>%
  mutate(Period = factor(phase, levels = c("phase 2", "phase 1"), labels = c("Period 2 (2022-23)", "Period 1 (2021-22)")))



gg <- ggplot(data = df, aes(x = Scenario, y = value/50e6*1e6, fill = Age.target, alpha = Period)) +
  geom_col(position = "stack") +
  geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5), size = 2.5) +
  facet_wrap(~ `Income group`, labeller = label_both, ncol = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  scale_fill_manual(values = c("grey", col1, col2, col3, col4)) +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  labs(x = "Strategy", y = "Deaths per million")

gg

ggsave("plots/example_fig_deaths_vaccinated_2.png", plot = gg, height = 6, width = 12)


gg <- ggplot(data = df, aes(x = strategy, y = value/50e6*1e6, fill = outcome, alpha = phase)) +
  geom_col(position = "dodge") +
  facet_grid(`Income group` ~ Age.target, labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  scale_alpha_discrete("phase", range = c(1, 0.25)) +
  labs(x = "Strategy", y = "Deaths per million")

gg

ggsave("plots/example_fig_deaths_vaccinated.png", plot = gg, height = 6, width = 10)


