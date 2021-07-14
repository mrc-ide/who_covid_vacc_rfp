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
  pivot_wider(names_from = t, values_from = deaths_cumu) %>%
  mutate(deaths_vacc_phase1 = `852` - `365`,
         deaths_vacc_phase2 = `1217` - `852`,
         deaths_phase1 = deaths_phase1 - deaths_vacc_phase1,
         deaths_phase2 = deaths_phase2 - deaths_vacc_phase2,
         deaths_vacc_phase1 = deaths_vacc_phase1 * 0.5,
         deaths_vacc_phase2 = deaths_vacc_phase2 * 0.5 ) %>%
  select(-c(`365`, `852`, `1217`)) %>%
  pivot_longer(contains("deaths")) %>%
  mutate(phase = if_else(name %in% c("deaths_phase1", "deaths_vacc_phase1"), "phase 1", "phase 2")) %>%
  mutate(Period = factor(phase, levels = c("phase 2", "phase 1"), labels = c("Period 2 (2022-23)", "Period 1 (2021-22)"))) %>%
  mutate(outcome = if_else(name %in% c("deaths_vacc_phase1", "deaths_vacc_phase2"), "vaccinated", "unvaccinated")) %>%
  select(-name, -phase) %>%
  group_by(`Income group`, strategy, Age.target, Period) %>%
  summarise(deaths = sum(value)) %>%
  pivot_wider(names_from = strategy, values_from = deaths) %>%
  mutate(deaths_averted = None - Vaccine)


g2b <- ggplot(data = df, aes(x = `Age.target`, y = (deaths_averted / 50e6 * 1e6), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=(round(deaths_averted / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(~ `Income group`, nrow = 4, labeller = label_both) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g2b
ggsave("plots/fig2b_IFR50.png", plot = g2b, height = 8, width = 5)
