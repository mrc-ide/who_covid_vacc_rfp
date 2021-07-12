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

################################################################################

dat <- readRDS("output/5_coverage_targets_sensitivity.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  mutate(strategy = if_else(max_coverage == 0, "None", "Vaccine")) %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a)))

dat_sub1 <- dat %>%
  filter(rel_infect_u10 == 1,
         efficacy_infection == 0.63,
         scaling_eff_dis == 0.73,
         rel_infectiousness_vaccinated == 0.55,
         hs_constraints == "Present") %>%
  mutate(Scenario = "Default")

dat_sub2 <- dat %>%
  filter(rel_infect_u10 == 1,
         efficacy_infection == 0.63,
         scaling_eff_dis == 0.73,
         rel_infectiousness_vaccinated == 0.55,
         hs_constraints == "Absent") %>%
  mutate(Scenario = "HS_Unconstrained")

dat_sub3 <- dat %>%
  filter(rel_infect_u10 == 0.5,
         efficacy_infection == 0.63,
         scaling_eff_dis == 0.73,
         rel_infectiousness_vaccinated == 0.55,
         hs_constraints == "Present") %>%
  mutate(Scenario = "Reduced_Inf_U10")

dat_sub4 <- dat %>%
  filter(rel_infect_u10 == 1,
         efficacy_infection == 0,
         scaling_eff_dis == 0.9,
         rel_infectiousness_vaccinated == 0.55,
         hs_constraints == "Present") %>%
  mutate(Scenario = "Disease_Blocking_Only")

dat <- rbind(dat_sub1, dat_sub2, dat_sub3, dat_sub4)

# figure 5a: trajectories
dat5 <- dat %>%
  unnest(cols = output) %>%
  filter(compartment == "deaths",
         Rt1 == 1.2,
         income_group == "LMIC") %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(date >= as.Date(vaccine_start_date), date <= as.Date("2023-06-30")) %>%
  mutate(t = t - as.numeric(as.Date(vaccine_start_date) - as.Date(date_start)) + 1)

g5 <- ggplot() +
  geom_line(data = filter(dat5, strategy == "None"),
            aes(x = t, y = value / target_pop * 1e6, linetype = strategy), col = "black") +
  geom_line(data = filter(dat5, date > as.Date("2021-01-01"), strategy != "None"), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = strategy)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 122, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_vline(xintercept = 122+365, linetype = "dotted") +
  geom_vline(xintercept = 122+365*2, linetype = "dotted") +
  scale_color_manual(values = c(col1, col2, col3, col4)) +
  facet_wrap( ~ Scenario, labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
        ) +
  labs(x = "Time (days)", y = "Deaths per million per day", linetype = "Intervention", col = "Age coverage \ntarget (years)") +
 annotate(geom = "text", x = 0, label="\nvaccination start", y=80, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122, label="vaccination stop\n", y=80, colour="darkgrey", angle=90)+
  annotate(geom = "text", x = 122+365, label="period 1 end\n", y=80, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122+365*2, label="period 2 end\n", y=80, colour="darkgrey", angle=90)


g5

ggsave("plots/fig5_trajectories.png", plot = g5, height = 6, width = 10)

# figure 2b: bar plots
dat5b <- dat %>%
  filter(max_coverage != 0,
         Rt1 == 1.2) %>%
  pivot_longer(c("deaths_averted_phase1", "hospitalisations_averted_phase1", "infections_averted_phase1", "deaths_averted_phase2", "hospitalisations_averted_phase2", "infections_averted_phase2"), names_to = "Event", values_to = "value") %>%
  mutate(name = Event) %>%
  mutate(Event = if_else(Event %in% c("deaths_averted_phase1", "deaths_averted_phase2"), "Deaths", Event),
         Event = if_else(Event %in% c("hospitalisations_averted_phase1", "hospitalisations_averted_phase2"), "Hospitalisations", Event),
         Event = if_else(Event %in% c("infections_averted_phase1", "infections_averted_phase2"), "Infections", Event)) %>%
  mutate(Period = if_else(name %in% c("infections_averted_phase1", "deaths_averted_phase1", "hospitalisations_averted_phase1"), "Period 1 (2021-22)", "Period 2 (2022-23)")) %>%
  mutate(Period = factor(Period, levels = c("Period 2 (2022-23)", "Period 1 (2021-22)"), labels = c("Period 2 (2022-23)", "Period 1 (2021-22)")))


g5b <- ggplot(data = filter(dat5b, Event == "Deaths", income_group == "LMIC"), aes(x = `Age.target`, y = (value / 50e6 * 1e6), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(~ Scenario, labeller = label_both) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g5b
ggsave("plots/fig5b_text.png", plot = g5b, height = 6, width = 7)

# FVP barplot
g5b_FVP <- ggplot(data = filter(dat5b, Event == "Deaths", income_group == "LMIC"), aes(x = `Age.target`, y = value / vaccine_n_phase1 * 100, alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(~ Scenario, labeller = label_both) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per 100 FVP", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col2b, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g5b_FVP
ggsave("plots/fig5b_FVP_text.png", plot = g5b_FVP, height = 6, width = 7)

##################################
# results across all income settings
plot_func_sensitivity <- function(x){
  df <- x %>%
    group_by(Event, Age.target, income_group, Scenario) %>%
    mutate(yax = sum(value)) %>%
    group_by(Event) %>%
    mutate(yax_max = max(yax))
  
  g5c <- ggplot(data = df , aes(x = Age.target, y = (value / target_pop * 1e6), alpha = Period, fill = Age.target)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5)) +
    facet_wrap(Event ~ Scenario, nrow = 3, labeller = label_both, scales = "free") +
    labs(x = "Age coverage target (years)", y = "Events averted per million total population", fill = "Age coverage \ntarget (years)") +
    scale_alpha_discrete("Period", range = c(0.25, 1)) +
    geom_blank(aes(y = yax_max / target_pop * 1e6)) +
    scale_fill_manual(values = c(col1, col2, col3, col4)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA, color = "white"),
          panel.border = element_blank(),
          axis.line = element_line(),
          #axis.text.x=element_text(angle=60, hjust = 1)
    )
  return(g5c)
}

icg_list <- c("HIC", "UMIC", "LMIC", "LIC")
for (i in 1:4){
  icg <- icg_list[i]
  
  g5c_disease_blocking <- plot_func_sensitivity(filter(dat5b, Scenario %in% c("Default", "Disease_Blocking_Only"), income_group == icg))
  
  ggsave(paste0("plots/fig5c_disease_blocking_", icg, "_text.png"), plot = g5c_disease_blocking, height = 8, width = 8)
  
  g5c_hs_unconst <- plot_func_sensitivity(filter(dat5b, Scenario %in% c("Default", "HS_Unconstrained"), income_group == icg))
  
  ggsave(paste0("plots/fig5c_hs_unconstrained_", icg, "_text.png"), plot = g5c_hs_unconst, height = 8, width = 8)
  
  g5c_Reduced_Inf_U10 <- plot_func_sensitivity(filter(dat5b, Scenario %in% c("Default", "Reduced_Inf_U10"), income_group == icg))
  
  ggsave(paste0("plots/fig5c_Reduced_Inf_U10_", icg, "_text.png"), plot = g5c_Reduced_Inf_U10, height = 8, width = 8)
}

