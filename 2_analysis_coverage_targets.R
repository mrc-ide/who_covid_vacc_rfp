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

dat <- readRDS("output/2_coverage_targets.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  mutate(strategy = if_else(max_coverage == 0, "None", "Vaccine")) %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a)))

dat2 <- dat %>%
  select(-c(output_cf, output_age, output_age_cf, output_new, output_new_cf)) %>%
  unnest(cols = output) %>%
    filter(compartment %in% c("deaths", "infections"),
           Rt1 == 1.2,
           hs_constraints == "Present") %>%
    mutate(date = as.Date(date_start) + t)

write_csv(filter(dat2,max_coverage == 1), "datx.csv")    

# figure 1a: trajectories deaths
dat2 <- dat2 %>%
  filter(date >= as.Date(vaccine_start_date), date <= as.Date("2023-06-30")) %>%
  mutate(t = t - as.numeric(as.Date(vaccine_start_date) - as.Date(date_start)) + 1)

g2 <- ggplot() +
  geom_line(data = filter(dat2, strategy == "None", compartment == "deaths"),
            aes(x = t, y = value / target_pop * 1e6, linetype = strategy), col = "black") +
  geom_line(data = filter(dat2, date > as.Date("2021-01-01"), compartment == "deaths", strategy != "None"), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = strategy)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 122, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_vline(xintercept = 122+365, linetype = "dotted") +
  geom_vline(xintercept = 122+365*2, linetype = "dotted") +
  scale_color_manual(values = c(col1, col2, col3, col4)) +
  facet_wrap( ~ `Income group`, labeller = label_both, ncol = 1) +
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

g2

ggsave("plots/fig2_deaths.png", plot = g2, height = 8, width = 7)

# trajectories infections
g2_inf <- ggplot() +
  geom_line(data = filter(dat2, strategy == "None", compartment == "infections"),
            aes(x = t, y = value / target_pop * 1e6, linetype = strategy), col = "black") +
  geom_line(data = filter(dat2, date > as.Date("2021-01-01"), compartment == "infections", strategy != "None"), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = strategy)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 122, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_vline(xintercept = 122+365, linetype = "dotted") +
  geom_vline(xintercept = 122+365*2, linetype = "dotted") +
  scale_color_manual(values = c(col1, col2, col3, col4)) +
  facet_wrap( ~ `Income group`, labeller = label_both, ncol = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Time (days)", y = "Infections per million per day", linetype = "Intervention", col = "Age coverage \ntarget (years)") +
  annotate(geom = "text", x = 0, label="\nvaccination start", y=15000, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122, label="vaccination stop\n", y=15000, colour="darkgrey", angle=90)+
  annotate(geom = "text", x = 122+365, label="period 1 end\n", y=15000, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122+365*2, label="period 2 end\n", y=15000, colour="darkgrey", angle=90)

g2_inf

ggsave("plots/fig2_infections.png", plot = g2_inf, height = 8, width = 7)


# figure 2b: bar plots
dat2b <- dat %>%
  filter(max_coverage != 0,
         Rt1 == 1.2,
         hs_constraints == "Present") %>%
  pivot_longer(c("deaths_averted_phase1", "hospitalisations_averted_phase1", "infections_averted_phase1", "deaths_averted_phase2", "hospitalisations_averted_phase2", "infections_averted_phase2"), names_to = "Event", values_to = "value") %>%
  mutate(name = Event) %>%
  mutate(Event = if_else(Event %in% c("deaths_averted_phase1", "deaths_averted_phase2"), "Deaths", Event),
         Event = if_else(Event %in% c("hospitalisations_averted_phase1", "hospitalisations_averted_phase2"), "Hospitalisations", Event),
         Event = if_else(Event %in% c("infections_averted_phase1", "infections_averted_phase2"), "Infections", Event)) %>%
  mutate(Period = if_else(name %in% c("infections_averted_phase1", "deaths_averted_phase1", "hospitalisations_averted_phase1"), "Period 1 (2021-22)", "Period 2 (2022-23)")) %>%
  mutate(Period = factor(Period, levels = c("Period 2 (2022-23)", "Period 1 (2021-22)"), labels = c("Period 2 (2022-23)", "Period 1 (2021-22)")))

dat2b_permill <- dat2b %>%
  group_by(Event, Age.target, income_group) %>%
  mutate(yax = sum(value)) %>%
  group_by(Event) %>%
  mutate(yax_max = max(yax))

dat2b_perFVP <- dat2b %>%
group_by(Event, Age.target, income_group) %>%
  mutate(yax = sum(value)/vaccine_n_phase1*100) %>%
  group_by(Event) %>%
  mutate(yax_max = max(yax))

g2b <- ggplot(data = filter(dat2b_permill, Event == "Deaths"), aes(x = `Age.target`, y = (value / 50e6 * 1e6), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(~ `Income group`, nrow = 4, labeller = label_both) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g2b
ggsave("plots/fig2b.png", plot = g2b, height = 8, width = 5)

g2c <- ggplot(data = filter(dat2b_permill), aes(x = `Age.target`, y = (value / target_pop * 1e6), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5), size = 2.5) +
  facet_wrap(`Income group` ~ Event, nrow = 4, labeller = label_both, scales = "free") +
  labs(x = "Age coverage target (years)", y = "Events averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  geom_blank(aes(y = yax_max / target_pop * 1e6)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g2c
ggsave("plots/fig2c.png", plot = g2c, height = 8, width = 10)

# events averted by age group

dat2d <- dat %>%
  select(output_age, `Income group`, target_pop, date_start, R0, Rt1, Rt2, Rt1_start, Rt2_start, Rt2_end, vaccine_start_date, target_group_stop, vacc_children, strategy_switch, efficacy_infection, scaling_eff_dis, rel_infectiousness_vaccinated, strategy, Age.target, hs_constraints) %>%
  unnest(cols = output_age) %>%
  filter(compartment %in% c("deaths", "infections", "hospitalisations"),
         Rt1 == 1.2) %>%
  pivot_wider(names_from = strategy, values_from = value) %>%
  left_join(age_group_key_2) %>%
  mutate(events_averted = None - Vaccine) %>%
  group_by(compartment, period, `Income group`, target_pop, date_start, R0, Rt1, Rt2, Rt1_start, Rt2_start, Rt2_end, vaccine_start_date, target_group_stop, vacc_children, strategy_switch, efficacy_infection, scaling_eff_dis, rel_infectiousness_vaccinated, age_group_10y, Age.target, hs_constraints) %>%
  summarise(None = sum(None),
            Vaccine = sum(Vaccine),
            events_averted = sum(events_averted)) %>%
  mutate(Period = factor(period, levels = c("phase2", "phase1"), labels = c("Period 2 (2022-23)", "Period 1 (2021-22)")))

###########################
# plot deaths with and without vaccine, in age groups they occur
dat2e <- dat2d %>%
  pivot_longer(c(None, Vaccine)) %>%
  filter((Age.target != "0+" & period == "phase1" & name == "Vaccine") | (Age.target == "0+" & period == "phase2" & name == "Vaccine"& name == "Vaccine") | (Age.target == "50+" & period == "phase1" & name == "None"))

event <- "deaths"
gs <- ggplot(data = filter(dat2e, compartment == event),
       aes(x = factor(age_group_10y), y = value / target_pop * 1e6, fill = `Age.target`, alpha = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(hs_constraints~ `Income group`, labeller = label_both) +
  scale_alpha_discrete("Scenario", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Age group in which events averted", y = paste0(event, " per million total population"), fill = "Age coverage \ntarget (years)")

ggsave("plots/fig2g_deaths.png", plot = gs, height = 7, width = 13)


event <- "infections"
gs <- ggplot(data = filter(dat2e, compartment == event, hs_constraints == "Present"),
             aes(x = factor(age_group_10y), y = value / target_pop * 1e6, fill = `Age.target`, alpha = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~ `Income group`, labeller = label_both) +
  scale_alpha_discrete("Scenario", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Age group in which events averted", y = paste0(event, " per million total population"), fill = "Age coverage \ntarget (years)")

gs
ggsave("plots/fig2g_infections.png", plot = gs, height = 4, width = 13)

###########################
# tables of raw values of plots above
table_out <- dat2d %>%
  filter(hs_constraints == "Present",
         target_group_stop == 1) %>%
  ungroup() %>%
  mutate(None = round(None / target_pop * 1e6),
         Vaccine = round(Vaccine / target_pop * 1e6)) %>%
  select(compartment, Period, `Income group`, age_group_10y, Age.target, None, Vaccine) 

table_out_p1_d <- table_out %>%
  filter(compartment == "deaths", Period == "Period 1 (2021-22)") %>%
  select(-Age.target, -Period, -compartment)
table_out_p1_h <- table_out %>%
  filter(compartment == "hospitalisations", Period == "Period 1 (2021-22)") %>%
  select(None, Vaccine)
table_out_p1_i <- table_out %>%
  filter(compartment == "infections", Period == "Period 1 (2021-22)") %>%
  select(None, Vaccine)
table_out_p2_d <- table_out %>%
  filter(compartment == "deaths", Period == "Period 2 (2022-23)") %>%
  select(None, Vaccine)
table_out_p2_h <- table_out %>%
  filter(compartment == "hospitalisations", Period == "Period 2 (2022-23)") %>%
  select(None, Vaccine)
table_out_p2_i <- table_out %>%
  filter(compartment == "infections", Period == "Period 2 (2022-23)") %>%
  select(None, Vaccine)

table_summary <- cbind(table_out_p1_d, table_out_p1_h, table_out_p1_i, table_out_p2_d, table_out_p2_h, table_out_p2_i)
colnames(table_summary)[3:14] <- c("None_P1_Deaths", "Vaccine_P1_Deaths",
                                   "None_P1_Hospitalisations", "Vaccine_P1_Hospitalisations",
                                   "None_P1_Infections", "Vaccine_P1_Infections",
                                   "None_P2_Deaths", "Vaccine_P2_Deaths",
                                   "None_P2_Hospitalisations", "Vaccine_P2_Hospitalisations",
                                   "None_P2_Infections", "Vaccine_P2_Infections")
write_csv(table_summary, "output/table_summary.csv")

###########################

plot_events_avert_age <- function(event){
  plot_out <- ggplot(data = filter(dat2d, compartment == event, hs_constraints == "Present"), aes(x = factor(age_group_10y), y = events_averted / target_pop * 1e6, alpha = period, fill = `Age.target`)) +
    geom_bar(stat = "identity") +
    facet_grid(`Income group` ~ `Age.target`, labeller = label_both) +
    scale_fill_manual(values = c(col1, col2, col3, col4)) +
    scale_alpha_discrete("Period", range = c(0.25, 1)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA, color = "white"),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.text.x=element_text(angle=60, hjust = 1)
    ) +
    labs(x = "Age group in which events averted", y = paste0(event, " averted per million total population"), fill = "Age coverage \ntarget (years)")
  return(plot_out)
}

g2d90 <- plot_events_avert_age("deaths")
ggsave("plots/fig2d_deaths.png", plot = g2d90, height = 8, width = 10)
g2e90 <- plot_events_avert_age("infections")
ggsave("plots/fig2e_infections.png", plot = g2e90, height = 8, width = 10)
g2f90 <- plot_events_avert_age("hospitalisations")
ggsave("plots/fig2f_hospitalisations.png", plot = g2f90, height = 8, width = 10)

######################################################################
# per FVP plots

g2b_FVP <- ggplot(data = filter(dat2b_perFVP, Event == "Deaths"), aes(x = `Age.target`, y = (value / vaccine_n_phase1 * 100), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=(round(value / vaccine_n_phase1 * 100,1))), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(~ `Income group`, nrow = 4, labeller = label_both) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per 100 FVP", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g2b_FVP
ggsave("plots/fig2b_FVP.png", plot = g2b_FVP, height = 8, width = 5)

g2c_FVP <- ggplot(data = dat2b_perFVP, aes(x = `Age.target`, y = (value / vaccine_n_phase1 * 100), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label=(round(value / vaccine_n_phase1 * 100,1))), stat = "identity", position = position_stack(vjust = 0.5), size = 2.5) +
  facet_wrap(`Income group` ~ Event, nrow = 4, labeller = label_both, scales = "free") +
  labs(x = "Age coverage target (years)", y = "Events averted per 100 FVP", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  geom_blank(aes(y = yax_max)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g2c_FVP
ggsave("plots/fig2c_FVP.png", plot = g2c_FVP, height = 8, width = 10)
