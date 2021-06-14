# Author: AB Hogan
# 2 June 2021
# Runs of individual trajectories

### Load packages ##############################################################
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)

################################################################################
# Plotting specifics
# Age bands
x1 <- seq(0,80,5)
y1 <- seq(4,84,5)
y2 <- seq(5, 85, 5)
z1 <- paste0(x1,"-",y1)
z2 <- paste0(x1, "-", y2)
z1[17] <- "80+"
z2[17] <- "80+"
a <- paste0(seq(0, 80, 5), "+")
age_group_key <- data.frame(target_group_stop = 1:17, `Age target` = a)

age_group_10y <- c("0-10", "0-10", "10-20", "10-20","20-30", "20-30", "30-40", "30-40","40-50","40-50", "50-60","50-60", "60-70", "60-70", "70-80", "70-80", "80+")

age_group_key_2 <- data.frame(age_group = z2, age_group_10y = age_group_10y)

col1 <- "#6dc1db"
col2 <- "#d8b9a9"
col3 <- "#e5546c"
col4 <- "#c00000"

################################################################################


dat <- readRDS("output/2_coverage_targets.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  mutate(strategy = if_else(max_coverage == 0, "None", "Vaccine")) %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a)))

# figure 1a: trajectories
dat2 <- dat %>%
  unnest(cols = output) %>%
  filter(compartment == "deaths",
         Rt1 == 1.2) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(date >= as.Date(vaccine_start_date), date <= as.Date("2022-06-30")) %>%
  mutate(t = t - as.numeric(as.Date(vaccine_start_date) - as.Date(date_start)) + 1)

g2 <- ggplot() +
  geom_line(data = filter(dat2, strategy == "None"),
            aes(x = t, y = value / target_pop * 1e6, linetype = strategy), col = "black") +
  geom_line(data = filter(dat2, date > as.Date("2021-01-01"), strategy != "None"), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = strategy)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 122, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid")) +
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
  annotate(geom = "text", x= 122, label="vaccination stop\n", y=80, colour="darkgrey", angle=90)

g2

ggsave("plots/fig2.png", plot = g2, height = 8, width = 6)

# figure 2b: bar plots
dat2b <- dat %>%
  filter(max_coverage != 0,
         Rt1 == 1.2) %>%
  pivot_longer(c("deaths_averted_phase1", "hospitalisations_averted_phase1", "infections_averted_phase1"), names_to = "Event", values_to = "value") %>%
  mutate(Event = if_else(Event == "deaths_averted_phase1", "Deaths", Event),
         Event = if_else(Event == "hospitalisations_averted_phase1", "Hospitalisations", Event),
         Event = if_else(Event == "infections_averted_phase1", "Infections", Event)) %>%
  group_by(Event) %>%
  mutate(yax_max = max(value))


g2b <- ggplot(data = filter(dat2b, Event == "Deaths"), aes(x = `Age.target`, y = (value / 50e6 * 1e6), fill = `Age.target`)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~ `Income group`, nrow = 4, labeller = label_both) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g2b
ggsave("plots/fig2b.png", plot = g2b, height = 8, width = 6)

g2c <- ggplot(data = dat2b , aes(x = `Age.target`, y = (value / target_pop * 1e6), fill = `Age.target`)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(`Income group` ~ Event, nrow = 4, labeller = label_both, scales = "free") +
  labs(x = "Age coverage target (years)", y = "Events averted per million total population", fill = "Age coverage \ntarget (years)") +
  geom_blank(aes(y = yax_max / target_pop * 1e6)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        #axis.text.x=element_text(angle=60, hjust = 1)
  )

g2c
ggsave("plots/fig2c.png", plot = g2c, height = 8, width = 10)

# deaths averted by age group
dat2d <- dat %>%
  select(output_age, `Income group`, target_pop, date_start, R0, Rt1, Rt2, Rt1_start, Rt2_start, Rt2_end, vaccine_start_date, target_group_stop, vacc_children, strategy_switch, efficacy_infection, scaling_eff_dis, rel_infectiousness_vaccinated, strategy, Age.target) %>%
  unnest(cols = output_age) %>%
  filter(compartment %in% c("deaths", "infections"),
         Rt1 == 1.2) %>%
  pivot_wider(names_from = strategy, values_from = value) %>%
  left_join(age_group_key_2) %>%
  mutate(events_averted = None - Vaccine) %>%
  group_by(compartment, period, `Income group`, target_pop, date_start, R0, Rt1, Rt2, Rt1_start, Rt2_start, Rt2_end, vaccine_start_date, target_group_stop, vacc_children, strategy_switch, efficacy_infection, scaling_eff_dis, rel_infectiousness_vaccinated, age_group_10y, Age.target) %>%
  summarise(None = sum(None),
            Vaccine = sum(Vaccine),
            events_averted = sum(events_averted))

g2d <- ggplot(data = filter(dat2d, compartment == "deaths"), aes(x = age_group_10y, y = events_averted / target_pop * 1e6, fill = `Age.target`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Income group`, ncol = 1, labeller = label_both) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Age group", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)")

g2d
ggsave("plots/fig2d.png", plot = g2d, height = 8, width = 6)

# infections averted by age group
g2e <- ggplot(data = filter(dat2d, compartment == "infections"), aes(x = age_group_10y, y = events_averted / target_pop * 1e6, fill = `Age.target`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Income group`, ncol = 1, labeller = label_both) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Age group", y = "Infections averted per million total population", fill = "Age coverage \ntarget (years)")

g2e
ggsave("plots/fig2e.png", plot = g2e, height = 8, width = 6)
