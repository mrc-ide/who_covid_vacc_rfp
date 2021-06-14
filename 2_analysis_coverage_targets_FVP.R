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

# figure 2b: bar plots
dat2b <- dat %>%
  filter(max_coverage != 0,
         Rt1 == 1.2) %>%
  pivot_longer(c("deaths_averted_phase1", "hospitalisations_averted_phase1", "infections_averted_phase1", "deaths_averted_phase2", "hospitalisations_averted_phase2", "infections_averted_phase2"), names_to = "Event", values_to = "value") %>%
  mutate(name = Event) %>%
  mutate(Event = if_else(Event %in% c("deaths_averted_phase1", "deaths_averted_phase2"), "Deaths", Event),
         Event = if_else(Event %in% c("hospitalisations_averted_phase1", "hospitalisations_averted_phase2"), "Hospitalisations", Event),
         Event = if_else(Event %in% c("infections_averted_phase1", "infections_averted_phase2"), "Infections", Event)) %>%
  mutate(Period = if_else(name %in% c("infections_averted_phase1", "deaths_averted_phase1", "hospitalisations_averted_phase1"), "Period 1 (2021-22)", "Period 2 (2022-23)")) %>%
  mutate(Period = factor(Period, levels = c("Period 2 (2022-23)", "Period 1 (2021-22)"), labels = c("Period 2 (2022-23)", "Period 1 (2021-22)"))) %>%
  group_by(Event, Age.target, income_group) %>%
  mutate(yax = sum(value)/vaccine_n_phase1*100) %>%
  group_by(Event) %>%
  mutate(yax_max = max(yax))

g2b <- ggplot(data = filter(dat2b, Event == "Deaths"), aes(x = `Age.target`, y = (value / vaccine_n_phase1 * 100), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ `Income group`, nrow = 4, labeller = label_both) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per 100 FVP", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g2b
ggsave("plots/fig2b_FVP.png", plot = g2b, height = 8, width = 6)

g2c <- ggplot(data = dat2b , aes(x = `Age.target`, y = (value / vaccine_n_phase1 * 100), alpha = Period, fill = `Age.target`)) +
  geom_bar(stat = "identity") +
  facet_wrap(`Income group` ~ Event, nrow = 4, labeller = label_both, scales = "free") +
  labs(x = "Age coverage target (years)", y = "Events averted per 100 FVP", fill = "Age coverage \ntarget (years)") +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  geom_blank(aes(y = yax_max)) +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        #axis.text.x=element_text(angle=60, hjust = 1)
  )

g2c
ggsave("plots/fig2c_FVP.png", plot = g2c, height = 8, width = 10)

