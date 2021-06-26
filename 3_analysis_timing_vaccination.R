
### Load packages ##############################################################
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)

source("R/plotting_specifics.R")

################################################################################

dat <- readRDS("output/3_timing_vaccination.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC")))

pd3 <- dat %>%
  select(R0, Rt1, Rt2, max_coverage, `Income group`, output, date_start, target_pop, vaccine_start_date, target_group_stop, duration_R) %>%
  filter(max_coverage != 0) %>%
  unnest(cols = output) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(compartment == "deaths",
         date >= min(vaccine_start_date),
         date <= "2022-06-30")

pd3a <- dat %>%
  select(R0, Rt1, Rt2, max_coverage, `Income group`, date_start, target_pop, vaccine_start_date, target_group_stop, duration_R, deaths_averted_phase1) %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a)))

pd3b <- pd3 %>%
  filter(date <= "2022-06-30",
        target_group_stop == 7)

scalefact <- 2000
g3a <- ggplot() +
  geom_bar(data = filter(pd3a, duration_R == 365), aes(x = as.Date(vaccine_start_date), y = deaths_averted_phase1, fill  = `Age.target`), stat = "identity") +
  geom_line(data = filter(pd3, max_coverage == 0, vaccine_start_date == as.Date("2021-03-01"), duration_R == 365),
            aes(x = date, y = value / target_pop * 1e6 * scalefact)) +
  scale_y_continuous("Deaths averted per million total population", 
    sec.axis = sec_axis(~ . / scalefact, name = "Deaths per million per day (counterfactual)")) +
  facet_wrap( ~`Age.target`, ncol = 1) +
scale_fill_manual(values = c(col1, col2, col2b, col3, col4)) +
  scale_x_continuous(labels = seq(0,486,100), breaks = seq(18687,(18687+487-1),100)) +
  labs(x = "Time (days)", fill = "Age coverage \ntarget (years)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())
  
g3a

ggsave("plots/fig3a_timing_VEdis90.png", plot = g3a, height = 10, width = 6)

g3b <- ggplot(data = filter(pd3b, t >= 600), aes(x = t, y = value / target_pop * 1e6, col = factor(vaccine_start_date))) +
  geom_line() +
  facet_wrap(~duration_R) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  labs(x = "Time (days)", y = "Deaths per million per day", col = "Vaccination start date")
g3b

ggsave("plots/fig3b_timing.png", plot = g3b, height = 5, width = 14)

