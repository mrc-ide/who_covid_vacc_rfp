
### Load packages ##############################################################
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)

source("R/plotting_specifics.R")

################################################################################

dat <- readRDS("output/4_voc_matrix.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) 

dat4 <- dat %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a))) %>%
  mutate(`Transmission` = if_else(Rt2 == 3.5, "Default transmission", "Higher VOC transmission"),
         `VE infection` = if_else(efficacy_infection == 0.63, "Default efficacy", "Lower VOC efficacy"))

dat4a <- dat4 %>%
  pivot_longer(cols = c(deaths_averted_phase1, deaths_averted_phase2)) %>%
  mutate(name = factor(name, levels = c("deaths_averted_phase2", "deaths_averted_phase1"), labels = c("Phase 2 (2022-23)", "Phase 1 (2021-22)")))


g4 <- ggplot(data = filter(dat4a, max_coverage == 1), aes(x = `Age.target`, y = value / vaccine_n_phase1 * 100, fill = `Age.target`, alpha = name)) +
  geom_bar(stat = "identity") +
  facet_grid(`Transmission` ~ `VE infection` + `Income group`) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per 100 FVP", fill = "Age coverage \ntarget (years)") +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g4

ggsave("plots/fig4_FVP.png", plot = g4, height = 6, width = 14)

#######################################################################
income_group <- c("HIC", "UMIC", "LMIC", "LIC")

for (i in 1:4){
  icg <- income_group[i]
  g4_icg <- ggplot(data = filter(dat4a, income_group == icg, max_coverage == 1), aes(x = `Age.target`, y = value / vaccine_n_phase1 * 100, fill = `Age.target`, alpha = name)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5), size = 2.5) +
    facet_grid(`Transmission` ~ `VE infection`) +
    labs(x = "Age coverage target (years)", y = "Deaths averted per 100 FVP", fill = "Age coverage \ntarget (years)", title = paste0("Income setting: ", icg)) +
    scale_fill_manual(values = c(col1, col2, col3, col4)) +
    scale_alpha_discrete("Period", range = c(0.25, 1)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA, color = "white"),
          panel.border = element_blank(),
          axis.line = element_line())
  ggsave(paste0("plots/fig4_", icg, "_FVP_text.png"), plot = g4_icg, height = 6, width = 7)
}

#######################################################################
dat4b <- dat4 %>%
  unnest(cols = output) %>%
  filter(compartment == "deaths",
         Rt1 == 1.2) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(date >= as.Date(vaccine_start_date), date <= as.Date("2023-06-30")) %>%
  mutate(t = t - as.numeric(as.Date(vaccine_start_date) - as.Date(date_start)) + 1)

for (i in 1:4){
  icg <- income_group[i]
g4b_icg <- ggplot() +
  geom_line(data = filter(dat4b, max_coverage == 0, income_group == icg),
            aes(x = t, y = value / target_pop * 1e6, linetype = factor(max_coverage)), col = "black") +
  geom_line(data = filter(dat4b, date > as.Date("2021-01-01"), max_coverage != 0, income_group == icg), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = factor(max_coverage))) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 122, linetype = "dotted") +
  geom_vline(xintercept = 122+365, linetype = "dotted") +
  geom_vline(xintercept = 122+365*2, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c(col1, col2, col3, col4)) +
  facet_grid(`Transmission` ~ `VE infection`) +  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Time (days)", y = "Deaths per million per day", linetype = "Intervention", col = "Age coverage \ntarget (years)", title = paste0("Income setting: ", icg)) +
  annotate(geom = "text", x = 0, label="\nvaccination start", y=80, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122, label="vaccination stop\n", y=80, colour="darkgrey", angle=90) +
annotate(geom = "text", x = 122+365, label="phase 1 end\n", y=80, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122+365*2, label="phase 2 end\n", y=80, colour="darkgrey", angle=90)

ggsave(paste0("plots/fig4b_", icg, ".png"), plot = g4b_icg, height = 6, width = 10)
}
