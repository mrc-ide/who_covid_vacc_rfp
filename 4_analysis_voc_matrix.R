
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

g4 <- ggplot(data = dat4a, aes(x = `Age.target`, y = (value / 50e6 * 1e6), fill = `Age.target`, alpha = name)) +
  geom_bar(stat = "identity") +
  facet_grid(`Transmission` ~ `VE infection` + `Income group`) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g4

ggsave("plots/fig4_deaths.png", plot = g4, height = 6, width = 14)

#######################################################################

g4_plot_func <- function(data, icg){
  ggplot(data = filter(data, income_group == icg, max_coverage != 0), aes(x = `Age.target`, y = (value / 50e6 * 1e6), fill = `Age.target`, alpha = name)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=(round(value / 50e6 * 1e6))), stat = "identity", position = position_stack(vjust = 0.5), size = 2.5) +
    facet_grid(`Transmission` ~ `VE infection` ) +
    labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)", title = paste0("Income setting: ", icg)) +
    scale_fill_manual(values = c(col1, col2, col3, col4)) +
    scale_alpha_discrete("Period", range = c(0.25, 1)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA, color = "white"),
          panel.border = element_blank(),
          axis.line = element_line())
}

g4_HIC <- g4_plot_func(dat4a, "HIC")
ggsave("plots/fig4_HIC_text.png", plot = g4_HIC, height = 6, width = 7)
g4_UMIC <- g4_plot_func(dat4a, "UMIC")
ggsave("plots/fig4_UMIC_text.png", plot = g4_UMIC, height = 6, width = 7)
g4_LMIC <- g4_plot_func(dat4a, "LMIC")
ggsave("plots/fig4_LMIC_text.png", plot = g4_LMIC, height = 6, width = 7)
g4_LIC <- g4_plot_func(dat4a, "LIC")
ggsave("plots/fig4_LIC_text.png", plot = g4_LIC, height = 6, width = 7)


#######################################################################
dat4b <- dat4 %>%
  unnest(cols = output) %>%
  filter(compartment == "deaths",
         Rt1 == 1.2) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(date >= as.Date(vaccine_start_date), date <= as.Date("2023-06-30")) %>%
  mutate(t = t - as.numeric(as.Date(vaccine_start_date) - as.Date(date_start)) + 1)

g4b_plot_func <- function(data, icg){
  ggplot() +
    geom_line(data = filter(data, max_coverage == 0, income_group == icg),
              aes(x = t, y = value / target_pop * 1e6, linetype = factor(max_coverage)), col = "black") +
    geom_line(data = filter(data, date > as.Date("2021-01-01"), max_coverage != 0, income_group == icg), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = factor(max_coverage))) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 122, linetype = "dotted") +
    geom_vline(xintercept = 122+365, linetype = "dotted") +
    geom_vline(xintercept = 122+365*2, linetype = "dotted") +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_color_manual(values = c(col1, col2, col3, col4)) +
    facet_grid(`Transmission` ~ `VE infection`) +  
    theme_bw() +
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
}
  
g4b_HIC <- g4b_plot_func(dat4b, "HIC")
ggsave("plots/fig4b_HIC.png", plot = g4b_HIC, height = 6, width = 10)
g4b_UMIC <- g4b_plot_func(dat4b, "UMIC")
ggsave("plots/fig4b_UMIC.png", plot = g4b_UMIC, height = 6, width = 10)
g4b_LMIC <- g4b_plot_func(dat4b, "LMIC")
ggsave("plots/fig4b_LMIC.png", plot = g4b_LMIC, height = 6, width = 10)
g4b_LIC <- g4b_plot_func(dat4b, "LIC")
ggsave("plots/fig4b_LIC.png", plot = g4b_LIC, height = 6, width = 10)
