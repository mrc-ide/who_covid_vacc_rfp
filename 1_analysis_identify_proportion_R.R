### Load packages ##############################################################
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)

# Estimate proportion in R at last time point
summarise_R <- function(x, timing, pop){
  d <- filter(x, compartment == "R", t <=timing) %>%
    filter(t == max(t)) %>%
    pull(value)
  d / pop
}

dat <- readRDS("output/1_identify_proportion_R.rds") %>%
  mutate(`Income group` = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  mutate(t_end = as.Date(vaccine_start_date) - as.Date(date_start) + 1) %>%
  mutate(prop_R = map2_dbl(output, t_end, summarise_R, pop = 50e6))

select(dat, income_group, Rt1, prop_R) %>%
  mutate(prop_R = paste0(round(prop_R*100,1), "%"))

# plot the counterfactual trajectories by income setting
pd1 <- dat %>%
  select(R0, Rt1, Rt2, `Income group`, output_cf, prop_R, date_start, target_pop) %>%
  unnest(cols = output_cf) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(compartment == "deaths") 

g1 <- ggplot(data = pd1, aes(x = t, y = value/target_pop * 1e6, col = factor(Rt1))) +
  geom_line() +
  facet_wrap( ~ `Income group`, labeller = label_both, ncol = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line()) +
  labs(x = "Time (days)", y = "Deaths per million per day", col = "Rt1") +
  scale_color_brewer(type = "qual", palette = "Dark2")

g1

ggsave("plots/fig1.png", plot = g1, height = 8, width = 6)

# plot for LMIC only
g1b <- ggplot(data = filter(pd1, `Income group` == "LMIC"), aes(x = t, y = value/target_pop * 1e6, col = factor(Rt1))) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line()) +
  labs(x = "Time (days)", y = "Deaths per million per day", col = "Rt1") +
  scale_color_brewer(type = "qual", palette = "Dark2")

g1b
ggsave("plots/fig1b.png", plot = g1b, height = 4, width = 6)
