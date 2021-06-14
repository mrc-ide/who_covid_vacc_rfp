
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

dat <- readRDS("output/4_voc_matrix.rds")

dat4 <- dat %>%
  left_join(age_group_key) %>%
  mutate(`Age.target` = factor(`Age.target`, levels = rev(a))) %>%
  mutate(`Transmission` = if_else(Rt2 == 3.5, "Default transmission", "Higher VOC transmission"),
         `Vaccine efficacy` = if_else(efficacy_infection == 0.63, "Default efficacy", "Lower VOC efficacy"))

dat4a <- dat4 %>%
  pivot_longer(cols = c(deaths_averted_phase1, deaths_averted_phase2)) %>%
  mutate(name = factor(name, levels = c("deaths_averted_phase2", "deaths_averted_phase1"), labels = c("Phase 2 (2022-23)", "Phase 1 (2021-22)")))

g4 <- ggplot(data = dat4a, aes(x = `Age.target`, y = (value / 50e6 * 1e6), fill = `Age.target`, alpha = name)) +
  geom_bar(stat = "identity") +
  facet_grid(`Transmission` ~ `Vaccine efficacy`) +
  labs(x = "Age coverage target (years)", y = "Deaths averted per million total population", fill = "Age coverage \ntarget (years)") +
  scale_fill_manual(values = c(col1, col2, col3, col4)) +
  scale_alpha_discrete("Period", range = c(0.25, 1)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g4

ggsave("plots/fig4.png", plot = g4, height = 6, width = 10)

dat4b <- dat4 %>%
  unnest(cols = output) %>%
  filter(compartment == "deaths",
         Rt1 == 1.2) %>%
  mutate(date = as.Date(date_start) + t) %>%
  filter(date >= as.Date(vaccine_start_date), date <= as.Date("2023-06-30")) %>%
  mutate(t = t - as.numeric(as.Date(vaccine_start_date) - as.Date(date_start)) + 1)

g4b <- ggplot() +
  geom_line(data = filter(dat4b, max_coverage == 0),
            aes(x = t, y = value / target_pop * 1e6, linetype = factor(max_coverage)), col = "black") +
  geom_line(data = filter(dat4b, date > as.Date("2021-01-01"), max_coverage != 0), aes(x = t, y = value / target_pop * 1e6, col = `Age.target`, linetype = factor(max_coverage))) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 122, linetype = "dotted") +
  geom_vline(xintercept = 122+365, linetype = "dotted") +
  geom_vline(xintercept = 122+365*2, linetype = "dotted") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c(col1, col2, col3, col4)) +
  facet_grid(`Transmission` ~ `Vaccine efficacy`) +  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x=element_text(angle=60, hjust = 1)
  ) +
  labs(x = "Time (days)", y = "Deaths per million per day", linetype = "Intervention", col = "Age coverage \ntarget (years)") +
  annotate(geom = "text", x = 0, label="\nvaccination start", y=80, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122, label="vaccination stop\n", y=80, colour="darkgrey", angle=90) +
annotate(geom = "text", x = 122+365, label="phase 1 end\n", y=80, colour="darkgrey", angle=90) +
  annotate(geom = "text", x= 122+365*2, label="phase 2 end\n", y=80, colour="darkgrey", angle=90)

g4b
ggsave("plots/fig4b.png", plot = g4b, height = 6, width = 10)
