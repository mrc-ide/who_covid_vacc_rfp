# Author: AB Hogan
# 1 June 2021
# Explore coverage targets by events averted and income setting

# After vaccinated 

library(nimue)
library(ggplot2)
library(dplyr)
library(purrr)
library(furrr)
library(tidyverse)
library(countrycode)
library(lubridate)

#############################################################################

source("R/functions.R")

#############################################################################
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
hs_constraints <- c("Present", "Absent")

# transmission
date_start <- "2020-03-01"
Rt1_start <- "2020-05-01"
Rt2_start <- "2021-07-01"
Rt2_end <- "2021-12-31"
R0 <- 2.5
Rt1 <- c(1.05, 1.2, 1.4)
Rt2 <- 3.5 #c(2.5, 3.5, 4.5)

# Vaccine start date
vaccine_start_date <- "2021-03-01"

# Option to switch strategy
strategy_switch <- FALSE

# Option to include children
vacc_children <- TRUE

# Target group vaccinated before stopping (implemented instead of reaching a target % coverage)
target_group_stop <- c(11, 5, 3, 1) # note that stop group 3 actually represents 12+ instead of 10+ - manual adjustment made in the functions script

takeup_over_65 <- 0.85
takeup_under_65 <- 0.7
takeup_under5_HIC <- 0.87
takeup_under5_UMIC <- 0.81
takeup_under5_LMIC <- 0.77
takeup_under5_LIC <- 0.7

# Efficacy
efficacy_infection <- 0.63
scaling_eff_dis <- 0.73
rel_infectiousness_vaccinated <- 0.55

# Pop size
target_pop <- 50e6

# Max coverage of targeting strategy
# note that the max_coverage values for WHO strategy are hardcoded in the functions
max_coverage <- c(0, 1)

# Scenario table
scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         date_start = date_start,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         Rt1_start = Rt1_start,
                         Rt2_start = Rt2_start,
                         Rt2_end = Rt2_end,
                         vaccine_start_date = vaccine_start_date,
                         target_group_stop = target_group_stop,
                         takeup_over_65 = takeup_over_65,
                         takeup_under_65 = takeup_under_65,
                         takeup_under5_HIC = takeup_under5_HIC,
                         takeup_under5_UMIC = takeup_under5_UMIC,
                         takeup_under5_LMIC = takeup_under5_LMIC,
                         takeup_under5_LIC = takeup_under5_LIC,
                         vacc_children = vacc_children,
                         strategy_switch = strategy_switch,
                         efficacy_infection = efficacy_infection,
                         scaling_eff_dis = scaling_eff_dis,
                         max_coverage = max_coverage,                       
                         rel_infectiousness_vaccinated = rel_infectiousness_vaccinated,
                         hs_constraints = hs_constraints)
nrow(scenarios)

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(scenarios, run_vaccine_income_setting, .progress = TRUE, .options = furrr_options(seed = NULL))})

#### Format output #############################################################
out_format <- format_out(out, scenarios)
################################################################################

### Save output ################################################################
saveRDS(out, "output/2_coverage_targets_raw.rds")
saveRDS(out_format, "output/2_coverage_targets.rds")
################################################################################