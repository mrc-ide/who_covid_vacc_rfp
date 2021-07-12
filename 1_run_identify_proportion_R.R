# Author: AB Hogan
# 1 June 2021

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

# transmission
date_start <- "2020-03-01"
Rt1_start <- "2020-05-01"
Rt2_start <- "2021-07-01"
Rt2_end <- "2021-12-31"
R0 <- 2.5
Rt1 <- c(1.05, 1.2, 1.25, 1.3, 1.4)
Rt2 <- 3.5 #c(2.5, 3.5, 4.5)

# vaccine
max_coverage <- 0
vaccine_start_date <- "2021-03-01"

# Pop size
target_pop <- 50e6

# Scenario table
scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         date_start = date_start,
                         vaccine_start_date = vaccine_start_date,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         Rt1_start = Rt1_start,
                         Rt2_start = Rt2_start,
                         Rt2_end = Rt2_end,
                         max_coverage = max_coverage)

nrow(scenarios)

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(scenarios, run_vaccine_income_setting, .progress = TRUE, .options = furrr_options(seed = NULL))})

#### Format output #############################################################
out_format <- format_out(out, scenarios)
################################################################################

### Save output ################################################################
saveRDS(out, "output/1_identify_proportion_R.rds")
saveRDS(out_format, "output/1_identify_proportion_R.rds")
################################################################################