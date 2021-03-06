
# Specify the country chosen to represent each income group
get_representative_country <- function(income_group){
  case_when(income_group == "HIC" ~ "Malta",
            income_group == "UMIC" ~ "Grenada",
            income_group == "LMIC" ~ "Nicaragua",
            income_group == "LIC" ~ "Madagascar")
}

# Set hospital and ICU capacity
get_capacity <- function(country, income_group, pop, hs_constraints){
  hc <- squire::get_healthcare_capacity(country = country)
  
  # Unconstrained healthcare
  if(hs_constraints == "Absent"){
    hc$hosp_beds <- 1000000
    hc$ICU_beds <- 1000000
  }
  
  if(hs_constraints == "Present"){
    if(income_group %in% c("HIC", "UMIC")){
      hc$hosp_beds <- 1000000
      hc$ICU_beds <- 1000000
    }
    if(income_group %in% c("LMIC", "LIC")){
      hc$ICU_beds <- 0
    }
  }
  
  hc$hosp_beds <- round(hc$hosp_beds * sum(pop) / 1000)
  hc$ICU_beds <- round(hc$ICU_beds * sum(pop) / 1000)
  
  return(hc)
}

# convert this into params for deterministic
beta_to_R0 <- function(beta, dur_IMild, dur_ICase, prob_hosp, mixing_matrix) {
  squire::adjusted_eigen(dur_IMild = dur_IMild, dur_ICase = dur_ICase,
                         prob_hosp = prob_hosp,
                         mixing_matrix =  mixing_matrix) * beta
}

# Parameterise poorer health outcomes in LMIC and LIC
get_prob_non_severe_death_treatment <- function(income_group, hs_constraints){
  psdt <- squire:::probs$prob_non_severe_death_treatment
  
  if(income_group  == "LIC" & hs_constraints == "Present"){
    psdt <- c(rep(0.25, 16), 0.5804312)
  }
  return(psdt)
}

###############################################################################
# run vaccine model
run_vaccine_income_setting <- function(
  R0 = 2.5,
  Rt1 = 1.2,
  Rt2 = 2.5,
  target_pop = 50e6,
  income_group = "HIC",
  hs_constraints = "Present",
  Rt1_start = "2020-04-13",
  Rt2_start = "2021-07-01",
  Rt2_end = "2021-12-31",
  date_start = "current",
  vaccine_start_date = "2021-04-13",
  target_group_switch = 1,
  target_group_stop = 1,
  efficacy_infection = 0.8,
  scaling_eff_dis = 0.6,
  strategy_switch = FALSE,
  vacc_children = TRUE,
  duration_R = 365,
  duration_V = 5000,
  seeding_cases = 60,
  max_coverage = 0.8,
  risk_proportion = 0,
  rel_infect_u10 = 1,
  rel_infectiousness_vaccinated = 0.55,
  takeup_over_65 = 0.85,
  takeup_under_65 = 0.7,
  takeup_under5_HIC = 0.87,
  takeup_under5_UMIC = 0.81,
  takeup_under5_LMIC = 0.77,
  takeup_under5_LIC = 0.7,
  use_DDE = TRUE,
  atol = 0.001,
  rtol = 0.001,
  step_size_min = 0.00001,
  step_size_max = 1,
  step_size_min_allow = TRUE
){
  
  rel_infectiousness_vaccinated <- rep(rel_infectiousness_vaccinated, 17)
  rel_infectiousness <- c(rep(rel_infect_u10,2), rep(1,15))
  
  # Convert dates to date format
  date_start <- as.Date(date_start)
  vaccine_start_date <- as.Date(vaccine_start_date)
  Rt1_start <- as.Date(Rt1_start)
  Rt2_start <- as.Date(Rt2_start)
  Rt2_end <- as.Date(Rt2_end)
  vacc_period <- 122
  vaccine_stop_date <- as.Date(vaccine_start_date + vacc_period)

  # Population and mixing
  rep_country <- get_representative_country(income_group = income_group)
  pop <- squire::get_population(country = rep_country)$n
  pop_standardise <- target_pop / sum(pop)
  pop <- pop * pop_standardise
  mm <- squire::get_mixing_matrix(country = rep_country)
  
  # Hospital capacity
  hc <- get_capacity(country = rep_country,
                     income_group = income_group,
                     pop = pop,
                     hs_constraints = hs_constraints)
  
  # Poorer health outcomes for LMICs and LICs
  pnsdt = get_prob_non_severe_death_treatment(income_group, hs_constraints)

  # Efficacy
  eff_inf <- rep(efficacy_infection, 17)
  eff_dis <- rep(scaling_eff_dis, 17)

  # vaccinate everyone over a period of 4 months
  ind <- 17 - target_group_stop + 1
  vacc_vect <- c(rep(0, 17 - ind), rep(1, ind)) * max_coverage # vector of which groups to target
  mc <- c(rep(takeup_under_65, 13), rep(takeup_over_65, 4)) # hard-code in WHO takeup
  if (income_group == "HIC") {mc[1] <- takeup_under5_HIC}
  if (income_group == "UMIC") {mc[1] <- takeup_under5_UMIC}
  if (income_group == "LMIC") {mc[1] <- takeup_under5_LMIC}
  if (income_group == "LIC") {mc[1] <- takeup_under5_LIC}
  
  # adjust takeup in 10-15-year-old group (i.e. group 3) to represent 12-15 instead.
  mc[3] <- 3/5*mc[3]
  
  target_pop_vacc <- sum(pop * vacc_vect * mc)
  vacc_per_day <- round(target_pop_vacc / vacc_period)
  
  # get timing of vaccine start, changes, and end
  tt_vaccine <-
    c(1,
      (as.numeric(as.Date(vaccine_start_date)) - as.numeric(as.Date(date_start))+1),
      (as.numeric(as.Date(vaccine_stop_date)) - as.numeric(as.Date(date_start))+1))
  
  # vaccinees per day: stop vaccinating after target group reached
  max_vaccine = c(0, vacc_per_day, 0)
  
  # create the prioritisation matrix
  # First X% is always "Elderly". then continue (strategy_switch == FALSE), or option to switch to a uniform all ages strategy (strategy_switch == TRUE)
  
  # note that max_coverage values for WHO framework are hardcoded in here
  m0 <- strategy_matrix("Elderly", max_coverage = takeup_over_65)
  m0_sub <- strategy_matrix("Elderly", max_coverage = takeup_under_65)[5:17,1:13]
  m0[5:17, 1:13] <- m0_sub
  
  if (strategy_switch == FALSE){
    if (vacc_children == FALSE) {
      m1 <- m0[1:14,]
    } else {m1 <- m0}
  } else if (strategy_switch == TRUE){
    ind <- (17 - target_group_switch + 1)
    m1 <- m0[1:ind,]
    m2 <- m0[17,]
    if (vacc_children == FALSE) {m2[1,1:3] <- 0}
    m1 <- rbind(m1, m2)
  }

  # transmission
  R0 <- c(R0, Rt1)
  tt_R0 <- c(0,
             (as.numeric(Rt1_start) - as.numeric(as.Date(date_start))))
  
  # Linear increase to reach Rt2 by Rt2_end
  j <- as.numeric(Rt2_start) - as.numeric(as.Date(date_start))
  k <- as.numeric(Rt2_end) - as.numeric(as.Date(date_start))
  tt_temp <- seq(j,k)
  Rt_temp <- seq(Rt1, Rt2, length.out = length(tt_temp))
    
  # remove first values as they are already in R0 and tt_R0 vectors
  tt_temp <- tt_temp[2:length(tt_temp)]
  Rt_temp <- Rt_temp[2:length(Rt_temp)]
    
  # join
  tt_R0 <- c(tt_R0, tt_temp)
  R0 <- c(R0, Rt_temp)

  t_period <- as.numeric(as.Date("2023-06-30")) - as.numeric(as.Date(date_start)) + 1
  
  # Run
  r1 <- nimue::run(
    time_period = t_period,
    R0 = R0, 
    tt_R0 = tt_R0,
    population = pop,
    contact_matrix_set = mm,
    hosp_bed_capacity = hc$hosp_beds,
    ICU_bed_capacity = hc$ICU_beds,
    prob_non_severe_death_treatment = pnsdt,
    seeding_cases = seeding_cases,
    seed = 1,
    dur_R = duration_R,
    dur_V = duration_V,
    vaccine_efficacy_infection = eff_inf,
    vaccine_efficacy_disease = eff_dis,
    max_vaccine = max_vaccine,
    tt_vaccine = tt_vaccine,
    vaccine_coverage_mat = m1,
    rel_infectiousness = rel_infectiousness,
    rel_infectiousness_vaccinated = rel_infectiousness_vaccinated,
    atol = 1e-5,
    rtol = 1e-5,
    step_size_min = 0.0001,
    step_size_max = 0.1,
    step_size_min_allow = TRUE,
    use_dde = TRUE
  )

  # Create output: 1) wrt time, 2) summaries by age 3) by age and time
  o1 <- nimue::format(r1)

  x <- nimue::format(r1,
                     compartments = NULL,
                     summaries = c("deaths", "infections", "vaccines", "hospitalisations"),
                     reduce_age = FALSE)

  j <- as.numeric(as.Date(vaccine_start_date)) - as.numeric(as.Date(date_start))
  k <- as.numeric(as.Date("2022-06-30")) - as.numeric(as.Date(date_start)) + 1
  l <- as.numeric(as.Date("2023-06-30")) - as.numeric(as.Date(date_start)) + 1

  value_phase1 <- summarise_by_age(x, j, k, "phase1")
  value_phase2 <- summarise_by_age(x, (k+1), l, "phase2")

  o2 <- rbind(value_phase1, value_phase2)

  nms <- colnames(r1$output)
  deaths_vacc_pos <- grep("^deaths_cumu.*4\\]|^deaths_cumu.*5\\]", nms)
  deaths <- r1$output[,c(1,deaths_vacc_pos),]
  o3 <- deaths
  
  tibble(output = list(o1), output_age = list(o2), output_new = list(o3))
}

###############################################################################

format_out <- function(out, scenarios){
  # Combine_inputs and outputs
  out1 <- bind_cols(scenarios, bind_rows(out))
  # Isolate counterfactual (Coverage == 0)
  outcf <- filter(out1, max_coverage == 0) %>%
    rename(output_cf = output,
           output_age_cf = output_age,
           output_new_cf = output_new) %>%
    select(-max_coverage) %>%
    unique()
  
  # Combine runs and counterfactual and estimate summaries
  summaries <- left_join(out1, outcf)
  
  m <- ncol(summaries)+1
  n <- ncol(summaries)+14
  
  summaries_phase1 <- summarise_outputs_age(summaries, p = "phase1")
  colnames(summaries_phase1)[m:n] <- paste0(colnames(summaries_phase1)[m:n], "_phase1")
  
  summaries_phase2 <- summarise_outputs_age(summaries, p = "phase2")
  colnames(summaries_phase2)[m:n] <- paste0(colnames(summaries_phase2)[m:n], "_phase2")
  
  summaries <- left_join(summaries, select(summaries_phase1, -contains("output"))) %>%
    left_join(select(summaries_phase2, -contains("output")))
}

# summarise by age and time period
summarise_by_age <- function(x, t_start, t_end, period){
  filter(x, t >= t_start, t <= t_end) %>%
    group_by(age_group, compartment) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = NULL) %>%
    ungroup() %>%
    mutate(period = factor(period))
} 

# Summarise outputs by age over different time periods
summarise_outputs_age <- function(x, p) {
  mutate(x, 
         infections = round(map_dbl(output_age, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations = round(map_dbl(output_age, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths = round(map_dbl(output_age, pull_total, outcome = "deaths", time_period = p), 2),
         yll = round(map_dbl(output_age, summarise_yll, time_period = p), 2),
         infections_cf = round(map_dbl(output_age_cf, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations_cf = round(map_dbl(output_age_cf, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths_cf = round(map_dbl(output_age_cf, pull_total, outcome = "deaths", time_period = p), 2),
         yll_cf = round(map_dbl(output_age_cf, summarise_yll, time_period = p), 2),
         infections_averted = infections_cf - infections,
         hospitalisations_averted = hospitalisations_cf - hospitalisations,
         deaths_averted = deaths_cf - deaths,
         deaths_averted_prop = deaths_averted / deaths_cf,
         years_life_saved = yll_cf - yll,
         vaccine_n = round(map_dbl(output_age, pull_total, outcome = "vaccines", time_period = p)))
}

# Pull sum totals
pull_total <- function(x, outcome, time_period){
  dplyr::filter(x, compartment == outcome, period == time_period) %>%
    pull(value) %>%
    sum()
}

# Estimate total years of life lost
summarise_yll <- function(x, lifespan = 86.6, time_period){
  filter(x, compartment == "deaths", period == time_period) %>%
    mutate(mid_age = (((as.integer(age_group) - 1) * 5) + 2.5),
           yll = pmax(0, (lifespan - mid_age) * value)) %>%
    pull(yll) %>%
    sum()
}

# Extract the proportion vaccinated (by age)
get_pv <- function(x){
  pivot_wider(x, id_cols = c(t, replicate, age_group), values_from = value, names_from = compartment) %>%
    mutate(prop_vaccinated = 1 - unvaccinated / N)
}

prop_older_than <- function(pop, age_index){
  x <- sum(pop[age_index:17])/sum(pop[1:17])
  return(x)
}
