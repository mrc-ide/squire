# Loading Required Libraries
library(tidyverse); library(zoo)

# Sourcing Functions for Running Model With Threshold Based Triggers
source("trigger_running_function.R")
load("data/income_strata_healthcare_capacity.rda")

# Trigger Thresholds to Use During Model Running
trigger_thresholds <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120,
                        140, 160, 180, 200, 235, 270, 300, 335, 370, 400, 450, 500,
                        600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2250, 2500,
                        2750, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 8000, 10000,
                        20000, 30000, 40000, 50000, 75000, 100000, 150000)


# Loading in ICU Capacity
load("data/income_strata_healthcare_capacity.rda")
ICU <- income_strata_healthcare_capacity$ICU_beds
LIC_icu <- (ICU[1] * 50000000 /1000) / 2
LMIC_icu <- (ICU[2] * 50000000 /1000) / 2
UMIC_icu <- (ICU[3] * 50000000 /1000) / 2
HIC_icu <- (ICU[4] * 50000000 /1000) / 2

# Defining Parameters Used in All Model Runs
replicates <- 60
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
suppression_reduction <- 0.25
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 20

### 1. Running Without Capacity Constraints to Examine Time In Suppression vs Capacity Required
###     -> For Runs Where We Have an Initial Suppression Based On Known Timings for Income Strata
income_strata <- c("LIC", "LMIC", "UMIC", "HIC")
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
raw_death_triggers <- c(0, 0.00243, 0.0553, 0.157)
death_triggers <- round(50 * raw_death_triggers)
a_max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
a_time_in_lockdown <- matrix(nrow = 4, ncol = length(trigger_thresholds))
a_deaths <- matrix(nrow = 4, ncol = length(trigger_thresholds))
a_list <- vector(mode = "list", length = 4)
for (i in 1:length(countries)) {
  pop <- get_population(countries[i])
  pop <- (50000000/sum(pop$n)) * pop$n
  for (j in 1:length(trigger_thresholds)) {
    x <- realistic_run_trigger_threshold(country = countries[i], population = pop,
                                         replicates = replicates,
                                         income_strata = income_strata[i],
                                         initial_trigger_threshold = death_triggers[i],
                                         trigger_threshold = trigger_thresholds[j],
                                         suppression_reduction = suppression_reduction,
                                         suppression_duration = suppression_duration,
                                         mitigation_reduction = mitigation_reduction,
                                         R0 = R0, tt_R0 = tt_R0,
                                         max_lockdowns = max_lockdowns,
                                         hospital_bed_capacity = 10000000,
                                         ICU_bed_capacity = 10000000,
                                         income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                         poorer_outcomes = FALSE)
    a_max_ICU_req[i, j] <- get_max_ICU_req(x)
    a_time_in_lockdown[i, j] <- get_time_in_lockdown(x)
    a_deaths[i, j] <- get_total_deaths(x)
    print(j)
  }
}
