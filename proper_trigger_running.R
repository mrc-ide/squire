# Loading Required Libraries
library(tidyverse); library(zoo)

# Sourcing Functions for Running Model With Threshold Based Triggers
source("trigger_running_function.R")

# Trigger Thresholds to Use During Model Running
trigger_thresholds <- c(1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120,
                        140, 160, 180, 200, 250, 300, 350, 420, 500, 600, 700,
                        850, 1000, 1200, 1500, 2000, 2500,
                        3000, 4000, 5000, 6000, 7000, 8000, 10000, 20000,
                        30000, 40000, 50000, 75000, 100000, 150000, 200000)

trigger_thresholds <- c(2, 10, 30, 50, 100, 850, 1000, 2500, 30000, 50000, 100000)

# Defining Parameters Used in All Model Runs
replicates <- 3
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
suppression_reduction <- 0.15
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 15
income_strata_healthcare_capacity <- squire::income_strata_healthcare_capacity

# Income Strata ICU Capacity
LIC_icu <- (1.5 * 50000000 * 1.25/1000)/100
LMIC_icu <- (2 * 50000000 * 2/1000)/100
UMIC_icu <- (3 * 50000000 * 2.5/1000)/100
HIC_icu <- (3.5 * 50000000 * 4.5/1000)/100

### 1. Running Without Capacity Constraints to Examine Time In Suppression vs Capacity Required
income_strata <- c("LIC", "LMIC", "UMIC", "HIC")
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
adjusted_max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
time_in_lockdown <- matrix(nrow = 4, ncol = length(trigger_thresholds))
deaths <- matrix(nrow = 4, ncol = length(trigger_thresholds))
for (i in 1:length(countries)) {
  pop <- get_population(countries[i])
  pop <- (50000000/sum(pop$n)) * pop$n
  for (j in 1:length(trigger_thresholds)) {
    x <- run_trigger_threshold(country = countries[i], population = pop,
                               income_strata = income_strata[i],
                               replicates = replicates,
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

    max_ICU_req[i, j] <- get_max_ICU_req(x)
    time_in_lockdown[i, j] <- get_time_in_lockdown(x)
    deaths[i, j] <- get_total_deaths(x)
    print(j)
  }
}

colnames(max_ICU_req) <- paste0("ICU_inc", as.integer(trigger_thresholds))
max_ICU <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), max_ICU_req)
max_ICU <- gather(max_ICU, threshold, max_capacity, -setting)

colnames(time_in_lockdown) <- paste0("ICU_inc", as.integer(trigger_thresholds))
lockdown_time <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), time_in_lockdown)
lockdown_time <- gather(lockdown_time, threshold, time_in_lockdown, -setting)

colnames(deaths) <- paste0("ICU_inc", as.integer(trigger_thresholds))
total_deaths <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), deaths)
total_deaths <- gather(total_deaths, threshold, deaths, -setting)

no_constraints_overall <- max_ICU %>%
  left_join(lockdown_time, by = c("setting", "threshold")) %>%
  left_join(total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC", "LMIC",
                                              "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold,
                            levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) %>%
  filter(max_capacity == min(max_capacity))

a <- ggplot(no_constraints_overall, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  geom_point(aes(x = 0.53, y = LIC_icu), colour = "#B7C0EE", size = 5) +
  geom_point(aes(x = 0.47, y = LMIC_icu), colour = "#7067CF", size = 5) +
  geom_point(aes(x = 0.42, y = UMIC_icu), colour = "#362E91", size = 5) +
  geom_point(aes(x = 0.38, y = HIC_icu), colour = "#241F60", size = 5) +
  theme_bw() +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown")


### 2. Running With Capacity Constraints to Examine Time In Suppression vs Deaths
income_strata <- c("LIC", "LIC", "LMIC", "LMIC", "UMIC", "HIC")
poorer_outcomes <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
countries <- c("Madagascar", "Madagascar", "Nicaragua", "Nicaragua", "Grenada", "Malta")
max_ICU_req <- matrix(nrow = 6, ncol = length(trigger_thresholds))
adjusted_max_ICU_req <- matrix(nrow = 6, ncol = length(trigger_thresholds))
time_in_lockdown <- matrix(nrow = 6, ncol = length(trigger_thresholds))
deaths <- matrix(nrow = 6, ncol = length(trigger_thresholds))
for (i in 1:length(countries)) {
  pop <- get_population(countries[i])
  pop <- (50000000/sum(pop$n)) * pop$n
  for (j in 1:length(trigger_thresholds)) {
    x <- run_trigger_threshold(country = countries[i], population = pop,
                               income_strata = income_strata[i],
                               replicates = replicates,
                               trigger_threshold = trigger_thresholds[j],
                               suppression_reduction = suppression_reduction,
                               suppression_duration = suppression_duration,
                               mitigation_reduction = mitigation_reduction,
                               R0 = R0, tt_R0 = tt_R0,
                               max_lockdowns = max_lockdowns,
                               hospital_bed_capacity = NULL,
                               ICU_bed_capacity = NULL,
                               income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                               poorer_outcomes = poorer_outcomes[i])
    max_ICU_req[i, j] <- get_max_ICU_req(x)
    time_in_lockdown[i, j] <- get_time_in_lockdown(x)
    deaths[i, j] <- get_total_deaths(x)
    print(j)
  }
}

colnames(max_ICU_req) <- paste0("ICU_inc", trigger_thresholds)
max_ICU <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), max_ICU_req)
max_ICU <- gather(max_ICU, threshold, max_capacity, -setting)

colnames(time_in_lockdown) <- paste0("ICU_inc", trigger_thresholds)
lockdown_time <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), time_in_lockdown)
lockdown_time <- gather(lockdown_time, threshold, time_in_lockdown, -setting)

colnames(deaths) <- paste0("ICU_inc", trigger_thresholds)
total_deaths <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), deaths)
total_deaths <- gather(total_deaths, threshold, deaths, -setting)

constraints_overall <- max_ICU %>%
  left_join(lockdown_time, by = c("setting", "threshold")) %>%
  left_join(total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC_poor", "LIC", "LMIC_poor",
                                              "LMIC", "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold,
                            levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) %>%
  filter(max_capacity == min(max_capacity))

b <- ggplot(constraints_overall, aes(x = time_in_lockdown, y = deaths, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("blue", "#B7C0EE", "blue", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  labs(y = "Total Number of Deaths", x = "Proportion of Time in Lockdown")


### 3. Running Without Capacity Constraints to Examine Time In Suppression vs Capacity Required
###     -> For Runs Where We Have an Initial Suppression Based On Known Timings for Income Strata
income_strata <- c("LIC", "LMIC", "UMIC", "HIC")
countries <- c("Madagascar", "Nicaragua", "Grenada", "Malta")
raw_death_triggers <- c(0, 0.00243, 0.0553, 0.157)
death_triggers <- round(50 * raw_death_triggers)
max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
adjusted_max_ICU_req <- matrix(nrow = 4, ncol = length(trigger_thresholds))
time_in_lockdown <- matrix(nrow = 4, ncol = length(trigger_thresholds))
deaths <- matrix(nrow = 4, ncol = length(trigger_thresholds))
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

    max_ICU_req[i, j] <- get_max_ICU_req(x)
    time_in_lockdown[i, j] <- get_time_in_lockdown(x)
    deaths[i, j] <- get_total_deaths(x)
    print(j)
  }
}

colnames(max_ICU_req) <- paste0("ICU_inc", as.integer(trigger_thresholds))
max_ICU <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), max_ICU_req)
max_ICU <- gather(max_ICU, threshold, max_capacity, -setting)

colnames(time_in_lockdown) <- paste0("ICU_inc", as.integer(trigger_thresholds))
lockdown_time <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), time_in_lockdown)
lockdown_time <- gather(lockdown_time, threshold, time_in_lockdown, -setting)

colnames(deaths) <- paste0("ICU_inc", as.integer(trigger_thresholds))
total_deaths <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), deaths)
total_deaths <- gather(total_deaths, threshold, deaths, -setting)

no_constraints_overall <- max_ICU %>%
  left_join(lockdown_time, by = c("setting", "threshold")) %>%
  left_join(total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC", "LMIC",
                                              "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold,
                            levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) %>%
  filter(max_capacity == min(max_capacity))

c <- ggplot(no_constraints_overall, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  xlim(c(0, 0.6)) +
  geom_point(aes(x = 0.53, y = LIC_icu), colour = "#B7C0EE", size = 5) +
  geom_point(aes(x = 0.47, y = LMIC_icu), colour = "#7067CF", size = 5) +
  geom_point(aes(x = 0.42, y = UMIC_icu), colour = "#362E91", size = 5) +
  geom_point(aes(x = 0.38, y = HIC_icu), colour = "#241F60", size = 5) +
  theme_bw() +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown")


### 4. Running With Capacity Constraints to Examine Time In Suppression vs Deaths
###     -> For Runs Where We Have an Initial Suppression Based On Known Timings for Income Strata
income_strata <- c("LIC", "LIC", "LMIC", "LMIC", "UMIC", "HIC")
poorer_outcomes <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
countries <- c("Madagascar", "Madagascar", "Nicaragua", "Nicaragua", "Grenada", "Malta")
raw_death_triggers <- c(0, 0, 0.00243, 0.00243, 0.0553, 0.157)
death_triggers <- round(50 * raw_death_triggers)
max_ICU_req <- matrix(nrow = 6, ncol = length(trigger_thresholds))
adjusted_max_ICU_req <- matrix(nrow = 6, ncol = length(trigger_thresholds))
time_in_lockdown <- matrix(nrow = 6, ncol = length(trigger_thresholds))
deaths <- matrix(nrow = 6, ncol = length(trigger_thresholds))
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
                                         hospital_bed_capacity = NULL,
                                         ICU_bed_capacity = NULL,
                                         income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                         poorer_outcomes = poorer_outcomes[i])
    max_ICU_req[i, j] <- get_max_ICU_req(x)
    time_in_lockdown[i, j] <- get_time_in_lockdown(x)
    deaths[i, j] <- get_total_deaths(x)
    print(j)
  }
}
colnames(max_ICU_req) <- paste0("ICU_inc", as.integer(trigger_thresholds))
max_ICU <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), max_ICU_req)
max_ICU <- gather(max_ICU, threshold, max_capacity, -setting)

colnames(time_in_lockdown) <- paste0("ICU_inc", as.integer(trigger_thresholds))
lockdown_time <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), time_in_lockdown)
lockdown_time <- gather(lockdown_time, threshold, time_in_lockdown, -setting)

colnames(deaths) <- paste0("ICU_inc", as.integer(trigger_thresholds))
total_deaths <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), deaths)
total_deaths <- gather(total_deaths, threshold, deaths, -setting)

constraints_overall <- max_ICU %>%
  left_join(lockdown_time, by = c("setting", "threshold")) %>%
  left_join(total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC_poor", "LIC", "LMIC_poor",
                                              "LMIC", "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold,
                            levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) %>%
  filter(max_capacity == min(max_capacity))

d <- ggplot(constraints_overall, aes(x = time_in_lockdown, y = deaths, col = setting)) +
    geom_path(size = 2) +
  geom_point() +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor",
                                 "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "blue", "#7067CF", "blue", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  geom_point(aes(x = 0.53, y = LIC_icu), colour = "#B7C0EE", size = 5) +
  geom_point(aes(x = 0.47, y = LMIC_icu), colour = "#7067CF", size = 5) +
  geom_point(aes(x = 0.42, y = UMIC_icu), colour = "#362E91", size = 5) +
  geom_point(aes(x = 0.38, y = HIC_icu), colour = "#241F60", size = 5) +
  theme_bw() +
  labs(y = "Total Number of Deaths", x = "Proportion of Time in Lockdown")
d
