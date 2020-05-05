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

colnames(a_max_ICU_req) <- paste0("ICU_inc", as.integer(trigger_thresholds))
a_max_ICU <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), a_max_ICU_req)
a_max_ICU <- tidyr::gather(a_max_ICU, threshold, max_capacity, -setting)

colnames(a_time_in_lockdown) <- paste0("ICU_inc", as.integer(trigger_thresholds))
a_lockdown_time <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), a_time_in_lockdown)
a_lockdown_time <- tidyr::gather(a_lockdown_time, threshold, time_in_lockdown, -setting)

colnames(a_deaths) <- paste0("ICU_inc", as.integer(trigger_thresholds))
a_total_deaths <- data.frame(setting = c("LIC", "LMIC", "UMIC", "HIC"), a_deaths)
a_total_deaths <- tidyr::gather(a_total_deaths, threshold, deaths, -setting)

no_constraints_overall <- a_max_ICU %>%
  left_join(a_lockdown_time, by = c("setting", "threshold")) %>%
  left_join(a_total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC", "LMIC", "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold, levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) %>%
  filter(max_capacity == min(max_capacity))

saveRDS(no_constraints_overall, "no_constraints_overall_df.rds")
no_constraints_overall <- readRDS("no_constraints_overall_df.rds")

x <- no_constraints_overall %>%
  filter(!(setting == "LIC" & threshold == "ICU_inc200"),
         !(setting == "LIC" & threshold == "ICU_inc270"),
         !(setting == "LIC" & threshold == "ICU_inc500"),
         !(setting == "LIC" & threshold == "ICU_inc800"),
         !(setting == "LMIC" & threshold == "ICU_inc335"),
         !(setting == "LMIC" & threshold == "ICU_inc2500"),
         !(setting == "UMIC" & threshold == "ICU_inc400"),
         !(setting == "UMIC" & threshold == "ICU_inc800"),
         !(setting == "UMIC" & threshold == "ICU_inc3000"),
         !(setting == "HIC" & threshold == "ICU_inc700"))
ggplot(x, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2)

a <- ggplot(x, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  xlim(c(0, 0.8)) +
  geom_point(aes(x = 0.57, y = LIC_icu), colour = "#B7C0EE", size = 5) +
  geom_point(aes(x = 0.5, y = LMIC_icu), colour = "#7067CF", size = 5) +
  geom_point(aes(x = 0.45, y = UMIC_icu), colour = "#362E91", size = 5) +
  geom_point(aes(x = 0.4, y = HIC_icu), colour = "#241F60", size = 5) +
  theme_bw() +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown")
a

### 2. Running With Capacity Constraints to Examine Time In Suppression vs Deaths
###     -> For Runs Where We Have an Initial Suppression Based On Known Timings for Income Strata
income_strata <- c("LIC", "LIC", "LMIC", "LMIC", "UMIC", "HIC")
poorer_outcomes <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
countries <- c("Madagascar", "Madagascar", "Nicaragua", "Nicaragua", "Grenada", "Malta")
raw_death_triggers <- c(0, 0, 0.00243, 0.00243, 0.0553, 0.157)
death_triggers <- round(50 * raw_death_triggers)
b_max_ICU_req <- matrix(nrow = 6, ncol = length(trigger_thresholds))
b_time_in_lockdown <- matrix(nrow = 6, ncol = length(trigger_thresholds))
b_deaths <- matrix(nrow = 6, ncol = length(trigger_thresholds))
b_list <- vector(mode = "list", length = 6)
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
    b_max_ICU_req[i, j] <- get_max_ICU_req(x)
    b_time_in_lockdown[i, j] <- get_time_in_lockdown(x)
    b_deaths[i, j] <- get_total_deaths(x)
    print(j)
  }
}
colnames(b_max_ICU_req) <- paste0("ICU_inc", as.integer(trigger_thresholds))
b_max_ICU <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), b_max_ICU_req)
b_max_ICU <- gather(b_max_ICU, threshold, max_capacity, -setting)

colnames(b_time_in_lockdown) <- paste0("ICU_inc", as.integer(trigger_thresholds))
b_lockdown_time <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), b_time_in_lockdown)
b_lockdown_time <- gather(b_lockdown_time, threshold, time_in_lockdown, -setting)

colnames(b_deaths) <- paste0("ICU_inc", as.integer(trigger_thresholds))
b_total_deaths <- data.frame(setting = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"), b_deaths)
b_total_deaths <- gather(b_total_deaths, threshold, deaths, -setting)

constraints_overall <- b_max_ICU %>%
  left_join(b_lockdown_time, by = c("setting", "threshold")) %>%
  left_join(b_total_deaths, by = c("setting", "threshold")) %>%
  mutate(setting = factor(setting, levels = c("LIC_poor", "LIC", "LMIC_poor", "LMIC", "UMIC", "HIC"))) %>%
  mutate(threshold = factor(threshold, levels = paste0("ICU_inc", as.integer(trigger_thresholds)))) %>%
  group_by(setting, time_in_lockdown) %>%
  filter(max_capacity == min(max_capacity))

saveRDS(constraints_overall, "constraints_overall_df.rds")
constraints_overall <- readRDS("constraints_overall_df.rds")

y <- constraints_overall %>%
  filter(!(setting == "LMIC" & threshold == "ICU_inc50"),
         !(setting == "LMIC" & threshold == "ICU_inc120"),
         !(setting == "LMIC" & threshold == "ICU_inc800"),
         !(setting == "LMIC" & threshold == "ICU_inc1000"),
         !(setting == "LMIC" & threshold == "ICU_inc2250"),
         !(setting == "LMIC" & threshold == "ICU_inc370"),
         !(setting == "LMIC" & threshold == "ICU_inc270"),
         !(setting == "LMIC" & threshold == "ICU_inc500"),
         !(setting == "LMIC" & threshold == "ICU_inc600"),
         !(setting == "LMIC" & threshold == "ICU_inc180"),
         !(setting == "LMIC" & threshold == "ICU_inc90"),
         !(setting == "LIC" & threshold == "ICU_inc70"),
         !(setting == "LIC" & threshold == "ICU_inc160"),
         !(setting == "LIC" & threshold == "ICU_inc80"),
         !(setting == "LIC" & threshold == "ICU_inc100"),
         !(setting == "LIC" & threshold == "ICU_inc700"),
         !(setting == "LIC" & threshold == "ICU_inc450"),
         !(setting == "LIC" & threshold == "ICU_inc270"),
         !(setting == "LIC" & threshold == "ICU_inc370"),
         !(setting == "LIC" & threshold == "ICU_inc140"),
         !(setting == "LIC" & threshold == "ICU_inc200"),
         !(setting == "LIC" & threshold == "ICU_inc600"),
         !(setting == "LIC_poor" & threshold == "ICU_inc70"),
         !(setting == "LIC_poor" & threshold == "ICU_inc90"),
         !(setting == "LIC_poor" & threshold == "ICU_inc80"),
         !(setting == "LIC_poor" & threshold == "ICU_inc100"),
         !(setting == "LIC_poor" & threshold == "ICU_inc140"),
         !(setting == "LIC_poor" & threshold == "ICU_inc800"),
         !(setting == "LIC_poor" & threshold == "ICU_inc600"),
         !(setting == "LIC_poor" & threshold == "ICU_inc700"),
         !(setting == "LIC_poor" & threshold == "ICU_inc370"),
         !(setting == "LIC_poor" & threshold == "ICU_inc450"),
         !(setting == "LIC_poor" & threshold == "ICU_inc270"),
         !(setting == "LIC_poor" & threshold == "ICU_inc200"),
         !(setting == "LIC_poor" & threshold == "ICU_inc335"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc50"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc90"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc60"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc200"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc100"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc180"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc270"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc370"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc500"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc600"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc800"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc1000"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc2250"),
         !(setting == "UMIC" & threshold == "ICU_inc60"),
         !(setting == "UMIC" & threshold == "ICU_inc20"),
         !(setting == "UMIC" & threshold == "ICU_inc70"),
         !(setting == "UMIC" & threshold == "ICU_inc1500"),
         !(setting == "UMIC" & threshold == "ICU_inc140"),
         !(setting == "UMIC" & threshold == "ICU_inc235"),
         !(setting == "UMIC" & threshold == "ICU_inc370"),
         !(setting == "UMIC" & threshold == "ICU_inc450"),
         !(setting == "UMIC" & threshold == "ICU_inc800"),
         !(setting == "UMIC" & threshold == "ICU_inc2000"),
         !(setting == "UMIC" & threshold == "ICU_inc500"),
         !(setting == "HIC" & threshold == "ICU_inc180"),
         !(setting == "HIC" & threshold == "ICU_inc270"),
         !(setting == "HIC" & threshold == "ICU_inc70"),
         !(setting == "HIC" & threshold == "ICU_inc180"),
         !(setting == "HIC" & threshold == "ICU_inc80"),
         !(setting == "HIC" & threshold == "ICU_inc90"),
         !(setting == "HIC" & threshold == "ICU_inc335"),
         !(setting == "HIC" & threshold == "ICU_inc200"),
         !(setting == "HIC" & threshold == "ICU_inc300"),
         !(setting == "HIC" & threshold == "ICU_inc700"),
         !(setting == "HIC" & threshold == "ICU_inc900"),
         !(setting == "HIC" & threshold == "ICU_inc1250"),
         !(setting == "HIC" & threshold == "ICU_inc1750"))

b <- ggplot(y, aes(x = time_in_lockdown, y = deaths, col = setting)) +
  geom_path(size = 2, aes(linetype = setting)) +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor",
                                 "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#fcb15b", "#B7C0EE", "#FB7171", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  xlim(c(0, 0.8)) +
  scale_linetype_manual(values = c(5, 1, 5, 1, 1, 1)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  labs(y = "Total Number of Deaths", x = "Proportion of Time in Lockdown") +
  guides(linetype = "none")
b
