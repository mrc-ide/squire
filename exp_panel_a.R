# Loading Rrequired Libraries
library(tidyverse); library(zoo); library(patchwork)

# Sourcing Required Functions
source("trigger_running_function.R")

# Run Invariant Parameters
suppression_reduction <- c(0.25, 0.15, 0.05)
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 15
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
replicates <- 100
r <- run_explicit_SEEIR_model("United Kingdom")
index <-  squire:::odin_index(r$model)

# Running for LIC
income_strata <- "LIC"
trigger_threshold <- 33
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")

med_LIC <- run_trigger_threshold(country = country, population = pop,
                                 replicates = replicates,
                                 income_strata = income_strata,
                                 trigger_threshold = trigger_threshold,
                                 suppression_reduction = suppression_reduction[2],
                                 suppression_duration = suppression_duration,
                                 mitigation_reduction = mitigation_reduction,
                                 R0 = R0, tt_R0 = tt_R0,
                                 max_lockdowns = max_lockdowns,
                                 hospital_bed_capacity = 10000000,
                                 ICU_bed_capacity = 10000000,
                                 income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                 poorer_outcomes = FALSE)

high_LIC <- run_trigger_threshold(country = country, population = pop,
                                  replicates = replicates,
                                  income_strata = income_strata,
                                  trigger_threshold = trigger_threshold,
                                  suppression_reduction = suppression_reduction[3],
                                  suppression_duration = suppression_duration,
                                  mitigation_reduction = mitigation_reduction,
                                  R0 = R0, tt_R0 = tt_R0,
                                  max_lockdowns = max_lockdowns,
                                  hospital_bed_capacity = 10000000,
                                  ICU_bed_capacity = 10000000,
                                  income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                  poorer_outcomes = FALSE)

mid <- process_output(med_LIC)
mid_time <- round(apply(med_LIC$time_in_lockdown[1:5500, ], 1, median))
mid$scenario <- "mid"

high <- process_output(high_LIC)
high_time <- round(apply(high_LIC$time_in_lockdown[1:5500, ], 1, median))
high$scenario <- "high"

overall <- rbind(mid, high) %>%
  cbind(lockdown = c(mid_time, high_time)) %>%
  filter(time > 25 & time < 120)

ggplot(overall, aes(x = time, y = median, colour = scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("red", "green")) +
  scale_fill_manual(values = c("red", "green")) +
  geom_line(aes(x = 50, y = lockdown * 500), colour = "black") +
  geom_line(aes(x = 94, y = lockdown * 500), colour = "green") +
  geom_line(aes(x = 109, y = lockdown * 500), colour = "red") +
  theme_bw()


# Running for LIC
replicates <- 20
income_strata <- "LIC"
trigger_threshold <- 30
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")

low_thresh_LIC <- run_trigger_threshold(country = country, population = pop,
                                        replicates = replicates,
                                        income_strata = income_strata,
                                        trigger_threshold = trigger_threshold,
                                        suppression_reduction = suppression_reduction[2],
                                        suppression_duration = suppression_duration,
                                        mitigation_reduction = mitigation_reduction,
                                        R0 = R0, tt_R0 = tt_R0,
                                        max_lockdowns = max_lockdowns,
                                        hospital_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000,
                                        income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                        poorer_outcomes = FALSE)

trigger_threshold <- 500
high_thresh_LIC <- run_trigger_threshold(country = country, population = pop,
                                         replicates = replicates,
                                         income_strata = income_strata,
                                         trigger_threshold = trigger_threshold,
                                         suppression_reduction = suppression_reduction[2],
                                         suppression_duration = suppression_duration,
                                         mitigation_reduction = mitigation_reduction,
                                         R0 = R0, tt_R0 = tt_R0,
                                         max_lockdowns = max_lockdowns,
                                         hospital_bed_capacity = 10000000,
                                         ICU_bed_capacity = 10000000,
                                         income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                         poorer_outcomes = FALSE)

low_thresh <- process_output(low_thresh_LIC)
low_thresh$scenario <- "low"
low_thresh_time <- round(apply(low_thresh_LIC$time_in_lockdown[1:5500, ], 1, median))

high_thresh <- process_output(high_thresh_LIC)
high_thresh$scenario <- "high"
high_thresh_time <- round(apply(high_thresh_LIC$time_in_lockdown[1:5500, ], 1, median))

a <- ggplot(low_thresh, aes(x = time, y = median)) +
  geom_line(colour = "#5B85AA") +
  labs(x = "Time (Days)") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#5B85AA", alpha = 0.2, colour = NA) +
  theme_bw()

b <- ggplot(high_thresh, aes(x = time, y = median)) +
  geom_line(colour = "#F46036") +
  labs(x = "Time (Days)") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#F46036", alpha = 0.2, colour = NA) +
  theme_bw()

a + b +
  plot_layout(nrow = 2)





#geom_ribbon(aes(ymin = 0, ymax = lockdown * 500, fill = scenario), alpha = 0.1, colour = NA) +

