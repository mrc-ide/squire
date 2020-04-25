# Loading Rrequired Libraries
library(tidyverse); library(zoo); library(patchwork)

# Sourcing Required Functions
source("trigger_running_function.R")

# Run Invariant Parameters
suppression_reduction <- 0.15
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 15
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
income_strata_healthcare_capacity <- squire::income_strata_healthcare_capacity
replicates <- 25
r <- run_explicit_SEEIR_model("United Kingdom")
index <-  squire:::odin_index(r$model)

# Running for LIC
income_strata <- "LIC"
trigger_threshold <- 40
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
LIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                       replicates = replicates,
                                       income_strata = income_strata,
                                       initial_trigger_threshold = death_triggers,
                                       trigger_threshold = trigger_threshold,
                                       suppression_reduction = suppression_reduction,
                                       suppression_duration = suppression_duration,
                                       mitigation_reduction = mitigation_reduction,
                                       R0 = R0, tt_R0 = tt_R0,
                                       max_lockdowns = max_lockdowns,
                                       hospital_bed_capacity = 10000000,
                                       ICU_bed_capacity = 10000000,
                                       income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                       poorer_outcomes = FALSE)

LIC_time <- LIC$model_output[, index$time, 1]
LIC_y <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  apply(LIC$model_output[, index$n_E2_I, p], 1, sum)
})
LIC_y <- do.call(cbind, LIC_y)
LIC_lockdown <- round(apply(LIC$time_in_lockdown[1:5500, ], 1, median))
LIC_z <- data.frame(time = LIC_time, y = LIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
LIC_max <- max(LIC_z$median) * 1.2
a <- ggplot(LIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = LIC_lockdown * LIC_max), alpha = 0.1) +
  geom_line(col = "#B7C0EE", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#B7C0EE") +
  theme_bw() +
  xlab("")

# Running for LMIC
income_strata <- "LMIC"
trigger_threshold <- 120
country <- "Nicaragua"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
LMIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                        replicates = replicates,
                                        income_strata = income_strata,
                                        initial_trigger_threshold = death_triggers,
                                        trigger_threshold = trigger_threshold,
                                        suppression_reduction = suppression_reduction,
                                        suppression_duration = suppression_duration,
                                        mitigation_reduction = mitigation_reduction,
                                        R0 = R0, tt_R0 = tt_R0,
                                        max_lockdowns = max_lockdowns,
                                        hospital_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000,
                                        income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                        poorer_outcomes = FALSE)

LMIC_time <- LMIC$model_output[, index$time, 1]
LMIC_y <- lapply(seq_along(1:dim(LMIC$model_output)[3]), function(p) {
  apply(LMIC$model_output[, index$n_E2_I, p], 1, sum)
})
LMIC_y <- do.call(cbind, LMIC_y)
LMIC_lockdown <- round(apply(LMIC$time_in_lockdown[1:5500, ], 1, median))
LMIC_z <- data.frame(time = LIC_time, y = LMIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
LMIC_max <- max(LMIC_z$median) * 1.2
b <- ggplot(LMIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = LMIC_lockdown * LMIC_max), alpha = 0.1) +
  geom_line(col = "#7067CF", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#7067CF") +
  theme_bw() +
  xlab("")

# Running for UMIC
income_strata <- "UMIC"
trigger_threshold <- 300
country <- "Grenada"
raw_death_trigger <- 3/50
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
UMIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                        replicates = replicates,
                                        income_strata = income_strata,
                                        initial_trigger_threshold = death_triggers,
                                        trigger_threshold = trigger_threshold,
                                        suppression_reduction = suppression_reduction,
                                        suppression_duration = suppression_duration,
                                        mitigation_reduction = mitigation_reduction,
                                        R0 = R0, tt_R0 = tt_R0,
                                        max_lockdowns = max_lockdowns,
                                        hospital_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000,
                                        income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                        poorer_outcomes = FALSE)

UMIC_time <- UMIC$model_output[, index$time, 1]
UMIC_y <- lapply(seq_along(1:dim(UMIC$model_output)[3]), function(p) {
  apply(UMIC$model_output[, index$n_E2_I, p], 1, sum)
})
UMIC_y <- do.call(cbind, UMIC_y)
UMIC_lockdown <- round(apply(UMIC$time_in_lockdown[1:5500, ], 1, median))
UMIC_z <- data.frame(time = LIC_time, y = UMIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
UMIC_max <- max(UMIC_z$median) * 1.2
c <- ggplot(UMIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = UMIC_lockdown * UMIC_max), alpha = 0.1) +
  geom_line(col = "#362E91", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#362E91") +
  theme_bw() +
  xlab("")

# Running for HIC
income_strata <- "HIC"
trigger_threshold <- 700
country <- "Malta"
raw_death_trigger <- 8/50
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
HIC <- realistic_run_trigger_threshold(country = country, population = pop,
                                        replicates = replicates,
                                        income_strata = income_strata,
                                        initial_trigger_threshold = death_triggers,
                                        trigger_threshold = trigger_threshold,
                                        suppression_reduction = suppression_reduction,
                                        suppression_duration = suppression_duration,
                                        mitigation_reduction = mitigation_reduction,
                                        R0 = R0, tt_R0 = tt_R0,
                                        max_lockdowns = max_lockdowns,
                                        hospital_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000,
                                        income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                                        poorer_outcomes = FALSE)

HIC_time <- HIC$model_output[, index$time, 1]
HIC_y <- lapply(seq_along(1:dim(HIC$model_output)[3]), function(p) {
  apply(HIC$model_output[, index$n_E2_I, p], 1, sum)
})
HIC_y <- do.call(cbind, HIC_y)
HIC_lockdown <- round(apply(HIC$time_in_lockdown[1:5500, ], 1, median))
HIC_z <- data.frame(time = LIC_time, y = HIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
HIC_max <- max(HIC_z$median) * 1.2
d <- ggplot(HIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = HIC_lockdown * HIC_max), alpha = 0.1) +
  geom_line(col = "#241F60", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#241F60") +
  theme_bw() +
  xlab("")


LIC_icu <- (1.5 * 50000000 * 1.25/1000)/100
LMIC_icu <- (2 * 50000000 * 2/1000)/100
UMIC_icu <- (3 * 50000000 * 2.5/1000)/100
HIC_icu <- (3.5 * 50000000 * 4.5/1000)/100
no_constraints_overall <- readRDS("no_constraints_overall_df.rds")
x <- no_constraints_overall %>%
  filter(!(setting == "HIC" & threshold == "ICU_inc5000"),
         !(setting == "UMIC" & threshold == "ICU_inc1200"),
         !(setting == "LMIC" & threshold == "ICU_inc2500"),
         !(setting == "LMIC" & threshold == "ICU_inc700"))

e <- ggplot(x, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  xlim(c(0, 0.65)) +
  geom_point(aes(x = 0.57, y = LIC_icu), colour = "#B7C0EE", size = 5) +
  geom_point(aes(x = 0.5, y = LMIC_icu), colour = "#7067CF", size = 5) +
  geom_point(aes(x = 0.45, y = UMIC_icu), colour = "#362E91", size = 5) +
  geom_point(aes(x = 0.4, y = HIC_icu), colour = "#241F60", size = 5) +
  theme_bw() +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown") +
  guides(colour = "none")


constraints_overall <- readRDS("constraints_overall_df.rds")
y <- constraints_overall %>%
  filter(!(setting == "HIC" & threshold == "ICU_inc2500"),
         !(setting == "HIC" & threshold == "ICU_inc4000"),
         !(setting == "HIC" & threshold == "ICU_inc180"),
         !(setting == "UMIC" & threshold == "ICU_inc1200"),
         !(setting == "UMIC" & threshold == "ICU_inc1500"),
         !(setting == "UMIC" & threshold == "ICU_inc120"),
         !(setting == "UMIC" & threshold == "ICU_inc30"),
         !(setting == "UMIC" & threshold == "ICU_inc250"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc1200"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc500"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc200"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc850"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc100"),
         !(setting == "LMIC_poor" & threshold == "ICU_inc90"),
         !(setting == "LIC_poor" & threshold == "ICU_inc120"),
         !(setting == "LIC_poor" & threshold == "ICU_inc600"),
         !(setting == "LIC_poor" & threshold == "ICU_inc2500"),
         !(setting == "LMIC" & threshold == "ICU_inc1200"),
         !(setting == "LMIC" & threshold == "ICU_inc850"),
         !(setting == "LMIC" & threshold == "ICU_inc200"),
         !(setting == "LMIC" & threshold == "ICU_inc500"),
         !(setting == "LIC" & threshold == "ICU_inc120"),
         !(setting == "LIC" & threshold == "ICU_inc600"))

f <- ggplot(y, aes(x = time_in_lockdown, y = deaths, col = setting)) +
  geom_path(size = 2, aes(linetype = setting)) +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor",
                                 "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#fcb15b", "#B7C0EE", "#FB7171", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  scale_linetype_manual(values = c(5, 1, 5, 1, 1, 1)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  xlim(c(0, 0.65)) +
  labs(y = "Total Number of Deaths", x = "Proportion of Time in Lockdown") +
  guides(linetype = "none", colour = "none")



# Plotting the Output

layout <- "AACCCC
AADDDD
BBEEEE
BBFFFF"

e + f + a + b + c + d +
  plot_layout(design = layout)

