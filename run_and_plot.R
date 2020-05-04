# Loading Rrequired Libraries
library(tidyverse); library(zoo); library(patchwork)

# Sourcing Required Functions
source("trigger_running_function.R")

# Loading in ICU Capacity
load("data/income_strata_healthcare_capacity.rda")
ICU <- income_strata_healthcare_capacity$ICU_beds
LIC_icu <- (ICU[1] * 50000000 /1000) / 2
LMIC_icu <- (ICU[2] * 50000000 /1000) / 2
UMIC_icu <- (ICU[3] * 50000000 /1000) / 2
HIC_icu <- (ICU[4] * 50000000 /1000) / 2

# Run Invariant Parameters
suppression_reduction <- 0.25
suppression_duration <- 30
mitigation_reduction <- 1
max_lockdowns <- 15
R0 <- c(3, 3)
tt_R0 <- c(0, 50)
replicates <- 50
r <- run_explicit_SEEIR_model("United Kingdom")
index <-  squire:::odin_index(r$model)

# Running for LIC
income_strata <- "LIC"
trigger_threshold <- 22
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")

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

max_LIC <- get_max_ICU_req(LIC)
time_LIC <- get_time_in_lockdown(LIC)

LIC_time <- LIC$model_output[, index$time, 1]
LIC_y <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  LIC$model_output[, index$total_ICU_req, p]
})
LIC_y <- do.call(cbind, LIC_y)
LIC_lockdown <- round(apply(LIC$time_in_lockdown[1:5500, ], 1, median))
LIC_z <- data.frame(time = LIC_time, y = LIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
LIC_max <- LIC_icu

a <- ggplot(LIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = LIC_lockdown * LIC_max * 2), alpha = 0.1) +
  geom_line(col = "#B7C0EE", size = 1) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#B7C0EE") +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = LIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = LIC_icu * 2), linetype = "dashed", size = 0.5)


# Running for LMIC
income_strata <- "LMIC"
trigger_threshold <- 39
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

max_LMIC <- get_max_ICU_req(LMIC)
time_LIMC <- get_time_in_lockdown(LMIC)

LMIC_time <- LMIC$model_output[, index$time, 1]
LMIC_y <- lapply(seq_along(1:dim(LMIC$model_output)[3]), function(p) {
  LMIC$model_output[, index$total_ICU_req, p]
})
LMIC_y <- do.call(cbind, LMIC_y)
LMIC_lockdown <- round(apply(LMIC$time_in_lockdown[1:5500, ], 1, median))
LMIC_z <- data.frame(time = LIC_time, y = LMIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
LMIC_max <- LMIC_icu
b <- ggplot(LMIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = LMIC_lockdown * LMIC_max * 2), alpha = 0.1) +
  geom_line(col = "#7067CF", size = 1) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#7067CF") +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = LMIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = LMIC_icu * 2), linetype = "dashed", size = 0.5)


# Running for UMIC
income_strata <- "UMIC"
trigger_threshold <- 100
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

max_UMIC <- get_max_ICU_req(UMIC)
time_UMIC <- get_time_in_lockdown(UMIC)

UMIC_time <- UMIC$model_output[, index$time, 1]
UMIC_y <- lapply(seq_along(1:dim(UMIC$model_output)[3]), function(p) {
  UMIC$model_output[, index$total_ICU_req, p]
})
UMIC_y <- do.call(cbind, UMIC_y)
UMIC_lockdown <- round(apply(UMIC$time_in_lockdown[1:5500, ], 1, median))
UMIC_z <- data.frame(time = LIC_time, y = UMIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
UMIC_max <- UMIC_icu
c <- ggplot(UMIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = UMIC_lockdown * UMIC_max * 2), alpha = 0.1) +
  geom_line(col = "#362E91", size = 1) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#362E91") +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = UMIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = UMIC_icu * 2), linetype = "dashed", size = 0.5)


# Running for HIC
income_strata <- "HIC"
trigger_threshold <- 211
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

max_HIC <- get_max_ICU_req(HIC)
time_HIC <- get_time_in_lockdown(HIC)

HIC_time <- HIC$model_output[, index$time, 1]
HIC_y <- lapply(seq_along(1:dim(HIC$model_output)[3]), function(p) {
  HIC$model_output[, index$total_ICU_req, p]
})
HIC_y <- do.call(cbind, HIC_y)
HIC_lockdown <- round(apply(HIC$time_in_lockdown[1:5500, ], 1, median))
HIC_z <- data.frame(time = HIC_time, y = HIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))
HIC_max <- HIC_icu
d <- ggplot(HIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = HIC_lockdown * HIC_max * 2), alpha = 0.1) +
  geom_line(col = "#241F60", size = 1) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#241F60") +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = HIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = HIC_icu * 2), linetype = "dashed", size = 0.5)


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

e <- ggplot(x, aes(x = time_in_lockdown, y = max_capacity, col = setting)) +
  geom_path(size = 2) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#B7C0EE", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  xlim(c(0, 0.8)) +
  geom_point(aes(x = time_LIC, y = LIC_icu), colour = "#B7C0EE", size = 5) +
  geom_point(aes(x = time_LIMC, y = LMIC_icu), colour = "#7067CF", size = 5) +
  geom_point(aes(x = time_UMIC, y = UMIC_icu), colour = "#362E91", size = 5) +
  geom_point(aes(x = time_HIC, y = HIC_icu), colour = "#241F60", size = 5) +
  theme_bw() +
  labs(y = "Maximum ICU Capacity Required", x = "Proportion of Time in Lockdown") +
  guides(colour = "none")


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

f <- ggplot(y, aes(x = time_in_lockdown, y = 1000 *deaths/50000000, col = setting)) +
  geom_path(size = 2, aes(linetype = setting)) +
  scale_colour_manual(labels = c("Low Income Poor", "Low Income", "Lower Middle Income Poor",
                                 "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#fcb15b", "#B7C0EE", "#FB7171", "#7067CF", "#362E91", "#241F60"),
                      name = "Income Strata") +
  scale_linetype_manual(values = c(5, 1, 5, 1, 1, 1)) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  xlim(c(0, 0.8)) +
  labs(y = "Deaths Per 1000 Population", x = "Proportion of Time in Lockdown") +
  guides(linetype = "none", colour = "none")


# Plotting the Output
layout <- "AACCCC
AADDDD
BBEEEE
BBFFFF"

e + f + a + b + c + d +
  plot_layout(design = layout)

# Running for LIC but for the initial illustrative plot
income_strata <- "LIC"
trigger_threshold <- 30
country <- "Madagascar"
raw_death_trigger <- 0
death_triggers <- round(50 * raw_death_trigger)
pop <- get_population(country)
pop <- (50000000/sum(pop$n)) * pop$n
contact_matrix <- squire::get_mixing_matrix("Madagascar")
suppression_reduction <- c(0.25, 0.15, 0.05)

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

mid <- process_output(med_LIC, index)
mid_time <- round(apply(med_LIC$time_in_lockdown[1:5500, ], 1, median))
mid$scenario <- "mid"

high <- process_output(high_LIC, index)
high_time <- round(apply(high_LIC$time_in_lockdown[1:5500, ], 1, median))
high$scenario <- "high"

first_mid <- mid_time
first_mid[800:5500] <- 0
first_high <- high_time
first_high[800:5500] <- 0
first_lockdown <- c(first_mid, first_high)

overall <- rbind(mid, high) %>%
  cbind(lockdown = first_lockdown) %>%
  filter(time > 25 & time < 120)

g <- ggplot(overall, aes(x = time, y = median, colour = scenario)) +
  geom_ribbon(aes(ymin = rep(0, length(lockdown)), ymax = lockdown * max(overall$upper)),
              fill = "grey", alpha = 0.2, colour = NA) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("#95A78D", "#C8C6AF")) +
  scale_fill_manual(values = c("#95A78D", "#C8C6AF")) +
  geom_line(aes(x = 94, y = lockdown * max(overall$upper)),
            size = 1, colour = "#C8C6AF") +
  geom_line(aes(x = 109, y = lockdown * max(overall$upper)),
            size = 1, colour = "#95A78D") +
  theme_bw() +
  theme(legend.position = "none")

# Illustrative Plot Part 2
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

low_thresh <- process_output(low_thresh_LIC, index)
low_thresh$scenario <- "low"
low_thresh_time <- round(apply(low_thresh_LIC$time_in_lockdown[1:5500, ], 1, median))

high_thresh <- process_output(high_thresh_LIC, index)
high_thresh$scenario <- "high"
high_thresh_time <- round(apply(high_thresh_LIC$time_in_lockdown[1:5500, ], 1, median))

h <- ggplot(low_thresh, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#C884A5", alpha = 0.2, colour = NA) +
  geom_line(colour = "#C884A5") +
  geom_ribbon(aes(ymin = 0, ymax = low_thresh_time * 600), alpha = 0.1) +
  labs(x = "") +
  theme_bw()

i <- ggplot(high_thresh, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = high_thresh_time * 7800), alpha = 0.1) +
  geom_line(colour = "#EB55A2") +
  labs(x = "") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#EB55A2", alpha = 0.2, colour = NA) +
  theme_bw()

layout <- "AAABBB
           AAACCC
           DDFFFF
           DDFFFF
           DDGGGG
           DDGGGG
           EEHHHH
           EEHHHH
           EEIIII
           EEIIII"

g + h + i + e + f + a + b + c + d +
  plot_layout(design = layout)


