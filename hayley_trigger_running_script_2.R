# Loading Rrequired Libraries
library(tidyverse); library(zoo); library(patchwork)

# Sourcing Required Functions
source("hayley_trigger_running_functions.R")

# Loading in ICU Capacity
load("data/income_strata_healthcare_capacity.rda")

# Run Invariant Parameters
suppression_reduction <- 0.25
suppression_duration <- 30
mitigation_reduction <- 0.66
max_lockdowns <- 15
R0 <- c(2.2, 2.2*0.66, 2.2*0.25, 2.2*0.66)
tt_R0 <- c(0, 27, 68, 98)
replicates <- 5
r <- run_explicit_SEEIR_model("United Kingdom")
index <- squire:::odin_index(r$model)            # get the indices for each of the model outputs

# Running for LIC
income_strata <- "LMIC"
trigger_threshold <- 1
country <- "Senegal"
pop <- get_population(country)
pop <- pop$n
contact_matrix <- squire::get_mixing_matrix("Senegal")
income_strata_healthcare_capacity <- squire::income_strata_healthcare_capacity
time_period <- 550
dt <- 0.05

LIC <- run_trigger_threshold(country = country,
                             population = pop,
                             replicates = replicates,
                             income_strata = income_strata,
                             trigger_threshold = trigger_threshold,
                             suppression_reduction = suppression_reduction,
                             suppression_duration = suppression_duration,
                             mitigation_reduction = mitigation_reduction,
                             R0 = R0,
                             tt_R0 = tt_R0,
                             max_lockdowns = 20,
                             hospital_bed_capacity = 10000000, #hospital_bed_capacity,
                             ICU_bed_capacity = 100000000, #ICU_bed_capacity,
                             income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                             poorer_outcomes = FALSE,
                             time_period = time_period,
                             dt = dt)
LIC_icu <- 35
max_LIC <- get_max_ICU_req(LIC)
time_LIMC <- get_time_in_lockdown(LIC)
LIC_time <- LIC$model_output[, index$time, 1]
LIC_y <- lapply(seq_along(1:dim(LIC$model_output)[3]), function(p) {
  LIC$model_output[, index$ICU_occ, p]
})
LIC_y <- do.call(cbind, LIC_y)
LIC_lockdown <- round(apply(LIC$time_in_lockdown[1:(time_period/dt), ], 1, median))

LIC_z_test <- data.frame(time = LIC_time, y = LIC_y) %>%
  gather(replicate, incidence, -time)
ggplot(LIC_z_test, aes(x = time, y = incidence, col = replicate)) +
  geom_line()

LIC_z <- data.frame(time = LIC_time, y = LIC_y) %>%
  gather(replicate, incidence, -time) %>%
  group_by(time) %>%
  summarise(median = median(incidence),
            lower = quantile(incidence, 0.025),
            upper = quantile(incidence, 0.975))

LIC_max <- LIC_icu
b <- ggplot(LIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = LIC_lockdown * max_LIC), alpha = 0.1) +
  geom_line(col = "#7067CF", size = 1) +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = LIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(x = tt_R0[1]), linetype = "dashed", size = 0.5) +
  geom_line(aes(x = tt_R0[2]), linetype = "dashed", size = 0.5) +
  geom_line(aes(x = tt_R0[3]), linetype = "dashed", size = 0.5) +
  geom_line(aes(x = tt_R0[4]), linetype = "dashed", size = 0.5)
b

