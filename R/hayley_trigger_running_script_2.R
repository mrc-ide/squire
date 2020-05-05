
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
R0 <- c(2.2, 2.2*0.66, 2.2*0.25, 2.2*0.66 )
tt_R0 <- c(0, 27, 68, 152 )
#R0_change <- c(1, 0.66, 0.25, 0.66)
replicates <- 5
hospital_bed_capacity = 407
ICU_bed_capacity = 35


# Running for LIC
income_strata <- "LMIC"
trigger_threshold <- 10
country <- "Senegal"
pop <- get_population(country)
pop <- pop$n
contact_matrix <- squire::get_mixing_matrix("Senegal")

r <- run_explicit_SEEIR_model("United Kingdom")
index <-  squire:::odin_index(r$model)

#r$model$set_user(beta_set = c(4L))

trigger_onwards = 152/0.1

LIC <- run_trigger_threshold(country = country, population = pop,
                             replicates = replicates,
                             income_strata = income_strata,
                             trigger_threshold = trigger_threshold,
                             suppression_reduction = suppression_reduction,
                             suppression_duration = suppression_duration,
                             mitigation_reduction = mitigation_reduction,
                             R0 = R0,
                             tt_R0 = tt_R0,
                            # R0_change = R0_change,
                             max_lockdowns = max_lockdowns,
                             hospital_bed_capacity = hospital_bed_capacity,
                             ICU_bed_capacity = ICU_bed_capacity,
                             income_strata_healthcare_capacity = income_strata_healthcare_capacity,
                             poorer_outcomes = FALSE)

LIC_icu <- 35

max_LIC <- get_max_ICU_req(LIC)
time_LIMC <- get_time_in_lockdown(LIC)
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
b <- ggplot(LIC_z, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = 0, ymax = LIC_lockdown * max_LIC), alpha = 0.1) +
  geom_line(col = "#7067CF", size = 1) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.7, fill = "#7067CF") +
  theme_bw() +
  xlab("") +
  geom_line(aes(y = LIC_icu), linetype = "dashed", size = 0.5) +
  geom_line(aes(y = LIC_icu * 2), linetype = "dashed", size = 0.5)
b
