# Load required libraries
library(tidyverse); library(zoo)

# Defining relevant parameters
dt <- 0.1
R0 = c(3, 3)
tt_R0 <- c(0, 50)

# Get population and mixing matrix
country <- "Malawi"
cpm <- squire:::parse_country_population_mixing_matrix(country = country, population = NULL,
                                                       contact_matrix_set = NULL)
country <- cpm$country
population <- cpm$population
contact_matrix_set <- cpm$contact_matrix_set

# Run model in full once
replicates <- 10
r <- run_explicit_SEEIR_model(country = "Malawi")
ICU_capacity <- r$parameters$ICU_bed_capacity
r <- run_explicit_SEEIR_model(population = population,
                              contact_matrix_set = contact_matrix_set,
                              tt_R0 = tt_R0, R0 = R0, dt = dt,
                              replicates = replicates, time_period = 550,
                              ICU_bed_capacity = 1000000, hosp_bed_capacity = 1000000)

# Get the index for looking up ICU requirements and deaths
baseline_beta <- r$model$contents()$beta_set[1]
index <- squire:::odin_index(r$model)
initials <- seq_along(r$model$initial()) + 1L
out <- r$output
length_output <- dim(r$output)[1]
trigger_times <- rep(1, replicates)

# Running
trigger_threshold <- 100
suppression_reduction <- 0.15
suppression_duration <- 30
mitigation_reduction <- 1
time_in_lockdown <- matrix(0, nrow = dim(out)[1], ncol = replicates)
for (i in 1:12) {

  if (i == 1) {
    req <- out[, index$total_number_requiring_IMV, ]
    trigger_times <- lapply(seq_along(trigger_times), function(x){
      daily_ICU_incidence <- rollapply(req[, x], 1/dt, sum, partial = TRUE, align = "right")
      trigger_times <- min(which(daily_ICU_incidence > trigger_threshold))
    })
    trigger_times <- unlist(trigger_times)
    for (k in 1:replicates) {
      startpoint <- min(trigger_times[k], length_output)
      endpoint <- min(trigger_times[k] + suppression_duration/dt, length_output)
      time_in_lockdown[startpoint:endpoint, k] <- 1
    }
  } else {
    req <- out[, index$total_number_requiring_IMV, ]
    trigger_times <- lapply(seq_along(trigger_times), function(x){
      daily_ICU_incidence <- rollapply(req[, x], 1/dt, sum, partial = TRUE, align = "right")
      starting_point <- trigger_times[x] + suppression_duration/dt
      timing_index <- min(length_output, starting_point)
      if (timing_index == length_output) {
        timing_index
      } else {
        temp <- timing_index + min(which(daily_ICU_incidence[timing_index:length_output] > trigger_threshold))
        if (is.infinite(temp)) {
          timing_index <- length_output
          timing_index
        } else {
          timing_index <- temp
          timing_index
        }
      }
    })
    trigger_times <- unlist(trigger_times)
    for (k in 1:replicates) {
      if (trigger_times[k] == length_output) {
        time_in_lockdown[trigger_times[k], k] <- 0
      } else {
        startpoint <- min(trigger_times[k], length_output)
        endpoint <- min(trigger_times[k] + suppression_duration/dt, length_output)
        time_in_lockdown[startpoint:endpoint, k] <- 1
      }
    }
  }

  print(trigger_times)
  if (sum(trigger_times == length_output) == length(trigger_times)) {
    return(out)
  }

  for(j in 1:replicates) {
    if (trigger_times[j] == length_output) {

    } else {
      r$model$set_user(beta_set = c(baseline_beta * suppression_reduction, baseline_beta * mitigation_reduction))
      r$model$set_user(tt_beta = c(trigger_times[j], trigger_times[j] + suppression_duration/dt))
      out[trigger_times[j]:length_output, , j] <- r$model$run(step = trigger_times[j]:length_output,
                                                              replicate = 1,
                                                              y = as.numeric(out[trigger_times[j], initials, j]))
    }
  }
  print(i)
}

daily_time_in_lockdown <- rollapply(time_in_lockdown[, 1], 1/dt, median, partial = TRUE, align = "right")
daily_time_in_lockdown <- daily_time_in_lockdown[seq(1, length(daily_time_in_lockdown), 1/dt)]

daily_ICU_incidence <- rollapply(out[, index$total_number_requiring_IMV, 1], 1/dt, sum, partial = TRUE, align = "right")
daily_ICU_incidence <- daily_ICU_incidence[seq(1, length(daily_ICU_incidence), 1/dt)]

plot(daily_ICU_incidence, type = "l")
lines(daily_time_in_lockdown * trigger_threshold * 0.85)
lines(rep(trigger_threshold, length(daily_ICU_incidence)), col = "red", lwd = 2)

daily_ICU_occupancy <- rollapply(out[, index$total_ICU_req, 1], 1/dt, mean, partial = TRUE, align = "right")
daily_ICU_occupancy <- daily_ICU_occupancy[seq(1, length(daily_ICU_occupancy), 1/dt)]

plot(daily_ICU_occupancy, type = "l")
lines(daily_time_in_lockdown * max(daily_ICU_occupancy)/2)
lines(rep(ICU_capacity, length(daily_ICU_occupancy)), col = "red", lwd = 2)
max(daily_ICU_occupancy)
sum(daily_time_in_lockdown)/length(daily_time_in_lockdown)
