# Running Multiple Model Replicates, Triggering Suppression Based on ICU Incidence
run_trigger_threshold <- function(country, population,
                                  replicates, suppression_reduction,
                                  trigger_threshold,
                                  suppression_duration, mitigation_reduction, dt = 0.1,
                                  R0 = c(3, 3), tt_R0 = c(0, 50), max_lockdowns = 15) {

  # Defining the Country, Population and Mixing Matrix
  contact_matrix <- get_mixing_matrix(country)
  cpm <- squire:::parse_country_population_mixing_matrix(country = country,
                                                         population = population,
                                                         contact_matrix_set = contact_matrix)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

  # Running the Model
  r <- run_explicit_SEEIR_model(population = population,
                                contact_matrix_set = contact_matrix_set,
                                tt_R0 = tt_R0, R0 = R0, dt = dt,
                                replicates = replicates, time_period = 550,
                                ICU_bed_capacity = 1000000, hosp_bed_capacity = 1000000)

  # Prepping Model Outputs for Iterative Rerunning to Suppression Threshold Triggers
  baseline_beta <- r$model$contents()$beta_set[1]  # get the baseline beta being used
  index <- squire:::odin_index(r$model)            # get the indices for each of the model outputs
  initials <- seq_along(r$model$initial()) + 1L    # defining the indices that comprise the model initials
  out <- r$output                                  # storing the model output (to be sequentially overwritten)
  length_output <- dim(r$output)[1]                # length of the model output
  trigger_times <- rep(1, replicates)              # initialise trigger times
  time_in_lockdown <- matrix(0, nrow = dim(out)[1], # initialise matrix to store time in lockdown
                             ncol = replicates)

  # Running Model Iteratively and Updating Each Time Lockdown is Triggered
  for (i in 1:max_lockdowns) {

    if (i == 1) {
      req <- out[, index$total_number_requiring_IMV, ]
      trigger_times <- lapply(seq_along(trigger_times), function(x){
        daily_ICU_incidence <- rollapply(req[, x], 1/dt, sum, partial = TRUE, align = "right")
        trigger_times <- min(which(daily_ICU_incidence > trigger_threshold))
      })
      trigger_times <- unlist(trigger_times)
      for (k in 1:replicates) {
        if (is.infinite(trigger_times[k])) {
          trigger_times[k] <- length_output
        } else {
          startpoint <- min(trigger_times[k], length_output)
          endpoint <- min(trigger_times[k] + suppression_duration/dt, length_output)
          time_in_lockdown[startpoint:endpoint, k] <- 1
        }
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
    if (sum(trigger_times == length_output) == length(trigger_times)) {
      return(list(model_output = out,
                  time_in_lockdown = time_in_lockdown,
                  index = index))
    }
    print(trigger_times)
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
  }

  # Processing and Storing Outputs
  return(list(model_output = out,
              time_in_lockdown = time_in_lockdown,
              index = index))
}


get_time_in_lockdown <- function(trigger_output) {
  time_in_lockdown <- trigger_output$time_in_lockdown
  replicates <- dim(time_in_lockdown)[2]
  overall_time_in_lockdown <- c()
  for (i in 1:replicates) {
    daily_time_in_lockdown <- rollapply(time_in_lockdown[, i], 10,
                                        median, partial = TRUE, align = "right")
    daily_time_in_lockdown <- daily_time_in_lockdown[seq(1, length(daily_time_in_lockdown), 10)]
    overall_time_in_lockdown[i] <- sum(daily_time_in_lockdown)/length(daily_time_in_lockdown)
  }
  return(mean(overall_time_in_lockdown))
}

get_max_ICU_req <- function(x) {
  index <- x$index
  out <- x$model_output
  replicates <- dim(out)[3]
  max_ICU_occupancy <- c()
  for (i in 1:replicates) {
    daily_ICU_occupancy <- rollapply(out[, index$total_ICU_req, i], 10,
                                     mean, partial = TRUE, align = "right")
    daily_ICU_occupancy <- daily_ICU_occupancy[seq(1, length(daily_ICU_occupancy), 10)]
    max_ICU_occupancy[i] <- max(daily_ICU_occupancy)
  }
  return(mean(max_ICU_occupancy))
}






