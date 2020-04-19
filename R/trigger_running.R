#' Trigger run
#'
#' @param trigger_type Type of trigger (deaths, ICU capacity)
#' @param trigger_threshold Number of observed deaths suppression to be triggered at
#' @param ... Further aguments for \code{run_explicit_SEEIR_model()}
#' @inheritParams run_explicit_SEEIR_model
#'
#' @export
#' @return List of time adjusted squire_simulations
trigger_run <- function(trigger_type = NULL,
                        trigger_threshold = NULL,
                        suppression_reduction = 0.3,
                        suppression_duration = 30,
                        replicates = 25,
                        country = NULL,
                        population = NULL,
                        contact_matrix_set = NULL,
                        dt = 0.1,
                        R0 = c(3, 3),
                        tt_R0 = c(0, 50),
                        ...) {

  # argument checks
  assert_character(trigger_type)
  assert_numeric(trigger_threshold)
  assert_numeric(replicates)

  # return errors if trigger parameters not specified
  if (is.null(trigger_type)) {
    stop("Argument triger_type not specified: choose one of deaths or ICU_capacity")
  }
  if(is.null(trigger_threshold)) {
    stop("Argument trigger_threshold not specified.")
  }

  # Handle country population args
  cpm <- parse_country_population_mixing_matrix(country = country,
                                                population = population,
                                                contact_matrix_set = contact_matrix_set)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

  # run model with fixed day step (to match up with daily deaths)
  r <- run_explicit_SEEIR_model(population = population,
                                contact_matrix_set = contact_matrix_set,
                                replicates = 10,
                                ...)

  # get the index for looking up ICU requirements and deaths
  index <- odin_index(r$model)
  out <- r$output

  for (i in 1:5) {
    if (trigger_type == "ICU_capacity") {
      req <- out[, index$total_ICU_req, ]
      timings <- apply(req, 2, function(x){
        trigger_times <- min(which(x > trigger_threshold))
      })
    } else if (trigger_type == "deaths") {
      req <- out[, index$total_deaths, ]
      timings <- apply(req, 2, function(x){
        trigger_times <- min(which(x > trigger_threshold))
      })
    } else {
      stop("trigger_type not one of ICU capacity or deaths")
    }
    beta <- r$model$contents()$beta_set[1]
    for(j in 1:replicates) {
      r$model$set_user(beta_set = c(beta * suppression_reduction, beta))
      r$model$set_user(tt_beta = c(timings[j] * dt, timings[j] * dt + suppression_duration))
      length_output <- dim(r$output)[1]
      r$output <- r$model$run(timings[j]:length_output, replicate = 1)
      out[timings[i]:length_output, , j] <- r$output
    }
  }










  # run our replicates
  t <- seq(from = 1, to = r$parameters$time_period / r$parameters$dt)
  nt <- length(t)
  out <- list()
  out[[1]] <- r
  # running and storing the model output for each of the different initial seeding cases
  for(i in 2:replicates) {
    r$model$set_user(E1_0 = E1_0[[i]])
    beta[1] <- beta_est_explicit(dur_IMild = r$parameters$dur_IMild,
                                 dur_ICase = r$parameters$dur_ICase,
                                 prob_hosp = r$parameters$prob_hosp,
                                 mixing_matrix =  process_contact_matrix_scaled_age(r$parameters$contact_matrix_set[[1]], r$parameters$population),
                                 R0 = R0_scan[i])
    r$model$set_user(beta_set = beta)
    r$output <- r$model$run(t, replicate = 1)
    while (sum(r$output[nt, index$R, 1]) < (sum(r$parameters$population)/20)) {
      r$output <- r$model$run(t, replicate = 1)
    }
    out[[i]] <- r
  }

# Get deaths timepoint
deaths_sim <- lapply(out, format_output, var_select = "D")
times <- sapply(deaths_sim, function(x){
  x$t[x$y >= true_deaths][1]
})

# Adjust time
for(i in 1:length(out)){
  out[[i]]$output[,"time",] <- out[[i]]$output[,"time",] - times[i]
}

outarray <- array(NA, dim = c(nrow(out[[1]]$output), ncol(out[[1]]$output), replicates))
for(i in 1:length(out)){
  outarray[,,i] <- out[[i]]$output
}
colnames(outarray) <- names(r$output[1,,1])
r$output <- outarray
r$parameters$replicates <- replicates
r$parameters$R0_scan <- R0_scan

return(r)
}


## Index locations of outputs in odin model
#' @noRd
odin_index <- function(model) {
  n_out <- environment(model$initialize)$private$n_out %||% 0
  n_state <- length(model$initial())
  model$transform_variables(seq_len(1L + n_state + n_out))
}

## Take odin state and calculate sum across ages in a replicate and vectorise
#' @noRd
odin_sv <- function(state, replicates, nt, reduce_age = TRUE) {
  if (reduce_age) {
    as.numeric(vapply(seq_len(replicates), function(x) {
      rowSums(state[,,x])
    }, FUN.VALUE = double(nt)))
  } else { # note: whole age-group results for single replicate produced, then next age-group etc
    as.numeric(vapply(seq_len(replicates), function(x) {
      state[, , x]
    }, FUN.VALUE = rep(double(nt), dim(state)[2])))
  }
}

## Final time varying variables at t = 0 in calibrate
#' @noRd
t0_variables <- function(r) {

  dims <- dim(r$output)

  # what state time point do we want
  state_pos <- vapply(seq_len(dims[3]), function(x) {
    which(r$output[,"time",x] == 0)
  }, FUN.VALUE = numeric(1))

  lapply(seq_len(dims[3]), function(i) {

    last <- tail(which(r$parameters$tt_R0 < state_pos[i]), 1)
    R0 <- r$parameters$R0[last]

    last <- tail(which(r$parameters$tt_contact_matrix < state_pos[i]), 1)
    contact_matrix_set <- r$parameters$contact_matrix_set[last]

    last <- tail(which(r$parameters$tt_hosp_beds < state_pos[i]), 1)
    hosp_bed_capacity <- r$parameters$hosp_bed_capacity[last]

    last <- tail(which(r$parameters$tt_ICU_beds < state_pos[i]), 1)
    ICU_bed_capacity <- r$parameters$ICU_bed_capacity[last]

    return(list(
      R0 = R0,
      contact_matrix_set = contact_matrix_set,
      hosp_bed_capacity = hosp_bed_capacity,
      ICU_bed_capacity = ICU_bed_capacity
    ))

  })

}
