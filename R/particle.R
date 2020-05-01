
#' Create a model, and fit with the particle filter
#'
#' @title Run particle filter
#'
#' @param data to fit to.
#'
#' @param squire_model A squire model to use
#'
#' @param model_params Squire model parameters. Created from a call to one of
##'   the \code{parameters_<type>_model} functions.
#'
#' @param model_start_date Date to run model simulations from
#'
#' @param obs_params List of parameters used for comparing
#'   model to data in the particle filter
#'
#' @param n_particles Number of particles
#'
#' @param forecast_days Days ahead to include in output
#'
#' @param save_particles Whether to save trajectories
#'
#' @param full_output Logical, indicating whether the full model output,
#'   including the state and the declared outputs are returned. Deafult = FALSE
#'
#' @param return Set return depending on what is needed. 'full' gives
#'   the entire particle filter output, 'll' gives the
#'   log-likelihood, 'sample' gives a sampled particle's
#'   trace, 'single' gives the final state
#'
#' @return Results from particle filter
#'
run_particle_filter <- function(data,
                                squire_model,
                                model_params,
                                model_start_date = "2020-02-02",
                                obs_params = list(phi_cases = 0.1,
                                                  k_cases = 2,
                                                  phi_death = 1,
                                                  k_death = 2,
                                                  exp_noise = 1e6),
                                n_particles = 1000,
                                forecast_days = 0,
                                save_particles = FALSE,
                                full_output = FALSE,
                                return = "full") {
  # parameter checks
  if (!(return %in% c("full", "ll", "sample", "single"))) {
    stop("return argument must be full, ll, sample or single")
  }
  if (as.Date(data$date[data$deaths > 0][1], "%Y-%m-%d") < as.Date(model_start_date, "%Y-%m-%d")) {
    stop("Model start date is later than data start date")
  }
  if (!save_particles && return == "sample") {
    message("Must save particles to sample runs")
    save_particles <- TRUE
  }
  if (save_particles && return == "single") {
    stop("Cannot save particles if only returning a single state")
  }

  if (return == "single") {
    save_sample_state <- TRUE
  } else {
    save_sample_state <- FALSE
  }

  #convert data into particle-filter form
  data <- particle_filter_data(data = data,
                               start_date = model_start_date,
                               steps_per_day = round(1 / model_params$dt))

  #set up model
  model_func <- squire_model$odin_model(user = model_params,
                                        unused_user_action = "ignore")

  #set up compare for observation likelihood
  compare_func <- squire_model$compare_model(model = model_func,
                                             pars_obs = obs_params,
                                             data = data)

  pf_results <- particle_filter(data = data,
                                model = model_func,
                                compare = compare_func,
                                n_particles = n_particles,
                                save_particles = save_particles,
                                forecast_days = forecast_days,
                                full_output = full_output,
                                save_sample_state = save_sample_state)

  # Set return type
  # 'full' and 'single' are handled by particle_filter()
  # as long as the right parameters are given
  if (return == "ll") {
    ret <- pf_results$log_likelihood
  } else if (return == "sample") {
    ret <- pf_results$states[, ,sample(n_particles, 1)]
  } else if (return == "single" || return == "full") {
    ret <- pf_results
  }

  ret
}

#' Run a particle filter
#'
#' @title Run a particle filter
#'
#' @param data Data to fit to.  This must be constructed with
#'   \code{particle_filter_data}
#'
#' @param model An odin model, used to generate stochastic samples
#'
#' @param compare A function to generate log-weights
#'
#' @param n_particles Number of particles
#'
#' @param forecast_days Number of days to forecast forward from end
#'   states.  Requires that \code{save_particles} is \code{TRUE}.
#'
#' @param save_particles Logical, indicating if we save full particle
#'   histories (this is slower).
#'
#' @param full_output Logical, indicating whether the full model output,
#'   including the state and the declared outputs are returned. Deafult = FALSE
#'
#' @param save_sample_state Logical, indicating whether we should save a
#'  single particle, chosen at random, at the final time point for which
#'  we have data
#'
#' @param save_end_states Logical, indicating whether we should save all
#'  particles at the final time point for which we have data
#'
particle_filter <- function(data, model, compare, n_particles,
                            forecast_days = 0, save_particles = FALSE,
                            full_output = FALSE,
                            save_sample_state = FALSE, save_end_states = FALSE) {

  if (!inherits(data, "particle_filter_data")) {
    stop("Expected a data set derived from particle_filter_data")
  }
  if (!inherits(model, "odin_model")) {
    stop("Expected 'model' to be an 'odin_model' object")
  }
  if (n_particles < 2) {
    stop("At least two particles required")
  }
  if (forecast_days > 0 && !save_particles) {
    stop("forecasting only possible if particles are saved")
  }
  if (forecast_days < 0) {
    stop("forecast_days must be positive")
  }
  if (save_particles && save_end_states){
    stop("Can not have both save_particles and save_end_states input as TRUE")
  }
  if (full_output) {
    save_particles <- TRUE
  }

  # which indexes are the initials
  i_state <- seq_along(model$initial()) + 1L

  ## ---------------------------------------------------------------------------
  ## Initial Step
  ## ---------------------------------------------------------------------------

  ## Special treatment for the burn-in phase; later we might use this
  ## same approach for skipping steps though.
  if (save_particles) {

    ## Storage for particles depending on full output or not:
    if (full_output) {
      particles <- array(NA_real_,
                         c(max(data$day_end) + 1L + forecast_days,
                           length(model$.__enclos_env__$private$ynames),
                           n_particles))
    } else {
      particles <- array(NA_real_,
                         c(max(data$day_end) + 1L + forecast_days,
                           length(i_state), n_particles))
    }

    # run for the first steps
    step <- seq(data$step_start[[1L]], data$step_end[[1L]], attr(data, "steps_per_day"))
    state_with_history <- model$run(step, use_names = FALSE, replicate = n_particles)

    ## Storage for all particles:
    if (full_output) {
      particles[seq_len(data$day_end[[1]] + 1), , ] <- state_with_history[, , ]
    } else {
      particles[seq_len(data$day_end[[1]] + 1), , ] <- state_with_history[, i_state, ]
    }

    # Grab just the state to continue using
    state <- state_with_history[length(step), i_state, , drop = TRUE]

  } else {
    particles <- NULL
    step <- c(data$step_start[[1L]], data$step_end[[1L]])
    state <- model$run(step = step, use_names = FALSE, replicate = n_particles,
                       return_minimal = TRUE)[, 1, , drop = TRUE]
  }

  ## ---------------------------------------------------------------------------
  ## Particle filter stepping
  ## ---------------------------------------------------------------------------

  log_likelihood <- 0
  for (t in seq_len(nrow(data))[-1L]) {

    # if saving particles we will want each time step
    if (save_particles) {
      step <- seq(data$step_start[t], data$step_end[t], attr(data, "steps_per_day"))
    } else {
      step <- c(data$step_start[t], data$step_end[t])
    }

    # previous state for comparison
    prev_state <- state

    # generate new states
    state <- particle_run_model(state, step, model, save_particles, full_output)

    if (save_particles) {

      ## Storage for all particles:
      if (full_output) {
        # minus first row as return_initial=FALSE doesn't work correctly in dde difeq_replicate
        particles[(data$day_start[t] + 2L):(data$day_end[t] + 1L), , ] <- state[-1 , , ]
        state <- state[length(step),i_state , , drop = TRUE]
      } else {
        particles[(data$day_start[t] + 2L):(data$day_end[t] + 1L), , ] <- state
        state <- state[length(step)-1L, , , drop = TRUE]
      }

    }

    # calculate the weights for this fit
    log_weights <- compare(t, state, prev_state)
    if (!is.null(log_weights)) {
      weights <- scale_log_weights(log_weights)
      log_likelihood <- log_likelihood + weights$average
      if (weights$average == -Inf) {
        ## Everything is impossible, so stop here
        break
      }

      # resample based on the weights
      kappa <- resample(weights$weights, "systematic")
      state <- state[, kappa]
      if (save_particles) {
        particles <- particles[, , kappa]
      }
    }
  }

  ## ---------------------------------------------------------------------------
  ## Forecasting from last state and returns
  ## ---------------------------------------------------------------------------

  # forecast ahead
  if (forecast_days > 0) {

    # step for our forecast
    step <- seq(data$step_end[nrow(data)], length.out = forecast_days + 1L, by = attr(data, "steps_per_day"))

    # run with full return
    state_with_history <- model$run(step, state, replicate = n_particles, use_names = FALSE)

    ## Storage for all particles:
    i <- seq(data$day_end[nrow(data)] + 1, length.out = forecast_days + 1L)
    if (full_output) {
      particles[i, , ] <- state_with_history[, , ]
    } else {
      particles[i, , ] <- state_with_history[, i_state, ]
    }

  }

  # start the return object creation with likelihoods and other return options
  ret <- list(log_likelihood = log_likelihood)
  if (save_particles) {
    date <- data$date[[1]] + seq_len(nrow(particles)) - 1L
    rownames(particles) <- as.character(date)
    attr(particles, "date") <- date
    ret$states <- particles
  }
  if (save_sample_state) {
    particle_chosen <- sample.int(n = n_particles, size = 1)
    ret$sample_state <- state[, particle_chosen]
  }
  if (save_end_states){
    ret$states <- state
  }
  ret
}

#' Compare the model to death data for use with the particle filter
#'
#' @title Compare model to death data
#'
#' @param model An \code{odin_model} object
#'
#' @param pars_obs Parameters for the observations
#'
#' @param data The data to be compared against
#'
#' @param type The class of the model. At the moment this can only be
#'   \code{explicit_SEIR} but as more models come online we can use
#'   this parameter to control the type of comparison function generated.
#'
compare_output <- function(model, pars_obs, data, type="explicit_SEEIR_model") {

  if (type == "simple_SEEIR_model") {
    stop("Particle filter deffault compare function does not work with simple")
  }

  index <- odin_index(model)

  ## Unpack things that we will use repeatedly
  phi_death <- pars_obs$phi_death
  k_death <- pars_obs$k_death
  phi_cases <- pars_obs$phi_cases
  k_cases <- pars_obs$k_cases
  exp_noise <- pars_obs$exp_noise

  # locations of these
  index_cases <- cases_total_index(model) - 1L
  index_D <- c(index$D) - 1L

  force(data)

  ## This returns a closure, with the above variables bound, the
  ## sampler will provide the arguments below.
  function(t, state, prev_state) {

    # for the time being we'll only fit to deaths however uncomment to add cases
    # if (is.na(data$cases[t] && is.na(data$deaths[t]))) {
    if (is.na(is.na(data$deaths[t]))) {
      return(NULL)
    }

    log_weights <- rep(0, ncol(state))

    if (!is.na(data$deaths[t])) {
      ## new deaths summed across ages/infectivities
      model_deaths <- colSums(state[index_D, ]) -
        colSums(prev_state[index_D, ])
      log_weights <- log_weights +
        ll_nbinom(data$deaths[t], model_deaths, phi_death, k_death, exp_noise)
    }

    # We are not going to be bringing cases in so comment this out

    # if (!is.na(data$cases[t])) {
    #   ## new deaths summed across ages/infectivities
    #   model_cases <- colSums(state[index_cases, ]) -
    #     colSums(prev_state[index_D, ])
    #   log_weights <- log_weights +
    #     ll_nbinom(data$deaths[t], model_deaths, phi_death, k_death, exp_noise)
    # }

    log_weights
  }
}



#' Prepare data for use with the particle filter.  This function
#' exists to make explicit how time changes through the model
#' relative to the data and to odin's internal clock.
#' @title Prepare particle filter data
#'
#' @param data A data.frame of observed data.  There must be a column
#'   \code{date}, containing dates (or ISO-formatted strings for
#'   conversion with \code{\link{as.Date}}.
#'
#' @param start_date The date to start the simulation from.  Must be
#'   earlier than the first date in \code{data}.
#'
#' @param steps_per_day The number of steps per day
#'
particle_filter_data <- function(data, start_date, steps_per_day) {
  if (!inherits(data, "data.frame")) {
    stop("Expected a data.frame for 'data'")
  }
  if (!("date" %in% names(data))) {
    stop("Expected a column 'date' within 'data'")
  }
  data$date <- as.Date(data$date)
  if (any(diff(data$date) <= 0)) {
    stop("'date' must be strictly increasing")
  }
  start_date <- as.Date(start_date)
  if (start_date >= as.Date(data$date[1], "%Y-%m-%d")) {
    stop("'start_date' must be less than the first date in data")
  }

  ## Then for each timestep we work out the start and end date
  ret <- data
  ret$day_start <- as.integer(data$date - start_date - 1L)
  ret$day_end <- as.integer(c(ret$day_start[2:nrow(ret)], ret$day_start[nrow(ret)] + 1L))

  d0 <- ret[1, ]
  d0[] <- NA
  d0$date <- start_date
  d0$day_start <- 0
  d0$day_end <- ret$day_start[[1]]
  ret <- rbind(d0, ret)
  rownames(ret) <- NULL

  ret$step_start <- ret$day_start * steps_per_day
  ret$step_end <- ret$day_end * steps_per_day

  class(ret) <- c("particle_filter_data", "data.frame")
  attr(ret, "steps_per_day") <- steps_per_day

  ret
}

#' Prepare dates of intervention for use with odin.  This function
#' exists to make explicit how time changes through the model
#' relative to the data and to odin's internal clock.
#' @title Prepare intervention timing for odin
#'
#' @param dates Dates (or ISO-formatted strings for
#'   conversion with \code{\link{as.Date}} at which R0 changes.
#'
#' @param start_date The date to start the simulation from.  Must be
#'   earlier than the first date in \code{data}.
#'
#' @param steps_per_day The number of steps per day
#'
intervention_dates_for_odin <- function(dates,
                                        start_date,
                                        steps_per_day) {

  ## assertions
  assert_pos_int(steps_per_day)
  assert_date(start_date)
  assert_date(dates)

  # date creations
  start_date <- as.Date(start_date)
  dates <- as.Date(dates)

  # checks on timings
    if (any(start_date >= dates)) {
      stop("'start_date' must be less than the first date in dates")
    }
    if (any(diff(dates) <= 0)) {
      stop("dates must be strictly increasing")
    }

  tt <- round((as.numeric(dates - start_date)) * steps_per_day)
  return(tt)

}

#' @noRd
interventions_unique <- function(df, x = "C") {

  assert_dataframe(df)

  # if it's an empty data frame just retrun NULLs for no intervention
  if(nrow(df) == 0){
    return(list(dates_change = NULL,
                tt = NULL,
                change = NULL))
  } else {
  if (!"date" %in% names(df)) {
    stop("df needs column 'date'")
  }
  if (!x %in% names(df)) {
    stop(sprintf("df has no column %s", x))
  }

  dates_change <- head(df[cumsum(rle(df[[x]])$lengths)+1,]$date, -1)
  change <- head(df[cumsum(rle(df[[x]])$lengths)+1,][[x]], -1)

  return(list(dates_change = as.Date(as.character(dates_change)),
              change = change))
  }
}



#' @noRd
particle_run_model <- function(y, step, model,
                               history = FALSE,
                               full_output = FALSE) {

  # do we need the full output
  if (full_output) {
    return(model$run(step, y, use_names = FALSE, replicate = ncol(y)))
  } else {
    # otherwise is it just the state or do we need all the
    if(!history) {
      model$run(step, y, use_names = FALSE, return_minimal = TRUE,
                replicate = ncol(y))[, 1, , drop = TRUE]
    } else {
      aperm(model$run(step, y, use_names = FALSE,
                      replicate = ncol(y), return_minimal = TRUE),
            c(2, 1, 3))
    }
  }

}

#' @noRd
resample <- function(weights, method) {
  if (method == "multinomial") {
    sample.int(length(weights), replace = TRUE, prob = weights)
  } else if (method == "systematic") {
    systematic_resample(weights)
  }
}

#' @importFrom stats runif
systematic_resample <- function(weights) {
  n <- length(weights)
  u <- runif(1, 0, 1 / n) + seq(0, by = 1 / n, length.out = n)
  cum_weights <- cumsum(weights / sum(weights))
  findInterval(u, cum_weights) + 1L
}

#' @importFrom stats dnbinom rexp
ll_nbinom <- function(data, model, phi, k, exp_noise) {
  mu <- phi * model + rexp(length(model), rate = exp_noise)
  dnbinom(data, k, mu = mu, log = TRUE)
}

#' @importFrom graphics plot points matlines
#' @importFrom stats quantile
plot_particles <- function(particles, ylab) {
  ## Need to set plot up first to get the dates to render on the axis
  ## (matplot does not cope with this)
  dates <- as.Date(rownames(particles))
  plot(dates, particles[, 1], type = "n", ylab = ylab, ylim = range(particles, na.rm = TRUE), xlab = "Date")
  ## Individual traces
  matlines(dates, particles, col="#ff444477", lty = 1)
  ## Quantiles
  quantiles <- t(apply(particles, 1, quantile, c(0.025, 0.5, 0.975)))
  matlines(dates, quantiles, col = "black", lty = "dashed")
}

#' @noRd
scale_log_weights <- function(log_weights) {
  max_log_weights <- max(log_weights)
  if (max_log_weights == -Inf){
    ## if all log_weights at a time-step are -Inf, this should
    ## terminate the particle filter and output the marginal
    ## likelihood estimate as -Inf
    average <- -Inf
    weights <- rep(NaN, length(log_weights))
  } else {
    ## calculation of weights, there is some rescaling here to avoid
    ## issues where exp(log_weights) might give computationally zero
    ## values
    weights <- exp(log_weights - max_log_weights)
    average <- log(mean(weights)) + max_log_weights
  }
  list(weights = weights, average = average)
}
