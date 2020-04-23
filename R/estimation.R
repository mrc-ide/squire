##' Run a grid search of the particle filter over R0 and start date.
##' This is parallelised, first run \code{plan(multiprocess)} to set
##' this up.
##'
##' @title Grid search of R0 and start date
##'
##' @param R0_min Minimum value of R0 in the search
##'
##' @param R0_max Maximum value of R0 in the search
##'
##' @param R0_step Step to increment R0 between min and max
##'
##' @param first_start_date Earliest start date as 'yyyy-mm-dd'
##'
##' @param last_start_date Latest start date as 'yyyy-mm-dd'
##'
##' @param day_step Step to increment date in days
##'
##' @param data Deaths data to fit to. See \code{example_deaths.csv}
##'   and \code{particle_filter_data()}
##'
##' @param model_params Squire model parameters. Created from a call to one of
##'   the \code{parameters_<type>_model} functions.
##'
##' @param R0_change Numeric vector for relative changes in R0. Default = NULL,
##'   i.e. no change in R0
##'
##' @param date_R0_change Calendar dates at which R0_change occurs.
##'   Defaut = NULL, i.e. no change in R0
##'
##' @param date_contact_matrix_set Calendar dates at which the contact matrices
##'   set in \code{model_params} change. Defaut = NULL, i.e. no change
##'
##' @param squire_model A squire model. Default = \code{explicit_SEIR()}
##'
##' @param pars_obs list of parameters to use for the comparison function.
##'
##' @param n_particles Number of particles. Positive Integer. Default = 100
##'
##' @return List of R0 and start date grid values, and
##'   normalised probabilities at each point
##'
##' @export
##' @import furrr
scan_R0_date <- function(
  R0_min,
  R0_max,
  R0_step,
  first_start_date,
  last_start_date,
  day_step,
  data,
  model_params,
  R0_change = NULL,
  date_R0_change = NULL,
  date_contact_matrix_set = NULL,
  squire_model = explicit_SEIR(),
  pars_obs = NULL,
  n_particles = 100,
  ...) {

  ## Assertions

  assert_custom_class(squire_model, "squire_model")
  assert_custom_class(model_params, "squire_parameters")
  assert_pos(R0_min)
  assert_pos(R0_max)
  assert_date(first_start_date)
  assert_date(last_start_date)
  assert_pos_int(day_step)

  ## Set up parameter space to scan

  R0_1D <- seq(R0_min, R0_max, R0_step)
  date_list <- seq(as.Date(first_start_date), as.Date(last_start_date), day_step)
  param_grid <- expand.grid(R0 = R0_1D, start_date = date_list)

  # Set up observation parameters that translate our model outputs to observations
  if (is.null(pars_obs)) {
    pars_obs <-  list(phi_cases = 0.1,
                                  k_cases = 2,
                                  phi_death = 1,
                                  k_death = 2,
                                  exp_noise = 1e6)
  }

  #
  # Multi-core futures with furrr (parallel purrr)
  #
  ## Particle filter outputs, extracting log-likelihoods
  pf_run_ll <- furrr::future_pmap_dbl(
    .l = param_grid,
    .f = R0_date_particle_filter,
    squire_model = squire_model,
    model_params = model_params,
    data = data,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    date_contact_matrix_set = date_contact_matrix_set,
    pars_obs = pars_obs,
    n_particles = n_particles,
    forecast_days = 0,
    save_particles = FALSE,
    return = "ll"
  )

  ## Construct a matrix with start_date as columns, and beta as rows
  ## order of return is set by order passed to expand.grid, above
  ## Returned column-major (down columns of varying beta) - set byrow = FALSE
  mat_log_ll <- matrix(
    pf_run_ll,
    nrow = length(R0_1D),
    ncol = length(date_list),
    byrow = FALSE)

  # Exponentiate elements and normalise to 1 to get probabilities
  prob_matrix <- exp(mat_log_ll)
  renorm_mat_LL <- prob_matrix/sum(prob_matrix)

  results <- list(x = R0_1D,
                  y = date_list,
                  mat_log_ll = mat_log_ll,
                  renorm_mat_LL = renorm_mat_LL,
                  inputs = list(
                    model = squire_model,
                    model_params = model_params,
                    pars_obs = pars_obs,
                    data = data))

  class(results) <- "squire_scan"
  results
}

##' @export
plot.squire_scan <- function(x, ..., what = "likelihood") {
  if (what == "likelihood") {
    graphics::image(x=x$x, y=x$y, z=x$mat_log_ll,
                    xlab="beta", ylab="Start date", main = "Log-likelihood")
  } else if (what == "probability") {
    graphics::image(x=x$x, y=x$y, z=x$renorm_mat_LL,
                    xlab="beta", ylab="Start date", main = "Probability")
  }
}

##' @export
plot.sample_grid_search <- function(x, ..., what = "ICU") {

  idx <- odin_index(x$inputs$model$odin_model(user = x$inputs$model_params,
                                              unused_user_action = "message"))

  # what are we plotting
  if (what == "ICU") {
    index <- c(idx$I_ICU_D,idx$I_ICU_R) - 1L
    ylab <- "ICU"
    particles <- vapply(seq_len(dim(x$trajectories)[3]), function(y) {
      rowSums(x$trajectories[,index,y], na.rm = TRUE)},
      FUN.VALUE = numeric(dim(x$trajectories)[1]))
    plot_particles(particles, ylab = ylab)
    points(as.Date(x$inputs$data$date), x$inputs$data$itu / x$inputs$pars_obs$phi_ICU, pch = 19)

  } else if (what == "general") {

    index <- c(idx$I_triage,idx$I_hosp_R,idx$I_hosp_D,idx$R_stepdown) - 1L
    ylab <- "General beds"
    particles <- vapply(seq_len(dim(x$trajectories)[3]), function(y) {
      rowSums(x$trajectories[,index,y], na.rm = TRUE)},
      FUN.VALUE = numeric(dim(x$trajectories)[1]))
    plot_particles(particles, ylab = ylab)
    points(as.Date(x$inputs$data$date), x$inputs$data$general / x$inputs$pars_obs$phi_general, pch = 19)

  }

  else if(what == "deaths") {

    index <- c(idx$D) - 1L
    ylab <- "Deaths"
    particles <- vapply(seq_len(dim(x$trajectories)[3]), function(y) {
      out <- c(0,diff(rowSums(x$trajectories[,index,y], na.rm = TRUE)))
      names(out)[1] <- rownames(x$trajectories)[1]
      out},
      FUN.VALUE = numeric(dim(x$trajectories)[1]))
    plot_particles(particles, ylab = ylab)
    points(as.Date(x$inputs$data$date),
           x$inputs$data$deaths/ x$inputs$pars_obs$phi_death, pch = 19)

  } else {

    stop("Requested what must be one of 'ICU', 'deaths' or 'general'")

  }

}

##' Particle filter outputs
##'
##' Helper function to run the particle filter with a
##' new R0 and start date for given interventions.
##'
##' @noRd
R0_date_particle_filter <- function(R0,
                                    start_date,
                                    squire_model,
                                    model_params,
                                    data,
                                    R0_change,
                                    date_R0_change,
                                    date_contact_matrix_set,
                                    pars_obs,
                                    n_particles,
                                    forecast_days = 0,
                                    save_particles = FALSE,
                                    return = "full") {

  # first set up our new timings for the new start date
  if (is.null(date_R0_change)) {
    tt_beta <- 0
  } else {
    tt_beta <- c(0, intervention_dates_for_odin(dates = date_R0_change,
                                           start_date = start_date,
                                           steps_per_day = 1/model_params$dt))
  }

  if (is.null(date_contact_matrix_set)) {
    tt_contact_matrix <- 0
  } else {
    tt_contact_matrix <- c(0, intervention_dates_for_odin(dates = date_contact_matrix_set,
                                                     start_date = start_date,
                                                     steps_per_day = 1/model_params$dt))
  }

  # Second create the new R0s for the R0
  if (!is.null(R0_change)) {
    R0 <- c(R0, R0 * R0_change)
  }

  # and work out our beta
  beta_set <- beta_est(squire_model = squire_model,
                       model_params = model_params,
                       R0 = R0)

  # update the model params accordingly
  model_params$beta_set <- beta_set
  model_params$tt_beta <- tt_beta
  model_params$tt_contact_matrix <- tt_contact_matrix

  # run the particle filter
  X <- run_particle_filter(data = data,
                           squire_model = squire_model,
                           model_params = model_params,
                           model_start_date = start_date,
                           obs_params = pars_obs,
                           n_particles = n_particles,
                           forecast_days = forecast_days,
                           save_particles = save_particles,
                           return = return)

  X
}
