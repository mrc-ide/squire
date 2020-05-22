#..............................................................
# Functions for Sampling PMCMC Posterior
#..............................................................
#' Sample from the posterior probability results produced by \code{\link{run_mcmc_chain}}
#' to select parameter set. For each parmater set sampled, run particle
#' filter with \code{num_particles} and sample 1 trajectory
#'
#' @title Sample PMCMC
#' @param pmcmc_results output of \code{\link{run_mcmc_chain}}; The results from the PMCMC run -- can have mutliple chains.
#' @param burnin integer; Number of iterations to discard from the start of MCMC run. Default = 0
#' @param n_trajectories interger; Number of trajectories to be returned. Integer. Default = 10.
#' @param n_particles integer; Number of particles to be considered in the particle filter. Default = 100
#' @param forecast_days integer; number of days being forecast. Default = 0
#' @param full_output logical; Indicator for whether the full model output,
#'   including the state and the declared outputs are returned. Deafult = FALSE
#'
#' @return \code{\link{list}}. First element (trajectories) is a 3
#'   dimensional array of trajectories (time, state, tranjectories). Second
#'   element (sampled_PMCMC_Results) is the parameters chosen when sampling from the
#'   \code{pmcmc} and the third dimension (inputs) is a list of
#'   model inputs.
#'
#' @import furrr
#' @importFrom utils tail

sample_pmcmc <- function(pmcmc_results,
                         burnin = 0,
                         n_chains,
                         n_trajectories = 10,
                         n_particles = 100,
                         forecast_days = 0) {
  #..................
  # assertions and checks
  #..................
  assert_pos_int(n_chains)
  if (n_chains == 1) {
    assert_custom_class(pmcmc_results, "pmcmc")
  } else {
    assert_custom_class(pmcmc_results, "pmcmc_list")
  }
  assert_pos_int(burnin)
  assert_pos_int(n_trajectories)
  assert_pos_int(n_particles)
  assert_pos_int(forecast_days)

  #..................
  # sample params based on the log posterior
  #..................
  if (n_chains > 1) {
    res <- create_master_chain(x = pmcmc_results, burn_in = burnin)
  } else if (n_chains == 1 & burnin > 0) {
    res <- pmcmc_results$results[-seq_along(burnin), ]
  } else {
    res <- pmcmc_results$results
  }

  assert_neg(res$log_posterior, zero_allowed = FALSE) # Log Posterior must be negative and nonzero -- the later because the posterior should never be P(1)
  logpos.prob <- 1/abs(res$log_posterior) # convert to probability, larger values -- i.e. less likely values --  have less "weight"
  logpos.prob <- logpos.prob/sum(logpos.prob) # standardize
  # sample rows and then
  if (n_trajectories > nrow(res)) {
    warning("Sampling more trajectories than MCMC iterations. Consider running your MCMC for longer or reducing your burnin")
    params.smpl <- sample(1:nrow(res), size = n_trajectories, prob = logpos.prob, replace = TRUE)
  } else {
    params.smpl <- sample(1:nrow(res), size = n_trajectories, prob = logpos.prob, replace = FALSE)
  }
  params.smpl <- res[params.smpl, !grepl("log", colnames(res))]

  # TODO relax this limitation?
  # catch
  assert_in(colnames(params.smpl), c("start_date", "R0", "Meff"),
            message = "Currently only allow for the start date, R0, and Meff to be inferred. All the three must be included")
  # put this in format for calc_loglikelihood
  pars.list <- split(params.smpl, 1:nrow(params.smpl))
  names(pars.list) <- rep("pars", length(pars.list))

  #..................
  # run particle filter for trajectories
  #..................
  message("Sampling from pMCMC Posterior...")

  if (Sys.getenv("SQUIRE_PARALLEL_DEBUG") == "TRUE") {
    traces <- purrr::map(
      .x = pars.list,
      .f = calc_loglikelihood,
      squire_model = pmcmc_results$inputs$squire_model,
      model_params = pmcmc_results$inputs$model_params,
      interventions = pmcmc_results$inputs$interventions,
      data = pmcmc_results$inputs$data,
      pars_obs = pmcmc_results$inputs$pars_obs,
      n_particles = n_particles,
      forecast_days = forecast_days,
      return = "full"
    )
  } else {
    traces <- furrr::future_map(
      .x = pars.list,
      .f = calc_loglikelihood,
      squire_model = pmcmc_results$inputs$squire_model,
      model_params = pmcmc_results$inputs$model_params,
      interventions = pmcmc_results$inputs$interventions,
      data = pmcmc_results$inputs$data,
      pars_obs = pmcmc_results$inputs$pars_obs,
      n_particles = n_particles,
      forecast_days = forecast_days,
      return = "full",
      .progress = TRUE
    )
  }


  # collapse into an array of trajectories
  # the trajectories are different lengths in terms of dates
  # so we will fill the arrays with NAs where needed
  num_rows <- unlist(lapply(traces, nrow))
  max_rows <- max(num_rows)
  seq_max <- seq_len(max_rows)
  max_date_names <- rownames(traces[[which.max(unlist(lapply(traces, nrow)))]])

  trajectories <- array(NA,
                        dim = c(max_rows, ncol(traces[[1]]), length(traces)),
                        dimnames = list(max_date_names, NULL, NULL))

  # fill the tail of the array slice
  # This is so that the end of the trajectories array is populated,
  # and the start is padded with NA if it's shorter than the max.
  for (i in seq_len(length(traces))){
    trajectories[tail(seq_max, nrow(traces[[i]])), , i] <- traces[[i]]
  }

  # combine and return
  out <- list("trajectories" = trajectories,
              "sampled_PMCMC_Results" = params.smpl,
              inputs = list(
                model = pmcmc_results$inputs$squire_model,
                model_params = pmcmc_results$inputs$model_params,
                interventions = pmcmc_results$inputs$interventions,
                data = pmcmc_results$inputs$data,
                pars_obs = pmcmc_results$inputs$pars_obs))

  class(out) <- "sample_PMCMC"

  return(out)

}


#..............................................................
# Misc Functions for pmcmc
#..............................................................
# Converts dates from data into a numeric offset as used in the MCMC
# Automatically converts type
start_date_to_offset <- function(first_data_date, start_date)
{
  # Format conversion cascades as required
  # string -> Date -> numeric

  # Convert any strings to Dates
  if (class(first_data_date) == "character" || class(first_data_date) == "factor") {
    first_data_date = as.Date(first_data_date)
  }
  if (class(start_date) == "character" || class(first_data_date) == "factor") {
    start_date = as.Date(start_date)
  }

  # Convert any Dates to numerics
  if (class(first_data_date) == "Date") {
    first_data_date = as.numeric(first_data_date)
  }
  if (class(start_date) == "Date") {
    start_date = as.numeric(start_date)
  }

  first_data_date - start_date
}

# Converts dates from  numeric offset as used in the MCMC to a Date
# Automatically converts type
offset_to_start_date <- function(first_data_date, start_date)
{
  if (class(start_date) != "numeric") {
    stop("Offset start date must be numeric")
  }

  # Convert any strings to Dates
  if (class(first_data_date) == "character" || class(first_data_date) == "factor") {
    first_data_date = as.Date(first_data_date)
  }

  as.Date(start_date, origin=first_data_date)
}


