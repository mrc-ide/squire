#..............................................................
# Functions for Sampling PMCMC Posterior
#..............................................................
#' Sample from the posterior probability results produced by \code{run_mcmc_chain}
#' to select parameter set. For each parmater set sampled, run particle
#' filter with \code{num_particles} and sample 1 trajectory
#'
#' @title Sample PMCMC
#' @param pmcmc_results output of \code{run_mcmc_chain}; The results from the PMCMC run -- can have mutliple chains.
#' @param burnin integer; Number of iterations to discard from the start of MCMC run. Default = 0
#' @param n_trajectories interger; Number of trajectories to be returned. Integer. Default = 10.
#' @param n_particles integer; Number of particles to be considered in the particle filter. Default = 100
#' @param n_chains number of chains that considered. Should inherent from pmcmc.
#' @param forecast_days integer; number of days being forecast. Default = 0
#'
#' @return \describe{
#'   \item{trajectories}{A 3-dimensional array of trajectories (time, state, tranjectories).}
#'   \item{sampled_PMCMC_Results}{The parameters chosen when sampling from the \code{pmcmc} posteriors}
#'   \item{inputs}{A list of model inputs.}
#'   }
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
    assert_custom_class(pmcmc_results, "squire_pmcmc")
  } else {
    assert_custom_class(pmcmc_results, "squire_pmcmc_list")
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

  # Log Posterior must be negative and nonzero -- the later because the posterior should never be P(1)
  assert_neg(res$log_posterior, zero_allowed = FALSE)

  # convert to probability by exponentiating it
  res <- unique(res)
  probs <- exp(res$log_posterior)
  probs <- probs/sum(probs)

  # occasionally the likelihoods are so low that this creates NAs so just decrease
  drop <- 0.9
  while(any(is.na(probs))) {
    probs <- exp(res$log_posterior*drop)
    probs <- probs/sum(probs)
    drop <- drop^2
  }

  # draw our sample from the unique posterior probaility space
  params_smpl <- sample(x =  length(probs), size = n_trajectories,
                  replace = TRUE, prob = probs)
  params_smpl <- res[params_smpl, !grepl("log", colnames(res))]

  # catch
  assert_in(colnames(params_smpl), c("start_date", "R0", "Meff", "Meff_pl"),
            message = paste0(
            "Currently only allow for the start date, R0, and Meff during and ",
            "after the lockdown to be inferred. All four must be included,",
            "although the Meff parameters can be fixed at 1 (and therefore not inferred)."
            ))

  # put this in format for calc_loglikelihood
  params_smpl$start_date <- offset_to_start_date(pmcmc_results$inputs$data$date[1],
                                                 round(params_smpl$start_date))
  pars.list <- split(params_smpl, 1:nrow(params_smpl))
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
      Rt_func = pmcmc_results$inputs$Rt_func,
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
      Rt_func = pmcmc_results$inputs$Rt_func,
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
                        dimnames = list(max_date_names, colnames(traces[[1]]), NULL))

  # fill the tail of the array slice
  # This is so that the end of the trajectories array is populated,
  # and the start is padded with NA if it's shorter than the max.
  for (i in seq_len(length(traces))){
    trajectories[tail(seq_max, nrow(traces[[i]])), , i] <- traces[[i]]
  }

  # combine and return
  out <- list("trajectories" = trajectories,
              "sampled_PMCMC_Results" = params_smpl,
              inputs = list(
                squire_model = pmcmc_results$inputs$squire_model,
                model_params = pmcmc_results$inputs$model_params,
                interventions = pmcmc_results$inputs$interventions,
                data = pmcmc_results$inputs$data,
                pars_obs = pmcmc_results$inputs$pars_obs))

  class(out) <- "squire_sample_PMCMC"

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

#..............................................................
# Function for updating the scaling factor
#..............................................................
#' Involved in the Johnstone-Change optimisation within the Metropolis-Hastings MCMC.
#' Function to iteratively update the scaling factor and covariance matrix involved
#' in the proposal distribution.
#'
#' @title update_sigma
#' @param accepted whether or not the most recent parameter proposal was accepted
#' @param i the iteration number
#' @param current_sf the current scaling factor
#' @param previous_mu running average of the MCMC parameters
#' @param current_parameters current parameters
#' @param current_covariance_matrix current covariance matrix
#' @param required_acceptance_ratio required acceptance ratio
jc_prop_update <- function(accepted, i, current_sf, previous_mu, current_parameters,
                           current_covariance_matrix, required_acceptance_ratio) {

  cooldown <- (i + 1) ^ -0.6
  new_covariance_matrix <- ((1 - cooldown) * current_covariance_matrix) +
    (cooldown * (t(current_parameters - previous_mu) %*% (current_parameters - previous_mu)))
  new_mu <- ((1 - cooldown) * previous_mu) + (cooldown * current_parameters)
  log_new_scaling_factor <- log(current_sf) + cooldown * (accepted - required_acceptance_ratio)
  new_scaling_factor = exp(log_new_scaling_factor);

  return(list("covariance_matrix" = new_covariance_matrix,
              "mu" = new_mu,
              "scaling_factor" = new_scaling_factor))
}

