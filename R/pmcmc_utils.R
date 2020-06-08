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
                        dimnames = list(max_date_names, NULL, NULL))

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
                model = pmcmc_results$inputs$squire_model,
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
#' Involved in the Robbins-Munro optimisation within the Metropolis-Hastings MCMC.
#' Function to iteratively update the scaling factor of the covariance matrix.
#'
#' @title update_sigma
#' @param scaling_factor the scaling factor involved in the Robbins-Munro optimisation
#' @param acceptance whether or not the most recent parameter proposal was accepted
#' @param probability the required acceptance probability
#' @param i the iteration number
#' @param number_of_parameters the number of parameters currently being estimated
#'
#' @importFrom stats qnorm

update_scaling_factor <- function(scaling_factor, acceptance, probability, i, number_of_parameters) {
  alpha <- -qnorm(probability / 2)
  c <- ((1 - 1 / number_of_parameters) * sqrt(2 * pi) * exp(alpha ^ 2 / 2) / (2 * alpha) + 1 / (number_of_parameters * probability * (1 - probability)))
  theta <- log(sqrt(scaling_factor))
  # theta <- theta + c * (acceptance - probability) / max(200, i / number_of_parameters)
  theta <- theta + c * (acceptance - probability) / max(50, (i / number_of_parameters))
  return(exp(theta)^2)
}


#..............................................................
# Function for updating the covariance matrix
#..............................................................
#' Involved in the Robbins-Munro optimisation within the Metropolis-Hastings MCMC.
#' Function to iteratively update the covariance matrix used to propose new parameter
#' values.
#'
#' @title update_cov
#' @param covariance_matrix the current covariance matrix being used to propose new parameter values
#' @param i the number of iterations since covariance matrix adaptation began
#' @param mean_vector initially this the mean of the columns of the MCMC chain. sequentially updated
#' by this function.
#' @param current_parameters the current parameter values in the MCMC chain
#' @param number_of_parameters the number of parameters currently being estimated

update_covariance_matrix <- function(covariance_matrix, i, mean_vector, current_parameters, number_of_parameters) {
  epsilon = 1 / i
  new_mean = ((mean_vector * i) + current_parameters) / (i + 1)
  new_covariance_matrix = (i - 1) / i * covariance_matrix + mean_vector %*% t(mean_vector) - (i + 1) / i * new_mean %*% t(new_mean) + 1 / i * current_parameters %*% t(current_parameters) + epsilon * diag(number_of_parameters)
  return(list(new_covariance_matrix = new_covariance_matrix, new_mean_vector = new_mean))
}
