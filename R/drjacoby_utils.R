#' @noRd
check_drjacoby_loglike <- function(ll_func) {

   if(!identical(names(formals(ll_func)), c("params", "data", "misc"))) {
     stop("Likelihood function for drjacoby must have following arguments:
          \n 'params', 'data', 'misc'")
   }

}

#' @noRd
check_drjacoby_logprior <- function(ll_func) {

  if(!identical(names(formals(ll_func)), c("params", "misc"))) {
    stop("Log prior function for drjacoby must have following arguments:
          \n 'params',  'misc'")
  }

}

#' @noRd
check_drjacoby_list <- function(drjl) {

  if(any(c("data", "df_params", "misc", "loglike", "logprior", "burnin", "samples") %in% names(drjl))) {
    stop("The following can not be named in drjacoby_list:\n",
         "data, df_params, misc, loglike, logprior, burnin or samples")
  }
  if(length(drjl) > 0){
  if(!all(names(drjl) %in% names(formals(drjacoby::run_mcmc)))) {
    stop("Arguments in drjacoby_list are not found in drjacoby::run_mcmc. Please check drjacoby_list")
  }
  }

}

#' @noRd
calc_loglikelihood_drjacoby <- function(ll_func = calc_loglikelihood) {

  convert_log_likelihood_func_for_drjacoby(ll_func)

}

#' Run a drjacoby mcmc sampler with the Squire model setup
#'
#' @title Run an MCMC Sampler using drjacoby
#'
#' @inheritParams pmcmc
#' @param drjacoby_list List of arguments to pass to [[drjacoby::run_mcmc]] that
#'  are not data, df_params, misc, loglike, logprior, burnin or samples
#'
#' @description The drjacoby mcmc sampler is very similar to [[pmcmc]] but there
#'   are a few subtle differences that meant it was easier to have a separate
#'   function for using drjacoby for the mcmc process
#'
#' @export
#' @import coda
#' @importFrom stats rnorm plogis qnorm cov median
#' @importFrom mvtnorm rmvnorm
#'
drjacoby_mcmc <- function(data,
                  n_mcmc,
                  log_likelihood = NULL,
                  log_prior = NULL,
                  n_particles = 1e2,
                  steps_per_day = 4,
                  output_proposals = FALSE,
                  n_chains = 1,
                  squire_model = explicit_model(),
                  pars_obs = list(phi_cases = 1,
                                  k_cases = 2,
                                  phi_death = 1,
                                  k_death = 2,
                                  exp_noise = 1e6),
                  pars_init = list('start_date'     = as.Date("2020-02-07"),
                                   'R0'             = 2.5,
                                   'Meff'           = 2,
                                   'Meff_pl'        = 3,
                                   "R0_pl_shift"    = 0),
                  pars_min = list('start_date'      = as.Date("2020-02-01"),
                                  'R0'              = 0,
                                  'Meff'            = 1,
                                  'Meff_pl'         = 2,
                                  "R0_pl_shift"     = -2),
                  pars_max = list('start_date'      = as.Date("2020-02-20"),
                                  'R0'              = 5,
                                  'Meff'            = 3,
                                  'Meff_pl'         = 4,
                                  "R0_pl_shift"     = 5),
                  pars_discrete = list('start_date' = TRUE,
                                       'R0'         = FALSE,
                                       'Meff'       = FALSE,
                                       'Meff_pl'    = FALSE,
                                       "R0_pl_shift" = FALSE),
                  reporting_fraction = 1,
                  treated_deaths_only = FALSE,
                  country = NULL,
                  population = NULL,
                  contact_matrix_set = NULL,
                  baseline_contact_matrix = NULL,
                  date_contact_matrix_set_change = NULL,
                  R0_change = NULL,
                  date_R0_change = NULL,
                  hosp_bed_capacity = NULL,
                  baseline_hosp_bed_capacity = NULL,
                  date_hosp_bed_capacity_change = NULL,
                  ICU_bed_capacity = NULL,
                  baseline_ICU_bed_capacity = NULL,
                  date_ICU_bed_capacity_change = NULL,
                  date_vaccine_change = NULL,
                  baseline_max_vaccine = NULL,
                  max_vaccine = NULL,
                  date_vaccine_efficacy_infection_change = NULL,
                  baseline_vaccine_efficacy_infection = NULL,
                  vaccine_efficacy_infection = NULL,
                  date_vaccine_efficacy_disease_change = NULL,
                  baseline_vaccine_efficacy_disease = NULL,
                  vaccine_efficacy_disease = NULL,
                  Rt_args = NULL,
                  burnin = 0,
                  replicates = 100,
                  forecast = 0,
                  drjacoby_list = list(),
                  ...
) {

  #------------------------------------------------------------
  # Section 1 of pMCMC Wrapper: Checks & Setup
  #------------------------------------------------------------

  #--------------------
  # assertions & checks
  #--------------------

  check_drjacoby_list(drjacoby_list)

  # if nimue keep to 1 step per day
  if(inherits(squire_model, "nimue_model")) {
    steps_per_day <- 1
  }

  # we work with pars_init being a list of inital conditions for starting
  if(any(c("start_date", "R0") %in% names(pars_init))) {
    pars_init <- list(pars_init)
  }

  # make it same length as chains, which allows us to pass in multiple starting points
  if(length(pars_init) != n_chains) {
    pars_init <- rep(pars_init, n_chains)
    pars_init <- pars_init[seq_len(n_chains)]
  }

  # data assertions
  assert_dataframe(data)
  assert_in("date", names(data))
  assert_in("deaths", names(data))
  assert_date(data$date)
  assert_increasing(as.numeric(as.Date(data$date)),
                    message = "Dates must be in increasing order")

  # check input pars df
  assert_list(pars_init)
  assert_list(pars_init[[1]])
  assert_list(pars_min)
  assert_list(pars_max)
  assert_list(pars_discrete)
  assert_eq(names(pars_init[[1]]), names(pars_min))
  assert_eq(names(pars_min), names(pars_max))
  assert_eq(names(pars_max), names(pars_discrete))
  assert_in(c("R0", "start_date"),names(pars_init[[1]]),
            message = "Params to infer must include R0, start_date")
  assert_date(pars_init[[1]]$start_date)
  assert_date(pars_min$start_date)
  assert_date(pars_max$start_date)
  if (pars_max$start_date >= as.Date(data$date[1])-1) {
    stop("Maximum start date must be at least 2 days before the first date in data")
  }

  # check date variables are as Date class
  for(i in seq_along(pars_init)) {
    pars_init[[i]]$start_date <- as.Date(pars_init[[i]]$start_date)
  }
  pars_min$start_date <- as.Date(pars_min$start_date)
  pars_max$start_date <- as.Date(pars_max$start_date)

  # check bounds
  for(var in names(pars_init[[1]])) {

    assert_bounded(as.numeric(pars_init[[1]][[var]]),
                   left = as.numeric(pars_min[[var]]),
                   right = as.numeric(pars_max[[var]]),
                   name = paste(var, "init"))

    assert_single_numeric(as.numeric(pars_min[[var]]), name = paste(var, "min"))
    assert_single_numeric(as.numeric(pars_max[[var]]), name = paste(var, "max"))
    assert_single_numeric(as.numeric(pars_init[[1]][[var]]), name = paste(var, "init"))

  }

  # additonal checks that R0 is positive as undefined otherwise
  assert_pos(pars_min$R0)
  assert_pos(pars_max$R0)
  assert_pos(pars_init[[1]]$R0)
  assert_bounded(pars_init[[1]]$R0, left = pars_min$R0, right = pars_max$R0)

  # check likelihood items
  if ( !(is.null(log_likelihood) | inherits(log_likelihood, "function")) ) {
    stop("Log Likelihood (log_likelihood) must be null or a user specified function")
  }
  if ( !(is.null(log_prior) | inherits(log_prior, "function")) ) {
    stop("Log Likelihood (log_likelihood) must be null or a user specified function")
  }
  assert_logical(unlist(pars_discrete))
  assert_list(pars_obs)
  assert_in(c("phi_cases", "k_cases", "phi_death", "k_death", "exp_noise"), names(pars_obs))
  assert_numeric(unlist(pars_obs[c("phi_cases", "k_cases", "phi_death", "k_death", "exp_noise")]))

  # mcmc items
  assert_pos_int(n_mcmc)
  assert_pos_int(n_chains)
  assert_pos_int(n_particles)
  assert_logical(output_proposals)

  # squire and odin
  assert_custom_class(squire_model, "squire_model")
  assert_pos_int(steps_per_day)
  assert_numeric(reporting_fraction)
  assert_bounded(reporting_fraction, 0, 1, inclusive_left = FALSE, inclusive_right = TRUE)
  assert_pos_int(replicates)

  # date change items
  assert_same_length(R0_change, date_R0_change)
  # checks that dates are not in the future compared to our data
  if (!is.null(date_R0_change)) {
    assert_date(date_R0_change)
    if(as.Date(tail(date_R0_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_R0_change is greater than the last date in data")
    }
  }

  # ------------------------------------
  # checks on odin interacting variables
  # ------------------------------------

  if(!is.null(contact_matrix_set)) {
    assert_list(contact_matrix_set)
  }
  assert_same_length(contact_matrix_set, date_contact_matrix_set_change)
  assert_same_length(ICU_bed_capacity, date_ICU_bed_capacity_change)
  assert_same_length(hosp_bed_capacity, date_hosp_bed_capacity_change)
  assert_same_length(max_vaccine, date_vaccine_change)
  assert_same_length(vaccine_efficacy_infection, date_vaccine_efficacy_infection_change)
  assert_same_length(vaccine_efficacy_disease, date_vaccine_efficacy_disease_change)

  # handle contact matrix changes
  if(!is.null(date_contact_matrix_set_change)) {

    assert_date(date_contact_matrix_set_change)
    assert_list(contact_matrix_set)

    if(is.null(baseline_contact_matrix)) {
      stop("baseline_contact_matrix can't be NULL if date_contact_matrix_set_change is provided")
    }
    if(as.Date(tail(date_contact_matrix_set_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_contact_matrix_set_change is greater than the last date in data")
    }

    # Get in correct format
    if(is.matrix(baseline_contact_matrix)) {
      baseline_contact_matrix <- list(baseline_contact_matrix)
    }

    tt_contact_matrix <- c(0, seq_len(length(date_contact_matrix_set_change)))
    contact_matrix_set <- append(baseline_contact_matrix, contact_matrix_set)

  } else {
    tt_contact_matrix <- 0
    contact_matrix_set <- baseline_contact_matrix
  }

  # handle ICU changes
  if(!is.null(date_ICU_bed_capacity_change)) {

    assert_date(date_ICU_bed_capacity_change)
    assert_vector(ICU_bed_capacity)
    assert_numeric(ICU_bed_capacity)

    if(is.null(baseline_ICU_bed_capacity)) {
      stop("baseline_ICU_bed_capacity can't be NULL if date_ICU_bed_capacity_change is provided")
    }
    assert_numeric(baseline_ICU_bed_capacity)
    if(as.Date(tail(date_ICU_bed_capacity_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_ICU_bed_capacity_change is greater than the last date in data")
    }

    tt_ICU_beds <- c(0, seq_len(length(date_ICU_bed_capacity_change)))
    ICU_bed_capacity <- c(baseline_ICU_bed_capacity, ICU_bed_capacity)

  } else {
    tt_ICU_beds <- 0
    ICU_bed_capacity <- baseline_ICU_bed_capacity
  }

  # handle vaccine changes
  if(!is.null(date_vaccine_change)) {

    assert_date(date_vaccine_change)
    assert_vector(max_vaccine)
    assert_numeric(max_vaccine)
    assert_numeric(baseline_max_vaccine)

    if(is.null(baseline_max_vaccine)) {
      stop("baseline_max_vaccine can't be NULL if date_vaccine_change is provided")
    }
    if(as.Date(tail(date_vaccine_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_vaccine_change is greater than the last date in data")
    }

    tt_vaccine <- c(0, seq_len(length(date_vaccine_change)))
    max_vaccine <- c(baseline_max_vaccine, max_vaccine)

  } else {
    tt_vaccine <- 0
    if(!is.null(baseline_max_vaccine)) {
      max_vaccine <- baseline_max_vaccine
    } else {
      max_vaccine <- 0
    }
  }

  # handle vaccine efficacy disease changes
  if(!is.null(date_vaccine_efficacy_infection_change)) {

    assert_date(date_vaccine_efficacy_infection_change)
    if(!is.list(vaccine_efficacy_infection)) {
      vaccine_efficacy_infection <- list(vaccine_efficacy_infection)
    }
    assert_vector(vaccine_efficacy_infection[[1]])
    assert_numeric(vaccine_efficacy_infection[[1]])
    assert_numeric(baseline_vaccine_efficacy_infection)

    if(is.null(baseline_vaccine_efficacy_infection)) {
      stop("baseline_vaccine_efficacy_infection can't be NULL if date_vaccine_efficacy_infection_change is provided")
    }
    if(as.Date(tail(date_vaccine_efficacy_infection_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_vaccine_efficacy_infection_change is greater than the last date in data")
    }

    tt_vaccine_efficacy_infection <- c(0, seq_len(length(date_vaccine_efficacy_infection_change)))
    vaccine_efficacy_infection <- c(list(baseline_vaccine_efficacy_infection), vaccine_efficacy_infection)

  } else {
    tt_vaccine_efficacy_infection <- 0
    if(!is.null(baseline_vaccine_efficacy_infection)) {
      vaccine_efficacy_infection <- baseline_vaccine_efficacy_infection
    } else {
      vaccine_efficacy_infection <- rep(0.8, 17)
    }
  }

  # handle vaccine efficacy disease changes
  if(!is.null(date_vaccine_efficacy_disease_change)) {

    assert_date(date_vaccine_efficacy_disease_change)
    if(!is.list(vaccine_efficacy_disease)) {
      vaccine_efficacy_disease <- list(vaccine_efficacy_disease)
    }
    assert_vector(vaccine_efficacy_disease[[1]])
    assert_numeric(vaccine_efficacy_disease[[1]])
    assert_numeric(baseline_vaccine_efficacy_disease)

    if(is.null(baseline_vaccine_efficacy_disease)) {
      stop("baseline_vaccine_efficacy_disease can't be NULL if date_vaccine_efficacy_disease_change is provided")
    }
    if(as.Date(tail(date_vaccine_efficacy_disease_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_vaccine_efficacy_disease_change is greater than the last date in data")
    }

    tt_vaccine_efficacy_disease <- c(0, seq_len(length(date_vaccine_efficacy_disease_change)))
    vaccine_efficacy_disease <- c(list(baseline_vaccine_efficacy_disease), vaccine_efficacy_disease)

  } else {
    tt_vaccine_efficacy_disease <- 0
    if(!is.null(baseline_vaccine_efficacy_disease)) {
      vaccine_efficacy_disease <- baseline_vaccine_efficacy_disease
    } else {
      vaccine_efficacy_disease <- rep(0.95, 17)
    }
  }


  # handle hosp bed changed
  if(!is.null(date_hosp_bed_capacity_change)) {

    assert_date(date_hosp_bed_capacity_change)
    assert_vector(hosp_bed_capacity)
    assert_numeric(hosp_bed_capacity)

    if(is.null(baseline_hosp_bed_capacity)) {
      stop("baseline_hosp_bed_capacity can't be NULL if date_hosp_bed_capacity_change is provided")
    }
    assert_numeric(baseline_hosp_bed_capacity)
    if(as.Date(tail(date_hosp_bed_capacity_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_hosp_bed_capacity_change is greater than the last date in data")
    }

    tt_hosp_beds <- c(0, seq_len(length(date_hosp_bed_capacity_change)))
    hosp_bed_capacity <- c(baseline_hosp_bed_capacity, hosp_bed_capacity)

  } else {
    tt_hosp_beds <- 0
    hosp_bed_capacity <- baseline_hosp_bed_capacity
  }

  #----------------
  # Generate Odin items
  #----------------

  # make the date definitely a date
  data$date <- as.Date(as.character(data$date))

  # adjust for reporting fraction
  pars_obs$phi_cases <- reporting_fraction
  pars_obs$phi_death <- reporting_fraction
  pars_obs$treated_deaths_only <- treated_deaths_only

  # build model parameters
  model_params <- squire_model$parameter_func(
    country = country,
    population = population,
    dt = 1/steps_per_day,
    contact_matrix_set = contact_matrix_set,
    tt_contact_matrix = tt_contact_matrix,
    hosp_bed_capacity = hosp_bed_capacity,
    tt_hosp_beds = tt_hosp_beds,
    ICU_bed_capacity = ICU_bed_capacity,
    tt_ICU_beds = tt_ICU_beds,
    max_vaccine = max_vaccine,
    tt_vaccine = tt_vaccine,
    vaccine_efficacy_infection = vaccine_efficacy_infection,
    tt_vaccine_efficacy_infection = tt_vaccine_efficacy_infection,
    vaccine_efficacy_disease = vaccine_efficacy_disease,
    tt_vaccine_efficacy_disease = tt_vaccine_efficacy_disease,
    ...)

  # collect interventions for odin model likelihood
  interventions <- list(
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    date_contact_matrix_set_change = date_contact_matrix_set_change,
    contact_matrix_set = contact_matrix_set,
    date_ICU_bed_capacity_change = date_ICU_bed_capacity_change,
    ICU_bed_capacity = ICU_bed_capacity,
    date_hosp_bed_capacity_change = date_hosp_bed_capacity_change,
    hosp_bed_capacity = hosp_bed_capacity,
    date_vaccine_change = date_vaccine_change,
    max_vaccine = max_vaccine,
    date_vaccine_efficacy_disease_change = date_vaccine_efficacy_disease_change,
    vaccine_efficacy_disease = vaccine_efficacy_disease,
    date_vaccine_efficacy_infection_change = date_vaccine_efficacy_infection_change,
    vaccine_efficacy_infection = vaccine_efficacy_infection
  )

  #----------------..
  # Collect Odin and MCMC Inputs
  #----------------..
  inputs <- list(
    data = data,
    n_mcmc = n_mcmc,
    model_params = model_params,
    interventions = interventions,
    pars_obs = pars_obs,
    Rt_args = Rt_args,
    squire_model = squire_model,
    pars = list(pars_obs = pars_obs,
                pars_init = pars_init,
                pars_min = pars_min,
                pars_max = pars_max,
                pars_discrete = pars_discrete),
    n_particles = n_particles)


  #----------------
  # create prior and likelihood functions given the inputs
  #----------------

  if(is.null(log_prior)) {
    # set improper, uninformative prior
    log_prior <- function(params, misc) log(1e-10)
  }
  calc_lprior <- log_prior
  check_drjacoby_logprior(calc_lprior)

  if(is.null(log_likelihood)) {
    log_likelihood <- calc_loglikelihood_drjacoby()
  }
  calc_ll <- log_likelihood
  check_drjacoby_loglike(calc_ll)

  #----------------
  # set run_mcmc_func here to out drjacoby one
  #----------------
  run_mcmc_func <- run_drjacoby_mcmc

  # needs to be a vector to pass to reflecting boundary function
  pars_min <- unlist(pars_min)
  pars_max <- unlist(pars_max)
  pars_discrete <- unlist(pars_discrete)

  #--------------------------------------------------------
  # Section 2 of pMCMC Wrapper: Run pMCMC
  #--------------------------------------------------------

  # Are we debuggine
  if (Sys.getenv("SQUIRE_PARALLEL_DEBUG") == "TRUE") {

    # if debug remove the cluster
    drjacoby_list$cl <- NULL

  }

  # run drjacoby
  message("Running drjacoby...")
  mcmc_out <- run_drjacoby_mcmc(loglike = calc_ll,
                              logprior = calc_lprior,
                              inputs = inputs,
                              burnin = burnin,
                              chains = n_chains,
                              drjacoby_list = drjacoby_list)

  # process output to play with rest of squire
  chains <- convert_drjacoby_mcmc(mcmc_out)
  if(n_chains > 1) {
  pmcmc <- list(inputs = inputs,
                chains = chains,
                drjacoby_out = mcmc_out)

  # drjaciby separates these so add them to align with pmcmc
  pmcmc$inputs$n_mcmc <- burnin + n_chains
  class(pmcmc) <- 'squire_pmcmc_list'
  } else {
    pmcmc <- chains$chain1
    pmcmc$inputs <- inputs
    pmcmc$drjacoby_out <- mcmc_out
    class(pmcmc) <- 'squire_pmcmc'
  }

  #--------------------------------------------------------
  # Section 3 of pMCMC Wrapper: Sample PMCMC Results
  #--------------------------------------------------------
  pmcmc_samples <- sample_drjacoby(pmcmc_results = pmcmc,
                                burnin = burnin,
                                n_chains = n_chains,
                                n_trajectories = replicates,
                                log_likelihood = log_likelihood,
                                n_particles = n_particles,
                                forecast_days = forecast)

  #--------------------------------------------------------
  # Section 4 of pMCMC Wrapper: Tidy Output
  #--------------------------------------------------------

  #----------------
  # Pull Sampled results and "recreate" squire models
  #----------------
  # create a fake run object and fill in the required elements
  r <- squire_model$run_func(country = country,
                             contact_matrix_set = contact_matrix_set,
                             tt_contact_matrix = tt_contact_matrix,
                             hosp_bed_capacity = hosp_bed_capacity,
                             tt_hosp_beds = tt_hosp_beds,
                             ICU_bed_capacity = ICU_bed_capacity,
                             tt_ICU_beds = tt_ICU_beds,
                             max_vaccine = max_vaccine,
                             tt_vaccine = tt_vaccine,
                             vaccine_efficacy_infection = vaccine_efficacy_infection,
                             tt_vaccine_efficacy_infection = tt_vaccine_efficacy_infection,
                             vaccine_efficacy_disease = vaccine_efficacy_disease,
                             tt_vaccine_efficacy_disease = tt_vaccine_efficacy_disease,
                             population = population,
                             replicates = 1,
                             day_return = TRUE,
                             time_period = nrow(pmcmc_samples$trajectories),
                             ...)

  # and add the parameters that changed between each simulation, i.e. posterior draws
  r$replicate_parameters <- pmcmc_samples$sampled_PMCMC_Results

  # as well as adding the pmcmc chains so it's easy to draw from the chains again in the future
  r$pmcmc_results <- pmcmc

  # then let's create the output that we are going to use
  names(pmcmc_samples)[names(pmcmc_samples) == "trajectories"] <- "output"
  dimnames(pmcmc_samples$output) <- list(dimnames(pmcmc_samples$output)[[1]], dimnames(r$output)[[2]], NULL)
  r$output <- pmcmc_samples$output

  # and adjust the time as before
  full_row <- match(0, apply(r$output[,"time",],2,function(x) { sum(is.na(x)) }))
  saved_full <- r$output[,"time",full_row]
  for(i in seq_len(replicates)) {
    na_pos <- which(is.na(r$output[,"time",i]))
    full_to_place <- saved_full - which(rownames(r$output) == as.Date(max(data$date))) + 1L
    if(length(na_pos) > 0) {
      full_to_place[na_pos] <- NA
    }
    r$output[,"time",i] <- full_to_place
  }

  # second let's recreate the output
  r$model <- pmcmc_samples$inputs$squire_model$odin_model(
    user = pmcmc_samples$inputs$model_params, unused_user_action = "ignore"
  )
  # we will add the interventions here so that we know what times are needed for projection
  r$interventions <- interventions

  # and fix the replicates
  r$parameters$replicates <- replicates
  r$parameters$time_period <- as.numeric(diff(as.Date(range(rownames(r$output)))))
  r$parameters$dt <- model_params$dt

  #--------------------..
  # out
  #--------------------..
  return(r)
}


#' Sample from a drjacoby mcmc
#'
#' @title Sample from a drjacoby mcmc
#'
#' @inheritParams sample_pmcmc
#'
#' @description The drjacoby sample is very similar to [[sample_pmcmc]] but there
#'   are a few subtle differences that meant it was easier to have a separate
#'   function for using drjacoby for the mcmc process
#'
#' @export
#' @import coda
#' @importFrom stats rnorm plogis qnorm cov median
#' @importFrom mvtnorm rmvnorm

sample_drjacoby <- function(pmcmc_results,
                         burnin = 0,
                         n_chains,
                         log_likelihood = calc_loglikelihood,
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

  params_smpl$start_date <- offset_to_numeric(pmcmc_results$inputs$data$date[1],
                                                 round(params_smpl$start_date))

  pars.list <- split(params_smpl, 1:nrow(params_smpl))
  names(pars.list) <- rep("pars", length(pars.list))

  #..................
  # run particle filter for trajectories
  #..................

  # get the misc but set the return to full
  misc <- pmcmc_results$drjacoby_out$parameters$misc
  misc$return <- "full"

  message("Sampling from pMCMC Posterior...")

  if (Sys.getenv("SQUIRE_PARALLEL_DEBUG") == "TRUE") {
    traces <- purrr::map(
      .x = pars.list,
      .f = log_likelihood,
      data = pmcmc_results$drjacoby_out$parameters$data,
      misc = misc
    )
  } else {
    traces <- furrr::future_map(
      .x = pars.list,
      .f = log_likelihood,
      data = pmcmc_results$drjacoby_out$parameters$data,
      misc = misc,
      .progress = TRUE,
      .options = furrr::furrr_options(seed = NULL)
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


#' @noRd
run_drjacoby_mcmc <- function(loglike,
                              logprior,
                              inputs,
                              burnin,
                              chains,
                              drjacoby_list
                              ){

  check_drjacoby_loglike(loglike)
  check_drjacoby_logprior(logprior)

  # define parameters dataframe
  df_params <- data.frame(
    name = names(inputs$pars$pars_max),
    min = as.numeric(inputs$pars$pars_min),
    max = as.numeric(inputs$pars$pars_max),
    init = as.numeric(inputs$pars$pars_init[[1]])
  )

  # create data list
  data_list <- list(data = inputs$data)

  # create misc list
  misc <- list(squire_model = inputs$squire_model,
               model_params = inputs$model_params,
               interventions = inputs$interventions,
               pars_obs = inputs$pars$pars_obs,
               n_particles = 2,
               forecast_days = 0,
               Rt_args = inputs$Rt_args,
               return = "ll",
               first_date = inputs$data$date[1])

  args <- list("data" = data_list,
       "df_params" = df_params,
       "loglike" = loglike,
       "logprior" = logprior,
       "burnin" = burnin,
       "samples" = inputs$n_mcmc,
       "misc" = misc,
       "chains" = chains)

  args <- append(args, drjacoby_list)

  mcmc <- do.call(drjacoby::run_mcmc, args)
  mcmc$parameters$misc <- misc
  return(mcmc)


}

#' @noRd
convert_drjacoby_mcmc <- function(mcmc) {

  # convert these dates back into offsets
  mcmc$output$start_date <- start_date_to_offset(
    numeric_to_start_date(mcmc$parameters$data$data$date[1], mcmc$output$start_date, FALSE),
    mcmc$parameters$data$data$date[1]
  )

  names(mcmc$output)[names(mcmc$output) %in% c("logprior", "loglikelihood")] <- c("log_prior", "log_likelihood")
  mcmc$output$log_posterior <- mcmc$output$log_likelihood + mcmc$output$log_prior

  chains <- split(mcmc$output, mcmc$output$chain)
  chains <- lapply(chains, function(x){list("results" = x[,-(1:3)])})
  names(chains) <- paste0("chain", seq_along(chains))

  return(chains)


}

#' @noRd
convert_log_likelihood_func_for_drjacoby <- function(func){

  log_like <- function(params, data, misc) {

    params <- as.list(params)
    params[["start_date"]] <- numeric_to_start_date(misc$first_date, params[["start_date"]])

    ret <- func(
      pars = params,
      data = data$data,
      squire_model = misc$squire_model,
      model_params = misc$model_params,
      pars_obs = misc$pars_obs,
      n_particles = misc$n_particles,
      forecast_days = misc$forecast_days,
      return = misc$return,
      Rt_args = misc$Rt_args,
      interventions = misc$interventions)

    # return
    if(misc$return == "ll") {
      return(ret$log_likelihood)
    } else if (misc$return == "full") {
      return(ret)
    }

  }

  return(log_like)


}

#' @noRd
convert_log_prior_func_for_drjacoby <- function(func){

  log_prior <- function(params, misc) {

    params <- as.list(params)
    params[["start_date"]] <- numeric_to_offset(misc$first_date, params[["start_date"]])

    ret <- func(
      pars = params
    )

    return(ret)

  }

  return(log_prior)

}

