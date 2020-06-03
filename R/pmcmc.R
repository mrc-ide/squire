#' Run a pmcmc sampler with the Squire model setup (i.e. include the various model parameters for the odin model to generate curves)
#'
#' @title Run a Particle MCMC Sampler within the Squire Framework
#'
#' @param data Data to fit to.  This must be constructed with
#'   \code{particle_filter_data}
#' @param squire_model A squire model to use
#' @param pars_obs list of parameters to use in comparison
#'   with \code{compare}. Must be a list containing, e.g.
#'   list(phi_cases = 0.1,
#'        k_cases = 2,
#'        phi_death = 1,
#'        k_death = 2,
#'        exp_noise = 1e6)
#' @param n_mcmc number of mcmc mcmc iterations to perform
#' @param pars_init named list of initial inputs for parameters being sampled
#' @param pars_min named list of lower reflecting boundaries for parameter proposals
#' @param pars_max named list of upper reflecting boundaries for parameter proposals
#' @param proposal_kernel named matrix of proposal covariance for parameters
#' @param pars_discrete named list of logicals, indicating if proposed jump should be discrete
#' @param log_likelihood function to calculate log likelihood, must take named parameter vector as input,
#'                 allow passing of implicit arguments corresponding to the main function arguments.
#'                 Returns a named list, with entries:
#'                   - $log_likelihood, a single numeric
#'                   - $sample_state, a numeric vector corresponding to the state of a single particle, chosen at random,
#'                   at the final time point for which we have data.
#'                   If NULL, calculated using the function calc_loglikelihood.
#' @param log_prior function to calculate log prior, must take named parameter vector as input, returns a single numeric.
#'                  If NULL, uses uninformative priors which do not affect the posterior
#' @param n_particles Number of particles (considered for both the PMCMC fit and sampling from posterior)
#' @param steps_per_day Number of steps per day
#' @param output_proposals Logical indicating whether proposed parameter jumps should be output along with results
#' @param n_chains number of MCMC chains to run
#' @inheritParams calibrate
#' @param date_Meff_change Date when mobility changed corresponding to expected changes in R0 during and after lockdown
#' @param burnin number of iterations to discard from the start of MCMC run when sampling from the posterior for trajectories
#' @param replicates number of trajectories (replicates) to be returned that are being sampled from the posterior probability results produced by \code{run_mcmc_chain}
#' to select parameter set. For each parmater set sampled, run particle filter with \code{n_particles} and sample 1 trajectory
#' @param forecast Number of days to forecast forward. Default = 0
#'
#' @return squire_simulation \describe{
#'   \item{output}{Trajectories from the sampled pMCMC parameter iterations.}
#'   \item{parameters}{Model parameters use for squire}
#'   \item{model}{Squire model used}
#'   \item{inputs}{Inputs into the squire model for the pMCMC.}
#'   \item{pMCMC_results}{An mcmc object generated from \code{pmcmc} and contains:}
#'      \describe{
#'         \item{inputs}{List of inputs}
#'         \item{chains}{List that include}:
#'            \describe{
#'              \item{results}{Matrix of accepted parameter samples, rows = iterations
#'                    as well as log prior, (particle filter estimate of) log likelihood and log posterior}
#'              \item{states}{Matrix of compartment states}
#'              \item{acceptance_rate}{MCMC acceptance rate}
#'              \item{ess}{MCMC chain effective sample size}
#'         }
#'        \item{rhat}{MCMC Diagnostics}
#'      }
#'  \item{interventions}{Contains the interventions that can be called with projections}.
#'  \item{replicate_parameters}{contains the parameter values for the sampled pMCMC parameter iterations
#'   used to generate the \code{squire_model} trajectories}
#'}
#'
#' @description The user inputs initial parameter values for R0, Meff, and the start date
#' The log prior likelihood of these parameters is calculated based on the user-defined
#' prior distributions.
#' The log likelihood of the data given the initial parameters is estimated using a particle filter,
#' which has two functions:
#'      - Firstly, to generate a set of 'n_particles' samples of the model state space,
#'        at time points corresponding to the data, one of which is
#'        selected randomly to serve as the proposed state sequence sample at the final
#'        data time point.
#'      - Secondly, to produce an unbiased estimate of the likelihood of the data given the proposed parameters.
#' The log posterior of the initial parameters given the data is then estimated by adding the log prior and
#' log likelihood estimate.
#'
#' The pMCMC sampler then proceeds as follows, for n_mcmc iterations:
#' At each loop iteration the pMCMC sampler pefsorms three steps:
#'   1. Propose new candidate samples for R0, Meff, Meff_pl, and start_date based on
#'     the current samples, using the proposal distribution
#'     (currently multivariate Gaussian with user-input covariance matrix (proposal_kernel), and reflecting boundaries defined by pars_min, pars_max)
#'   2. Calculate the log prior of the proposed parameters,
#'      Use the particle filter to estimate log likelihood of the data given the proposed parameters, as described above,
#'      as well as proposing a model state space.
#'      Add the log prior and log likelihood estimate to estimate the log posterior of the proposed parameters given the data.
#'   3. Metropolis-Hastings step: The joint canditate sample (consisting of the proposed parameters
#'      and state space) is then accepted with probability min(1, a), where the acceptance ratio is
#'      simply the ratio of the posterior likelihood of the proposed parameters to the posterior likelihood
#'      of the current parameters. Note that by choosing symmetric proposal distributions by including
#'      reflecting boundaries, we avoid the the need to include the proposal likelihood in the MH ratio.
#'
#'   If the proposed parameters and states are accepted then we update the current parameters and states
#'   to match the proposal, otherwise the previous parameters/states are retained for the next iteration.
#'
#' After generating the pMCMC simulation, there are \code{replicates} specific iterations sampled based on the
#' posterior probability. The parameters from those iterations are then used to generate new trajectories within
#' the \code{squire_model} framework.
#'
#' @export
#' @import coda
#' @importFrom stats rnorm
#' @importFrom mvtnorm rmvnorm

pmcmc <- function(data,
                  n_mcmc,
                  log_likelihood = NULL,
                  log_prior = NULL,
                  n_particles = 1e2,
                  steps_per_day = 4,
                  output_proposals = FALSE,
                  n_chains = 1,
                  squire_model = explicit_model(),
                  pars_obs = list(phi_cases = 0.1,
                                  k_cases = 2,
                                  phi_death = 1,
                                  k_death = 2,
                                  exp_noise = 1e6),
                  pars_init = list('start_date'     = as.Date("2020-02-07"),
                                   'R0'             = 2.5,
                                   'Meff'           = 2,
                                   'Meff_pl'        = 3),
                  pars_min = list('start_date'      = as.Date("2020-02-01"),
                                  'R0'              = 0,
                                  'Meff'            = 1,
                                  'Meff_pl'         = 2),
                  pars_max = list('start_date'      = as.Date("2020-02-20"),
                                  'R0'              = 5,
                                  'Meff'            = 3,
                                  'Meff_pl'         = 4),
                  pars_discrete = list('start_date' = TRUE,
                                       'R0'         = FALSE,
                                       'Meff'       = FALSE,
                                       'Meff_pl'    = FALSE),
                  proposal_kernel = NULL,
                  reporting_fraction = 1,
                  country = NULL,
                  population = NULL,
                  contact_matrix_set = NULL,
                  baseline_contact_matrix = NULL,
                  date_contact_matrix_set_change = NULL,
                  date_R0_change = NULL,
                  R0_change = NULL,
                  date_Meff_change = NULL,
                  hosp_bed_capacity = NULL,
                  baseline_hosp_bed_capacity = NULL,
                  date_hosp_bed_capacity_change = NULL,
                  ICU_bed_capacity = NULL,
                  baseline_ICU_bed_capacity = NULL,
                  date_ICU_bed_capacity_change = NULL,
                  burnin = 0,
                  replicates = 100,
                  forecast = 0,
                  required_acceptance_ratio = 0.23,
                  start_covariance_adaptation = round(n_mcmc/2),
                  start_scaling_factor_adaptation = 1
) {

  #............................................................
  # Section 1 of pMCMC Wrapper: Checks & Setup
  #...........................................................
  #..................
  # assertions & checks
  #..................
  # data
  assert_dataframe(data)
  assert_in("date", names(data))
  assert_in("deaths", names(data))
  assert_date(data$dat)
  assert_increasing(as.numeric(as.Date(data$date)),
                    message = "Dates must be in increasing order")

  # check input pars df
  assert_list(pars_init)
  assert_list(pars_min)
  assert_list(pars_max)
  assert_list(pars_discrete)
  assert_eq(names(pars_init), names(pars_min))
  assert_eq(names(pars_min), names(pars_max))
  assert_eq(names(pars_max), names(pars_discrete))
  assert_in(names(pars_init), c("R0", "start_date", "Meff", "Meff_pl"),
            message = "Params to infer must include R0, start_date, and Effective Mobility during and after lockdown")
  assert_date(pars_init$start_date)
  assert_date(pars_min$start_date)
  assert_date(pars_max$start_date)
  if (pars_max$start_date >= as.Date(data$date[1])-1) {
    stop("Maximum start date must be at least 2 days before the first date in data")
  }
  assert_bounded(as.numeric(pars_init$start_date), left = as.numeric(pars_min$start_date), right = as.numeric(pars_max$start_date))

  assert_pos(pars_min$R0)
  assert_pos(pars_max$R0)
  assert_pos(pars_init$R0)
  assert_bounded(pars_init$R0, left = pars_min$R0, right = pars_max$R0)

  assert_pos(pars_min$Meff)
  assert_pos(pars_max$Meff)
  assert_pos(pars_init$Meff)
  assert_bounded(pars_init$Meff, left = pars_min$Meff, right = pars_max$Meff)

  assert_pos(pars_min$Meff_pl)
  assert_pos(pars_max$Meff_pl)
  assert_pos(pars_init$Meff_pl)
  assert_bounded(pars_init$Meff_pl, left = pars_min$Meff_pl, right = pars_max$Meff_pl)

  assert_logical(unlist(pars_discrete))

  # check proposal kernel
  assert_matrix(proposal_kernel)
  assert_eq(colnames(proposal_kernel), names(pars_init))
  assert_eq(rownames(proposal_kernel), names(pars_init))

  # check likelihood items
  if ( !(is.null(log_likelihood) | inherits(log_likelihood, "function")) ) {
    stop("Log Likelihood (log_likelihood) must be null or a user specified function")
  }
  if ( !(is.null(log_prior) | inherits(log_prior, "function")) ) {
    stop("Log Likelihood (log_likelihood) must be null or a user specified function")
  }
  assert_list(pars_obs)
  assert_eq(names(pars_obs), c("phi_cases", "k_cases", "phi_death", "k_death", "exp_noise"))
  assert_numeric(unlist(pars_obs))

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
  #TODO talk to OJ about country vs. explicit model vs. population
  assert_string(country)
  #assert_numeric(population)
  #if (is.null(squire_model)) {
  #  assert_string(country)
  #  assert_numeric(population)
  #}

  # date change items
  assert_same_length(R0_change, date_R0_change)
  # checks that dates are not in the future compared to our data
  if (!is.null(date_R0_change)) {
    assert_date(date_R0_change)
    if(as.Date(tail(date_R0_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_R0_change is greater than the last date in data")
    }
    if (as.Date(pars_max$start_date) >= as.Date(head(date_R0_change, 1))) {
      stop("First date in date_R0_change is earlier than maximum start date allowed in pars search")
    }
  }

  if (!is.null(date_R0_change)) {
    assert_date(date_R0_change)
    if(as.Date(tail(date_R0_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_R0_change is greater than the last date in data")
    }
    if (as.Date(pars_max$start_date) >= as.Date(head(date_R0_change, 1))) {
      stop("First date in date_R0_change is earlier than maximum start date allowed in pars search")
    }
  }

  # catch that if date_R0_change isn't set, Meff isn't doing anything -- not to confuse user
  if (is.null(date_R0_change)) {
    if (pars_min[["Meff"]] != 1 & pars_max[["Meff"]] != 1 & pars_init[["Meff"]] != 1 &
        pars_min[["Meff_pl"]] != 1 & pars_max[["Meff_pl"]] != 1 & pars_init[["Meff_pl"]] != 1) {
      stop("Without an R0 date change, Meff during and post lockdown is not being used. Set Meff during and after lockdow to 1 in all pars lists")
    } else {
      # this slight tweak, so it still works with reflect proposal but won't move since it's not being estimated
      pars_min[["Meff"]] <- pars_min[["Meff"]] - 1e-10
      pars_max[["Meff"]] <- pars_max[["Meff"]] + 1e-10
      pars_discrete[["Meff"]] <- TRUE

      pars_min[["Meff_pl"]] <- pars_min[["Meff_pl"]] - 1e-10
      pars_max[["Meff_pl"]] <- pars_max[["Meff_pl"]] + 1e-10
      pars_discrete[["Meff_pl"]] <- TRUE

    }
  }

  # meff change
  if (is.null(date_Meff_change)) {
    if (pars_min[["Meff_pl"]] != 1 & pars_max[["Meff_pl"]] != 1 & pars_init[["Meff_pl"]] != 1) {
      stop("Without date of Meff change, Meff after lockdown is not being used. Set Meff after lockdow to 1 in all pars lists")
    } else {
      # this slight tweak, so it still works with reflect proposal but won't move since it's not being estimated
      pars_min[["Meff_pl"]] <- pars_min[["Meff_pl"]] - 1e-10
      pars_max[["Meff_pl"]] <- pars_max[["Meff_pl"]] + 1e-10
      pars_discrete[["Meff_pl"]] <- TRUE

    }
  }
  if (!is.null(date_Meff_change)) {
    assert_date(date_Meff_change)
    if(as.Date(date_Meff_change) >= as.Date(tail(date_R0_change, 1))) {
      stop("Meff change date is at or after last date_R0_change")
    }
    if (as.Date(date_Meff_change) <= as.Date(head(date_R0_change, 1))) {
      stop("Meff change date is at or before first date_R0_change")
    }
  }

  if(!is.null(contact_matrix_set)) {
    assert_list(contact_matrix_set)
  }
  assert_same_length(contact_matrix_set, date_contact_matrix_set_change)
  assert_same_length(ICU_bed_capacity, date_ICU_bed_capacity_change)
  assert_same_length(hosp_bed_capacity, date_hosp_bed_capacity_change)

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
    if(as.Date(pars_max$start_date) >= as.Date(head(date_contact_matrix_set_change, 1))) {
      stop("First date in date_contact_matrix_set_change is earlier than maximum start date allowed in pars search")
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
    if(as.Date(pars_max$start_date) >= as.Date(head(date_ICU_bed_capacity_change, 1))) {
      stop("First date in date_ICU_bed_capacity_change is earlier than maximum start date of epidemic")
    }

    tt_ICU_beds <- c(0, seq_len(length(date_ICU_bed_capacity_change)))
    ICU_bed_capacity <- c(baseline_ICU_bed_capacity, ICU_bed_capacity)

  } else {
    tt_ICU_beds <- 0
    ICU_bed_capacity <- baseline_ICU_bed_capacity
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
    if(as.Date(pars_max$start_date) >= as.Date(head(date_hosp_bed_capacity_change, 1))) {
      stop("First date in date_hosp_bed_capacity_change is earlier than maximum start date of epidemic")
    }

    tt_hosp_beds <- c(0, seq_len(length(date_hosp_bed_capacity_change)))
    hosp_bed_capacity <- c(baseline_hosp_bed_capacity, hosp_bed_capacity)

  } else {
    tt_hosp_beds <- 0
    hosp_bed_capacity <- baseline_hosp_bed_capacity
  }

  #..................
  # Generate Odin items
  #..................
  # make the date definitely a date
  data$date <- as.Date(as.character(data$date))
  # adjust for reporting fraction
  data$deaths <- (data$deaths/reporting_fraction)

  # build model parameters
  model_params <- squire_model$parameter_func(country = country,
                                              population = population,
                                              dt = 1/steps_per_day,
                                              contact_matrix_set = contact_matrix_set,
                                              tt_contact_matrix = tt_contact_matrix,
                                              hosp_bed_capacity = hosp_bed_capacity,
                                              tt_hosp_beds = tt_hosp_beds,
                                              ICU_bed_capacity = ICU_bed_capacity,
                                              tt_ICU_beds = tt_ICU_beds)
  # collect interventions for odin model likelihood
  interventions <- list(R0_change = R0_change,
                        date_R0_change = date_R0_change,
                        date_Meff_change = date_Meff_change,
                        date_contact_matrix_set_change = date_contact_matrix_set_change,
                        contact_matrix_set = contact_matrix_set,
                        date_ICU_bed_capacity_change = date_ICU_bed_capacity_change,
                        ICU_bed_capacity = ICU_bed_capacity,
                        date_hosp_bed_capacity_change = date_hosp_bed_capacity_change,
                        hosp_bed_capacity = hosp_bed_capacity)


  #..................
  # Collect Odin and MCMC Inputs
  #..................
  inputs <- list(
    data = data,
    n_mcmc = n_mcmc,
    model_params = model_params,
    interventions = interventions,
    pars_obs = pars_obs,
    squire_model = squire_model,
    pars = list(pars_obs = pars_obs,
                pars_init = pars_init,
                pars_min = pars_min,
                pars_max = pars_max,
                proposal_kernel = proposal_kernel,
                pars_discrete = pars_discrete),
    n_particles = n_particles)



  #..................
  # create prior and likelihood functions given the inputs
  #..................
  if(is.null(log_prior)) {
    # set improper, uninformative prior
    log_prior <- function(pars) log(1e-10)
  }
  calc_lprior <- log_prior

  if(is.null(log_likelihood)) {
    log_likelihood <- calc_loglikelihood
  } else if (!('...' %in% names(formals(log_likelihood)))){
    stop('log_likelihood function must be able to take unnamed arguments')
  }

  # create shorthand function to calc ll given main inputs
  calc_ll <- function(pars) {
    X <- log_likelihood(pars = pars,
                        data = data,
                        squire_model = squire_model,
                        model_params = model_params,
                        interventions = interventions,
                        pars_obs = pars_obs,
                        n_particles = n_particles,
                        forecast_days = 0,
                        return = "ll"
    )
    X
  }

  #..................
  # proposals
  #..................
  # NB, squire set up for date input, but proposal either for MCMC as numeric
  # need to update proposal bounds
  pars_min[["start_date"]] <- -(start_date_to_offset(data$date[1], pars_min[["start_date"]])) # negative here because we are working backwards in time
  pars_max[["start_date"]] <- -(start_date_to_offset(data$date[1], pars_max[["start_date"]]))

  # needs to be a vector to pass to reflecting boundary function
  pars_min <- unlist(pars_min)
  pars_max <- unlist(pars_max)
  pars_discrete <- unlist(pars_discrete)

  # create shorthand function to propose new pars given main inputs
  propose_jump <- function(pars) {
    propose_parameters(pars = pars,
                       proposal_kernel = proposal_kernel,
                       pars_discrete = pars_discrete,
                       pars_min = pars_min,
                       pars_max = pars_max)
  }

  #............................................................
  # Section 2 of pMCMC Wrapper: Run pMCMC
  #...........................................................
  # Run the chains in parallel
  message("Running pMCMC...")
  if (Sys.getenv("SQUIRE_PARALLEL_DEBUG") == "TRUE") {
    chains <- purrr::pmap(
      .l =  list(n_mcmc = rep(n_mcmc, n_chains)),
      .f = run_mcmc_chain,
      inputs = inputs,
      curr_pars = pars_init,
      calc_lprior = calc_lprior,
      calc_ll = calc_ll,
      propose_jump = propose_jump,
      first_data_date = data$date[1],
      output_proposals = output_proposals,
      required_acceptance_ratio = required_acceptance_ratio,
      start_covariance_adaptation = start_covariance_adaptation,
      start_scaling_factor_adaptation)

  } else {
    chains <- furrr::future_pmap(
      .l =  list(n_mcmc = rep(n_mcmc, n_chains)),
      .f = run_mcmc_chain,
      inputs = inputs,
      curr_pars = pars_init,
      calc_lprior = calc_lprior,
      calc_ll = calc_ll,
      propose_jump = propose_jump,
      first_data_date = data$date[1],
      output_proposals = output_proposals,
      required_acceptance_ratio = required_acceptance_ratio,
      start_covariance_adaptation = start_covariance_adaptation,
      start_scaling_factor_adaptation,
      .progress = TRUE)
  }

  #..................
  # MCMC diagnostics and tidy
  #..................
  if (n_chains > 1) {
    names(chains) <- paste0('chain', seq_len(n_chains))

    # calculating rhat
    # convert parallel chains to a coda-friendly format
    chains_coda <- lapply(chains, function(x) {

      traces <- x$results
      if('start_date' %in% names(pars_init)) {
        traces$start_date <- start_date_to_offset(data$date[1], traces$start_date)
      }

      coda::as.mcmc(traces[, names(pars_init)])
    })

    rhat <- tryCatch(expr = {
      x <- coda::gelman.diag(chains_coda)
      x
    }, error = function(e) {
      print('unable to calculate rhat')
    })


    pmcmc <- list(inputs = chains[[1]]$inputs,
                  rhat = rhat,
                  chains = lapply(chains, '[', -1))

    class(pmcmc) <- 'squire_pmcmc_list'
  } else {
    pmcmc <- chains[[1]]
    class(pmcmc) <- "squire_pmcmc"
  }
  #............................................................
  # Section 3 of pMCMC Wrapper: Sample PMCMC Results
  #...........................................................
  pmcmc_samples <- sample_pmcmc(pmcmc_results = pmcmc,
                                burnin = burnin,
                                n_chains = n_chains,
                                n_trajectories = replicates,
                                n_particles = n_particles,
                                forecast_days = forecast)

  #............................................................
  # Section 4 of pMCMC Wrapper: Tidy Output
  #...........................................................
  #..................
  # Pull Sampled results and "recreate" squire models
  #..................
  if (inherits(squire_model, "stochastic")) {

    # create a fake run object and fill in the required elements
    r <- squire_model$run_func(country = country,
                               contact_matrix_set = contact_matrix_set,
                               tt_contact_matrix = tt_contact_matrix,
                               hosp_bed_capacity = hosp_bed_capacity,
                               tt_hosp_beds = tt_hosp_beds,
                               ICU_bed_capacity = ICU_bed_capacity,
                               tt_ICU_beds = tt_ICU_beds,
                               population = population,
                               replicates = 1,
                               time_period = max(tt_contact_matrix,tt_hosp_beds,tt_ICU_beds,1))

    # first let's add our pmcmc results for a nice return
    # we'll save our inputs so it is easy to recreate later
    r$inputs <- pmcmc_samples$inputs
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

  } else if (inherits(squire_model, "deterministic")) {


    r <- list("output" = pmcmc_samples$trajectories)
    # same as above, add in pmcmc items
    r$inputs <- pmcmc_samples$inputs
    r$replicate_parameters <- pmcmc_samples$sampled_PMCMC_Results
    r$pmcmc_results <- pmcmc
    r <- structure(r, class = "squire_simulation")
  }

  # second let's recreate the output
  r$model <- pmcmc_samples$inputs$model$odin_model(
    user = pmcmc_samples$inputs$model_params, unused_user_action = "ignore"
  )
  # we will add the interventions here so that we know what times are needed for projection
  r$interventions <- interventions

  # and fix the replicates
  r$parameters$replicates <- replicates
  r$parameters$time_period <- as.numeric(diff(as.Date(range(rownames(r$output)))))

  #......................
  # out
  #......................
  return(r)
}



# Run a single pMCMC chain
run_mcmc_chain <- function(inputs,
                           curr_pars,
                           calc_lprior,
                           calc_ll,
                           n_mcmc,
                           propose_jump,
                           first_data_date,
                           output_proposals,
                           required_acceptance_ratio,
                           start_covariance_adaptation,
                           start_scaling_factor_adaptation
                           ) {

  #..................
  # Set initial state
  #..................
  # run particle filter on initial parameters
  p_filter_est <- calc_ll(pars = curr_pars)

  # NB, squire originally set up to deal with date format triggering
  # however, proposal and log prior set up to deal with numerics, need to change
  curr_pars[["start_date"]] <- -(start_date_to_offset(first_data_date, curr_pars[["start_date"]]))
  curr_pars <- unlist(curr_pars)

  number_of_parameters <- length(curr_pars) ## CHECK THIS - UNCLEAR WHAT CURR_PARS IS ATM

  ## calculate initial prior
  curr_lprior <- calc_lprior(pars = curr_pars)


  #..................
  # assertions and checks on log_prior and log_likelihood functions
  #..................
  if(length(curr_lprior) > 1) {
    stop('log_prior must return a single numeric representing the log prior')
  }

  if(is.infinite(curr_lprior)) {
    stop('initial parameters are not compatible with supplied prior')
  }

  if(length(p_filter_est) != 2) {
    stop('log_likelihood function must return a list containing elements log_likelihood and sample_state')
  }
  if(!setequal(names(p_filter_est), c('log_likelihood', 'sample_state'))) {
    stop('log_likelihood function must return a list containing elements log_likelihood and sample_state')
  }
  if(length(p_filter_est$log_likelihood) > 1) {
    stop('log_likelihood must be a single numeric representing the estimated log likelihood')
  }
  assert_neg(x = p_filter_est$log_likelihood,
             message1 = 'log_likelihood must be negative or zero')

  if ((start_scaling_factor_adaptation + 100) > start_covariance_adaptation) {
    warning('recommend starting to adapt scaling factor at least 100 iterations before beginning covariance
            matrix adaptation')
  }

  #..................
  # Create objects to store outputs
  #..................
  # extract loglikelihood estimate and sample state
  # calculate posterior
  curr_ll <- p_filter_est$log_likelihood
  curr_lpost <- curr_lprior + curr_ll
  curr_ss <- p_filter_est$sample_state

  # initialise output arrays
  res_init <- c(curr_pars,
                'log_prior' = curr_lprior,
                'log_likelihood' = curr_ll,
                'log_posterior' = curr_lpost)
  res <- matrix(data = NA,
                nrow = n_mcmc + 1L,
                ncol = length(res_init),
                dimnames = list(NULL,
                                names(res_init)))

  states <- matrix(data = NA,
                   nrow = n_mcmc + 1L,
                   ncol = length(curr_ss))

  # New storage arrays for Robbins-Munro related optimisation stuff
  acceptances <- vector(mode = "numeric", length = n_mcmc)
  covariance_matrix_storage <- vector(mode = "list", length = (n_mcmc - start_covariance_adaptation + 1))
  scaling_factor_storage <- vector(mode = "numeric", length = n_mcmc)

  # New parameters related to Robbins Munro optimisation stuff
  scaling_factor <- 0.2 # NEED A BETTER WAY OF INITIALISING THE INITIAL SCALING FACTOR - PERHAPS AS AN ARGUMENT?

  if(output_proposals) {
    proposals <- matrix(data = NA,
                        nrow = n_mcmc + 1L,
                        ncol = length(res_init) + 1L,
                        dimnames = list(NULL,
                                        c(names(res_init),
                                          'accept_prob')))
  }

  ## record initial results
  res[1, ] <- res_init
  states[1, ] <- curr_ss

  #..................
  # main pmcmc loop
  #..................
  for(iter in seq_len(n_mcmc) + 1L) {

    # propose new parameters
    prop_pars <- propose_jump(curr_pars)

    ## calculate proposed prior / lhood / posterior
    prop_lprior <- calc_lprior(pars = prop_pars)
    # NB, squire originally set up to deal with date format triggering --> convert the current parameter start date back to date, whereas proposal and potential log prior function are done in numeric
    prop_pars.squire <- as.list(prop_pars)
    prop_pars.squire[["start_date"]] <- offset_to_start_date(first_data_date, prop_pars[["start_date"]]) # convert to date
    p_filter_est <- calc_ll(pars = prop_pars.squire)
    prop_ll <- p_filter_est$log_likelihood
    prop_ss <- p_filter_est$sample_state
    prop_lpost <- prop_lprior + prop_ll


    # calculate probability of acceptance
    accept_prob <- exp(prop_lpost - curr_lpost)


    if(runif(1) < accept_prob) { # MH step
      # update parameters and calculated likelihoods
      curr_pars <- prop_pars
      curr_lprior <- prop_lprior
      curr_ll <- prop_ll
      curr_lpost <- prop_lpost
      curr_ss <- prop_ss
      acceptances[iter] <- 1
    }

    # record results
    res[iter, ] <- c(curr_pars,
                     curr_lprior,
                     curr_ll,
                     curr_lpost)
    states[iter, ] <- curr_ss

    # adapt and update covariance matrix
    if (iter >= start_covariance_adaptation) {
      if (iter == start_covariance_adaptation) {
        covariance_matrix <- cov(res[1:start_covariance_adaptation, 1:number_of_parameters])
        mean_vector <- apply(res[, 1:number_of_parameters], 2, mean, na.rm = TRUE)
        covariance_matrix_storage[[iter - start_covariance_adaptation + 1]] <- covariance_matrix
      } else {
        tmp <- update_covariance_matrix(covariance_matrix, iter, mean_vector, output, number_of_parameters)
        covariance_matrix <- tmp$new_covariance_matrix
        mean_vector <- tmp$new_mean_vector
        covariance_matrix_storage[[iter - start_covariance_adaptation + 1]] <- covariance_matrix
      }
    }

    # adapt and updat scaling factor
    if (iter > start_scaling_factor_adaptation) {
      scaling_factor <- update_scaling_factor(scaling_factor, acceptances[iter], required_acceptance_ratio, iter, number_of_parameters)
      scaling_factor_storage[iter - start_scaling_factor_adapting + 1] <- scaling_factor
    }

    if(output_proposals) {
      proposals[iter, ] <- c(prop_pars,
                             prop_lprior,
                             prop_ll,
                             prop_lpost,
                             min(accept_prob, 1))
    }

  }

  res <- as.data.frame(res)

  coda_res <- coda::as.mcmc(res)
  rejection_rate <- coda::rejectionRate(coda_res)
  ess <- coda::effectiveSize(coda_res)

  res$start_date <- offset_to_start_date(first_data_date, res$start_date)

  out <- list('inputs' = inputs,
              'results' = as.data.frame(res),
              'states' = states,
              'acceptance_rate' = 1-rejection_rate,
              "ess" = ess,
              "scaling_factor" = scaling_factor_storage,
              "covariance_matrix" = covariance_matrix_storage,
              "acceptance_ratio" = mean(acceptances))

  if(output_proposals) {
    proposals <- as.data.frame(proposals)
    proposals$start_date <- offset_to_start_date(first_data_date, proposals$start_date)
    out$proposals <- proposals
  }

  out

}

# Run odin model to calculate log-likelihood
# return: Set to 'll' to return the log-likelihood (for MCMC) or to
#
calc_loglikelihood <- function(pars, data, squire_model, model_params,
                               pars_obs, n_particles,
                               forecast_days = 0, return = "ll",
                               interventions) {
  #..................
  # specify particle setup
  #..................
  switch(return,
         "full" = {
           save_particles <- TRUE
           full_output <- TRUE
           pf_return <- "sample"
         },
         "ll" = {
           save_particles <- FALSE
           forecast_days <- 0
           full_output <- FALSE
           pf_return <- "single"
         },
         {
           stop("Unknown return type to calc_loglikelihood")
         }
  )

  #..................
  # (potentially redundant) assertion
  #..................
  assert_in(c("R0", "start_date", "Meff", "Meff_pl"), names(pars),
            message = "Must specify R0, start date, and Movement effect size during and after lockdown as parameters to infer")
  #..................
  # unpack current params
  #..................
  R0 <- pars[["R0"]]
  start_date <- pars[["start_date"]]
  Meff <- pars[["Meff"]]
  Meff_pl <- pars[["Meff_pl"]]

  #..................
  # more (potentially redundant) assertions
  #..................
  assert_pos(R0)
  assert_date(start_date)
  assert_pos(Meff)
  assert_pos(Meff_pl)

  #..................
  # setup model based on inputs and interventions
  #..................
  R0_change <- interventions$R0_change
  date_R0_change <- interventions$date_R0_change
  date_Meff_change <- interventions$date_Meff_change
  date_contact_matrix_set_change <- interventions$date_contact_matrix_set_change
  date_ICU_bed_capacity_change <- interventions$date_ICU_bed_capacity_change
  date_hosp_bed_capacity_change <- interventions$date_hosp_bed_capacity_change

  # change betas
  if (is.null(date_R0_change)) {
    tt_beta <- 0
  } else {
    tt_list <- intervention_dates_for_odin(dates = date_R0_change,
                                           change = R0_change,
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt)) # NB, dt and steps_per_day redundant but kept for posterity
    model_params$tt_beta <- unique(c(0, tt_list$tt))
    R0_change <- tt_list$change
  }

  # and contact matrixes
  if (is.null(date_contact_matrix_set_change)) {
    tt_contact_matrix <- 0
  } else {
    tt_list <- intervention_dates_for_odin(dates = date_contact_matrix_set_change,
                                           change = model_params$contact_matrix_set[-1],
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt))
    model_params$tt_contact_matrix <- unique(c(0, tt_list$tt))
    model_params$contact_matrix_set <- append(model_params$contact_matrix_set[1], tt_list$change)
  }
  # and icu beds
  if (is.null(date_ICU_bed_capacity_change)) {
    tt_ICU_beds <- 0
  } else {
    tt_list <- intervention_dates_for_odin(dates = date_ICU_bed_capacity_change,
                                           change = model_params$ICU_beds[-1],
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt))
    model_params$tt_ICU_beds <- unique(c(0, tt_list$tt))
    model_params$ICU_beds <- c(model_params$ICU_beds[1], tt_list$change)
  }
  # and hosp beds
  if (is.null(date_hosp_bed_capacity_change)) {
    tt_hosp_beds <- 0
  } else {
    tt_list <- intervention_dates_for_odin(dates = date_hosp_bed_capacity_change,
                                           change = model_params$hosp_beds[-1],
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt))
    model_params$tt_hosp_beds <- unique(c(0, tt_list$tt))
    model_params$hosp_beds <- c(model_params$hosp_beds[1], tt_list$change)
  }

  #......................
  # update new R0s based on R0_change and R0_date_change, and Meff_date_change
  #......................
  # and now get new R0s for the R0
  if (!is.null(R0_change)) {
    if (is.null(date_Meff_change)) {
      R0 <- c(R0, R0 * R0_change * Meff)
    } else if (!is.null(date_Meff_change)) {
      # when does mobility change take place
      swtchdates <- which(date_R0_change >= date_Meff_change)
      #R0 <- c(R0, R0 * R0_change[1:(min(swtchdates)-1)] * Meff, R0 * R0_change[min(swtchdates):(length(R0_change))] * Meff_pl)
      R0 <- c(R0,
              exp(log(R0) - Meff*(1-R0_change[1:(min(swtchdates)-1)])),
              exp(log(R0) * Meff_pl*(1-R0_change[min(swtchdates):(length(R0_change))]))
              )
    }
  }

  # which allow us to work out our beta
  beta_set <- beta_est(squire_model = squire_model,
                       model_params = model_params,
                       R0 = R0)

  #..................
  # update the model params accordingly from new inputs
  #..................
  model_params$beta_set <- beta_set

  #..................
  # run the particle filter
  #..................
  if (inherits(squire_model, "stochastic")) {

    pf_result <- run_particle_filter(data = data,
                                     squire_model = squire_model,
                                     model_params = model_params,
                                     model_start_date = start_date,
                                     obs_params = pars_obs,
                                     n_particles = n_particles,
                                     forecast_days = forecast_days,
                                     save_particles = save_particles,
                                     full_output = full_output,
                                     return = pf_return)

  } else if (inherits(squire_model, "deterministic")) {

    # a bit of an ugly work-around to keep pMCMC sample state and log-likelihood framework consistent
    # with scan results as well
    # need states for pMCMC
    switch(pf_return,
           "single"={
             pf_result <- list()
             pf_result$log_likelihood <- run_deterministic_comparison(data = data,
                                                                      squire_model = squire_model,
                                                                      model_params = model_params,
                                                                      model_start_date = start_date,
                                                                      obs_params = pars_obs,
                                                                      forecast_days = 0,
                                                                      save_history = FALSE,
                                                                      return = "ll")
             # need states for pMCMC
             pf_result$sample_state <- run_deterministic_comparison(data = data,
                                                                    squire_model = squire_model,
                                                                    model_params = model_params,
                                                                    model_start_date = start_date,
                                                                    obs_params = pars_obs,
                                                                    forecast_days = forecast_days,
                                                                    save_history = TRUE,
                                                                    return = "single")
           },
           "full" = {
             pf_result <- run_deterministic_comparison(data = data,
                                                       squire_model = squire_model,
                                                       model_params = model_params,
                                                       model_start_date = start_date,
                                                       obs_params = pars_obs,
                                                       forecast_days = forecast_days,
                                                       save_history = TRUE,
                                                       return = "full")
           },
           "ll" = {
             pf_result$log_likelihood <- run_deterministic_comparison(data = data,
                                                                      squire_model = squire_model,
                                                                      model_params = model_params,
                                                                      model_start_date = start_date,
                                                                      obs_params = pars_obs,
                                                                      forecast_days = 0,
                                                                      save_history = FALSE,
                                                                      return = "ll")
           },
           "sample" = {
             pf_result <- run_deterministic_comparison(data = data,
                                                       squire_model = squire_model,
                                                       model_params = model_params,
                                                       model_start_date = start_date,
                                                       obs_params = pars_obs,
                                                       forecast_days = forecast_days,
                                                       save_history = TRUE,
                                                       return = "sample")
           }
    )
  }

  # out
  pf_result
}


# proposal for MCMC
propose_parameters <- function(pars, proposal_kernel, pars_discrete, pars_min, pars_max) {

  ## proposed jumps are normal with mean pars and sd as input for parameter
  jumps <- pars + drop(rmvnorm(n = 1,  sigma = proposal_kernel))

  # discretise if necessary
  jumps[pars_discrete] <- round(jumps[pars_discrete])
  # reflect proposal if it exceeds upper or lower parameter boundary
  jumps <- reflect_proposal(x = jumps,
                            floor = pars_min,
                            cap = pars_max)
  jumps
}


## create function to reflect proposal boundaries at pars_min and pars_max
# this ensures the proposal is symetrical and we can simplify the MH step

reflect_proposal <- function(x, floor, cap) {
  interval <- cap - floor
  abs((x + interval - floor) %% (2 * interval) - interval) + floor
}




