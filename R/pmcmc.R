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
#' @param Rt_args List of arguments to be passed to \code{evaluate_Rt_pmcmc} for calculating Rt.
#'   Current arguments are available in \code{Rt_args_list}
#' @param Rt_modify_func Function to modify our Rt after evaluate_Rt_pmcmc. Must take
#'   R0 as vector, pars and Rt_args.
#' @param burnin number of iterations to discard from the start of MCMC run when sampling from the posterior for trajectories
#' @param replicates number of trajectories (replicates) to be returned that are being sampled from the posterior probability results produced by \code{run_mcmc_chain}
#' to select parameter set. For each parmater set sampled, run particle filter with \code{n_particles} and sample 1 trajectory
#' @param forecast Number of days to forecast forward. Default = 0
#' @param required_acceptance_ratio Desired MCMC acceptance ratio
#' @param start_adaptation Iteration number to begin RM optimisation of scaling factor at
#' @param ... Further aguments for the model parameter function. If using the
#'   \code{\link{explicit_model}} (default) this will be
#'   \code{parameters_explicit_SEEIR}.
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
#' @importFrom stats rnorm plogis qnorm cov median
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
                  proposal_kernel = NULL,
                  reporting_fraction = 1,
                  country = NULL,
                  population = NULL,
                  contact_matrix_set = NULL,
                  baseline_contact_matrix = NULL,
                  date_contact_matrix_set_change = NULL,
                  date_R0_change = NULL,
                  R0_change = NULL,
                  hosp_bed_capacity = NULL,
                  baseline_hosp_bed_capacity = NULL,
                  date_hosp_bed_capacity_change = NULL,
                  ICU_bed_capacity = NULL,
                  baseline_ICU_bed_capacity = NULL,
                  date_ICU_bed_capacity_change = NULL,
                  Rt_args = NULL,
                  Rt_modify_func = NULL,
                  burnin = 0,
                  replicates = 100,
                  forecast = 0,
                  required_acceptance_ratio = 0.23,
                  start_adaptation = round(n_mcmc/2),
                  ...
) {

  #------------------------------------------------------------
  # Section 1 of pMCMC Wrapper: Checks & Setup
  #------------------------------------------------------------

  #--------------------
  # assertions & checks
  #--------------------

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

  # check proposal kernel
  assert_matrix(proposal_kernel)
  assert_eq(colnames(proposal_kernel), names(pars_init[[1]]))
  assert_eq(rownames(proposal_kernel), names(pars_init[[1]]))

  # check likelihood items
  if ( !(is.null(log_likelihood) | inherits(log_likelihood, "function")) ) {
    stop("Log Likelihood (log_likelihood) must be null or a user specified function")
  }
  if ( !(is.null(log_prior) | inherits(log_prior, "function")) ) {
    stop("Log Likelihood (log_likelihood) must be null or a user specified function")
  }
  assert_logical(unlist(pars_discrete))
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

  #----------------
  # Generate Odin items
  #----------------

  # make the date definitely a date
  data$date <- as.Date(as.character(data$date))

  # adjust for reporting fraction
  pars_obs$phi_cases <- reporting_fraction
  pars_obs$phi_death <- reporting_fraction

  # build model parameters
  model_params <- squire_model$parameter_func(country = country,
                                              population = population,
                                              dt = 1/steps_per_day,
                                              contact_matrix_set = contact_matrix_set,
                                              tt_contact_matrix = tt_contact_matrix,
                                              hosp_bed_capacity = hosp_bed_capacity,
                                              tt_hosp_beds = tt_hosp_beds,
                                              ICU_bed_capacity = ICU_bed_capacity,
                                              tt_ICU_beds = tt_ICU_beds,
                                              ...)

  # collect interventions for odin model likelihood
  interventions <- list(R0_change = R0_change,
                        date_R0_change = date_R0_change,
                        date_contact_matrix_set_change = date_contact_matrix_set_change,
                        contact_matrix_set = contact_matrix_set,
                        date_ICU_bed_capacity_change = date_ICU_bed_capacity_change,
                        ICU_bed_capacity = ICU_bed_capacity,
                        date_hosp_bed_capacity_change = date_hosp_bed_capacity_change,
                        hosp_bed_capacity = hosp_bed_capacity)

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
                proposal_kernel = proposal_kernel,
                pars_discrete = pars_discrete),
    n_particles = n_particles)


  #----------------
  # create prior and likelihood functions given the inputs
  #----------------

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

  # create shorthand function to calc_ll given main inputs
  calc_ll <- function(pars) {
    X <- log_likelihood(pars = pars,
                        data = data,
                        squire_model = squire_model,
                        model_params = model_params,
                        interventions = interventions,
                        pars_obs = pars_obs,
                        n_particles = n_particles,
                        forecast_days = 0,
                        Rt_args = Rt_args,
                        Rt_modify_func = Rt_modify_func,
                        return = "ll"
    )
    X
  }

  #----------------
  # proposals
  #----------------

  # needs to be a vector to pass to reflecting boundary function
  pars_min <- unlist(pars_min)
  pars_max <- unlist(pars_max)
  pars_discrete <- unlist(pars_discrete)

  #--------------------------------------------------------
  # Section 2 of pMCMC Wrapper: Run pMCMC
  #--------------------------------------------------------

  # Run the chains in parallel
  message("Running pMCMC...")
  if (Sys.getenv("SQUIRE_PARALLEL_DEBUG") == "TRUE") {

    chains <- purrr::pmap(
      .l =  list(n_mcmc = rep(n_mcmc, n_chains),
                 curr_pars = pars_init),
      .f = run_mcmc_chain,
      inputs = inputs,
      calc_lprior = calc_lprior,
      calc_ll = calc_ll,
      first_data_date = data$date[1],
      output_proposals = output_proposals,
      required_acceptance_ratio = required_acceptance_ratio,
      start_adaptation = start_adaptation,
      proposal_kernel = proposal_kernel,
      pars_discrete = pars_discrete,
      pars_min = pars_min,
      pars_max = pars_max)

  } else {

    chains <- furrr::future_pmap(
      .l =  list(n_mcmc = rep(n_mcmc, n_chains),
                 curr_pars = pars_init),
      .f = run_mcmc_chain,
      inputs = inputs,
      calc_lprior = calc_lprior,
      calc_ll = calc_ll,
      first_data_date = data$date[1],
      output_proposals = output_proposals,
      required_acceptance_ratio = required_acceptance_ratio,
      start_adaptation = start_adaptation,
      proposal_kernel = proposal_kernel,
      pars_discrete = pars_discrete,
      pars_min = pars_min,
      pars_max = pars_max,
      .progress = TRUE)

  }

  #----------------
  # MCMC diagnostics and tidy
  #----------------
  if (n_chains > 1) {
    names(chains) <- paste0('chain', seq_len(n_chains))

    # calculating rhat
    # convert parallel chains to a coda-friendly format
    chains_coda <- lapply(chains, function(x) {

      traces <- x$results
      if('start_date' %in% names(pars_init[[1]])) {
        traces$start_date <- start_date_to_offset(data$date[1], traces$start_date)
      }

      coda::as.mcmc(traces[, names(pars_init[[1]])])
    })

    rhat <- tryCatch(expr = {
      x <- coda::gelman.diag(chains_coda)
      x
    }, error = function(e) {
      message('unable to calculate rhat')
    })


    pmcmc <- list(inputs = chains[[1]]$inputs,
                  rhat = rhat,
                  chains = lapply(chains, '[', -1))

    class(pmcmc) <- 'squire_pmcmc_list'

  } else {

    pmcmc <- chains[[1]]
    class(pmcmc) <- "squire_pmcmc"

  }
  #--------------------------------------------------------
  # Section 3 of pMCMC Wrapper: Sample PMCMC Results
  #--------------------------------------------------------
  pmcmc_samples <- sample_pmcmc(pmcmc_results = pmcmc,
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
                             population = population,
                             replicates = 1,
                             day_return = TRUE,
                             time_period = nrow(pmcmc_samples$trajectories))

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


#' Run a single pMCMC chain
#'
#' Helper function to run the particle filter with a
#' new R0 and start date for given interventions within the pmcmc
#'
#' @importFrom stats cov
#' @noRd
run_mcmc_chain <- function(inputs,
                           curr_pars,
                           calc_lprior,
                           calc_ll,
                           n_mcmc,
                           first_data_date,
                           output_proposals,
                           required_acceptance_ratio,
                           start_adaptation,
                           proposal_kernel,
                           pars_discrete,
                           pars_min,
                           pars_max) {


  #----------------
  # Set initial state
  #----------------

  # run particle filter on initial parameters
  p_filter_est <- calc_ll(pars = curr_pars)

  # NB, squire originally set up to deal with date format triggering
  # however, proposal and log prior set up to deal with numerics, need to change
  curr_pars[["start_date"]] <- -(start_date_to_offset(first_data_date, curr_pars[["start_date"]]))
  curr_pars <- unlist(curr_pars)

  ## calculate initial prior
  curr_lprior <- calc_lprior(pars = curr_pars)

  #----------------..
  # assertions and checks on log_prior and log_likelihood functions
  #----------------..
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

  #----------------..
  # Create objects to store outputs
  #----------------..
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
                dimnames = list(NULL, names(res_init)))
  states <- matrix(data = NA,
                   nrow = n_mcmc + 1L,
                   ncol = length(curr_ss))

  # New storage arrays for Robbins-Munro optimisation

  # storage for acceptances over time
  acceptances <- vector(mode = "numeric", length = n_mcmc) # tracks acceptances over time

  # storage for covariance matrices over time - only properly initalised if we're actually adapting
  # i.e. in instances where start_adaptation < n_mcmc
  if (n_mcmc - start_adaptation <= 0) {
    covariance_matrix_storage <- vector(mode = "list", length = 1)
  } else {
    covariance_matrix_storage <- vector(mode = "list", length = (n_mcmc - start_adaptation + 1))
  }

  # storage for scaling factor over time - only properly initalised if we're actually adapting
  # i.e. in instances where start_adaptation < n_mcmc
  if (n_mcmc - start_adaptation <= 0) {
    scaling_factor_storage <- vector(mode = "numeric", length = 1)
  } else {
    scaling_factor_storage <- vector(mode = "numeric", length = (n_mcmc - start_adaptation + 1))
  }

  if(output_proposals) {
    proposals <- matrix(data = NA,
                        nrow = n_mcmc + 1L,
                        ncol = length(res_init) + 1L,
                        dimnames = list(NULL, c(names(res_init), 'accept_prob')))
  }

  ## record initial results
  res[1, ] <- res_init
  states[1, ] <- curr_ss

  # negative here because we are working backwards in time
  pars_min[["start_date"]] <- -(start_date_to_offset(first_data_date, pars_min[["start_date"]]))
  pars_max[["start_date"]] <- -(start_date_to_offset(first_data_date, pars_max[["start_date"]]))

  #----------------
  # main pmcmc loop
  #----------------
  scaling_factor <- 1
  for(iter in seq_len(n_mcmc) + 1L) {

    prop_pars <- propose_parameters(curr_pars,
                                    proposal_kernel * scaling_factor,
                                    unlist(pars_discrete),
                                    unlist(pars_min),
                                    unlist(pars_max))

    prop_for_eval <- prop_pars$for_eval
    prop_for_chain <- prop_pars$for_chain

    ## calculate proposed prior / lhood / posterior
    prop_lprior <- calc_lprior(pars = prop_for_eval)
    prop_pars.squire <- as.list(prop_for_eval)
    prop_pars.squire[["start_date"]] <- offset_to_start_date(first_data_date, prop_for_eval[["start_date"]]) # convert to date
    p_filter_est <- calc_ll(pars = prop_pars.squire)
    prop_ll <- p_filter_est$log_likelihood
    prop_ss <- p_filter_est$sample_state
    prop_lpost <- prop_lprior + prop_ll

    # calculate probability of acceptance
    accept_prob <- exp(prop_lpost - curr_lpost)

    if(runif(1) < accept_prob) { # MH step
      # update parameters and calculated likelihoods
      curr_pars <- prop_for_chain
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
    if (iter >= start_adaptation) {
      timing_cov <- iter - start_adaptation + 1 # iteration relative to when covariance adaptation started
      if (iter == start_adaptation) {
        previous_mu <- matrix(colMeans(res[1:iter, seq_along(curr_pars)]), nrow = 1)
        current_parameters <- matrix(curr_pars, nrow = 1)
        temp <- jc_prop_update(acceptances[iter], timing_cov, scaling_factor, previous_mu,
                               curr_pars, proposal_kernel, required_acceptance_ratio)
        scaling_factor <- temp$scaling_factor
        proposal_kernel <- temp$covariance_matrix
        previous_mu <- temp$mu

        covariance_matrix_storage[[timing_cov]] <- proposal_kernel
        scaling_factor_storage[[timing_cov]] <- scaling_factor

      } else {
        temp <- jc_prop_update(acceptances[iter], timing_cov, scaling_factor, previous_mu,
                               curr_pars, proposal_kernel, required_acceptance_ratio)
        scaling_factor <- temp$scaling_factor
        proposal_kernel <- temp$covariance_matrix
        previous_mu <- temp$mu

        covariance_matrix_storage[[timing_cov]] <- proposal_kernel
        scaling_factor_storage[[timing_cov]] <- scaling_factor
      }
    }

    if(output_proposals) {
      proposals[iter, ] <- c(prop_for_chain,
                             prop_lprior,
                             prop_ll,
                             prop_lpost,
                             min(accept_prob, 1))
    }

    if (iter %% 100 == 0) {
      print(c(round(scaling_factor, 3), round(sum(acceptances, na.rm = TRUE)/iter, 3), round(iter, 1)))
    }
  }

  res <- as.data.frame(res)

  coda_res <- coda::as.mcmc(res)
  rejection_rate <- coda::rejectionRate(coda_res)
  ess <- coda::effectiveSize(coda_res)

  # res$start_date <- offset_to_start_date(first_data_date, res$start_date)

  out <- list('inputs' = inputs,
              'results' = as.data.frame(res),
              'states' = states,
              'acceptance_rate' = 1-rejection_rate,
              "ess" = ess,
              "scaling_factor" = scaling_factor_storage,
              "covariance_matrix" = covariance_matrix_storage,
              "acceptance_ratio" = mean(acceptances),
              "acceptances" = acceptances)

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
                               Rt_args,Rt_modify_func = NULL,
                               interventions) {
  #----------------..
  # specify particle setup
  #----------------..
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

  #----------------..
  # (potentially redundant) assertion
  #----------------..
  assert_in(c("R0", "start_date"), names(pars),
            message = "Must specify R0, start date to infer")

  #----------------..
  # unpack current params
  #----------------..
  R0 <- pars[["R0"]]
  start_date <- pars[["start_date"]]

  #----------------..
  # more assertions
  #----------------..
  assert_pos(R0)
  assert_date(start_date)

  #----------------..
  # setup model based on inputs and interventions
  #----------------..
  R0_change <- interventions$R0_change
  date_R0_change <- interventions$date_R0_change
  date_contact_matrix_set_change <- interventions$date_contact_matrix_set_change
  date_ICU_bed_capacity_change <- interventions$date_ICU_bed_capacity_change
  date_hosp_bed_capacity_change <- interventions$date_hosp_bed_capacity_change

  # change betas
  if (is.null(date_R0_change)) {
    tt_beta <- 0
  } else {

    # quick handle for env changes
    if("env_slp" %in% names(pars)) {
      tt_list <- intervention_dates_for_odin(dates = date_R0_change,
                                             change = Rt_args[["env_dat"]],
                                             start_date = start_date,
                                             steps_per_day = round(1/model_params$dt))
      Rt_args_save <- Rt_args
      Rt_args[["env_dat"]] <- tt_list$change

    }

    tt_list <- intervention_dates_for_odin(dates = date_R0_change,
                                           change = R0_change,
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt))
    model_params$tt_beta <- tt_list$tt
    R0_change <- tt_list$change
    date_R0_change <- tt_list$dates



  }

  # and contact matrixes
  if (is.null(date_contact_matrix_set_change)) {
    tt_contact_matrix <- 0
  } else {
    tt_list <- intervention_dates_for_odin(dates = sort(unique(c(start_date,date_contact_matrix_set_change))),
                                           change = model_params$contact_matrix_set,
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt))
    model_params$tt_contact_matrix <- tt_list$tt
    model_params$contact_matrix_set <- tt_list$change
  }

  # and icu beds
  if (is.null(date_ICU_bed_capacity_change)) {
    tt_ICU_beds <- 0
  } else {
    tt_list <- intervention_dates_for_odin(dates = sort(unique(c(start_date,date_ICU_bed_capacity_change))),
                                           change = model_params$ICU_beds,
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt))
    model_params$tt_ICU_beds <- tt_list$tt
    model_params$ICU_beds <- tt_list$change
  }

  # and hosp beds
  if (is.null(date_hosp_bed_capacity_change)) {
    tt_hosp_beds <- 0
  } else {
    tt_list <- intervention_dates_for_odin(dates = sort(unique(c(start_date,date_hosp_bed_capacity_change))),
                                           change = model_params$hosp_beds,
                                           start_date = start_date,
                                           steps_per_day = round(1/model_params$dt))
    model_params$tt_hosp_beds <- tt_list$tt
    model_params$hosp_beds <- tt_list$change
  }

  #--------------------..
  # update new R0s based on R0_change and R0_date_change, and Meff_date_change
  #--------------------..
  # and now get new R0s for the R0
  R0 <- evaluate_Rt_pmcmc(R0_change = R0_change,
                          R0 = R0,
                          date_R0_change = date_R0_change,
                          pars = pars,
                          Rt_args = Rt_args)

  # modify if extra func provided
  if (!is.null(Rt_modify_func)) {
    R0 <- Rt_modify_func(R0, pars, Rt_args)
    # probably don't even need to do this save as it should be the default Rt_args each time calc_loglikelihood is run
    if(exists("Rt_args_save")) {
      Rt_args <- Rt_args_save
    }
  }

  # which allow us to work out our beta
  beta_set <- beta_est(squire_model = squire_model,
                       model_params = model_params,
                       R0 = R0)

  #----------------..
  # update the model params accordingly from new inputs
  #----------------..
  model_params$beta_set <- beta_set

  #----------------..
  # run the particle filter
  #----------------..
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

    pf_result <- run_deterministic_comparison(data = data,
                                              squire_model = squire_model,
                                              model_params = model_params,
                                              model_start_date = start_date,
                                              obs_params = pars_obs,
                                              forecast_days = forecast_days,
                                              save_history = save_particles,
                                              return = pf_return)

  }

  # out
  pf_result
}

#' @noRd
Rt_args_list <- function(plateau_duration = 7,
                         date_Meff_change = NULL,
                         scale_Meff_pl = FALSE,
                         Rt_shift_duration = 30) {

  if(!is.null(date_Meff_change)) {
    assert_date(date_Meff_change)
  }

  list(plateau_duration = plateau_duration,
       date_Meff_change = date_Meff_change,
       scale_Meff_pl = scale_Meff_pl,
       Rt_shift_duration = Rt_shift_duration)

}

# Evaluate Rt
#' @noRd
evaluate_Rt_pmcmc <- function(R0_change,
                              R0,
                              date_R0_change,
                              pars,
                              Rt_args) {

  # unpack pars
  Meff <- pars[["Meff"]]
  Meff_pl <- pars[["Meff_pl"]]
  Rt_shift <- pars[["Rt_shift"]]
  Rt_shift_scale <- pars[["Rt_shift_scale"]]

  # unpack Rt_args
  plateau_duration <- Rt_args[["plateau_duration"]]
  date_Meff_change <- Rt_args[["date_Meff_change"]]
  scale_meff_pl <- Rt_args[["scale_Meff_pl"]]
  Rt_shift_duration <- Rt_args[["Rt_shift_duration"]]

  # and now get new Rts for the R0
  if (!is.null(R0_change)) {

    # if there is no Meff then we just do a linear transform
    if (is.null(Meff)) {

      Rt <- R0*R0_change

    } else {

    # if no date_Meff_change then all from R0_change and Meff
    if (is.null(date_Meff_change)) {

      Rt <- R0 * (2 * plogis(-(R0_change - 1) * -Meff))

    } else if (!is.null(date_Meff_change)) {

      date_Meff_change <- as.Date(date_Meff_change)
      date_R0_change <- as.Date(date_R0_change)

      # scale Meff accordingly
      if (!is.null(scale_meff_pl)) {
      if (scale_meff_pl) {
        Meff_pl <- Meff_pl*Meff
      }
      }

      # if no shift then set to 0
      if (is.null(Rt_shift)) {
        Rt_shift <- 0
      } else {
        Rt_shift <- Rt_shift*plogis(seq(-10,10,length.out = max(2,Rt_shift_duration)),
                              scale = Rt_shift_scale)
      }

      # when does mobility change take place
      if(date_Meff_change <= date_R0_change[1]) {

        mob_up <- R0_change-R0_change[1]
        Rt <- R0 * 2*(plogis( -Meff * -(R0_change-1) - Meff_pl*(mob_up) ))

      } else if (date_Meff_change > tail(date_R0_change, 1)) {

        Rt <- R0 * (2 * plogis(-(R0_change - 1) * -Meff))

      } else {

        # when is the swithc in our data
        swtchdates <- which(date_R0_change >= date_Meff_change)
        min_d <- as.Date(date_R0_change[min(swtchdates)])
        dates_to_median <- seq(min_d - floor(plateau_duration/2),min_d + floor(plateau_duration/2),1)

        # Work out the mobility during this period
        mob_pld <- median(R0_change[which(date_R0_change %in% dates_to_median)])

        mob_up <- c(rep(0, swtchdates[1]-1),
                    R0_change[min(swtchdates):(length(R0_change))] - mob_pld)

        Rt_pl_change <- c(rep(0, min(swtchdates[1]-1)),
                          Rt_shift,
                          rep(tail(Rt_shift,1), max(0, length(mob_up)-Rt_shift_duration)))
        Rt_pl_change <- head(Rt_pl_change, length(mob_up))

        # now work out Rt forwards based on mobility increasing from this plateau
        Rt <- R0 * 2*(plogis( -Meff * -(R0_change-1) - Meff_pl*(mob_up) - Rt_pl_change ))

      }
    }

    }

  } else {

    Rt <- rep(R0, length(date_R0_change))

  }

  return(Rt)

}


# proposal for MCMC
propose_parameters <- function(pars, proposal_kernel, pars_discrete, pars_min, pars_max) {

  ## proposed jumps are normal with mean pars and sd as input for parameter
  proposed <- pars + drop(rmvnorm(n = 1,  sigma = proposal_kernel))
  for_chain <- reflect_proposal(x = proposed,
                                floor = pars_min,
                                cap = pars_max)

  # discretise if necessary
  for_eval <- proposed
  for_eval[pars_discrete] <- round(for_eval[pars_discrete])
  for_eval <- reflect_proposal(x = for_eval,
                               floor = pars_min,
                               cap = pars_max)
  return(list(for_eval = for_eval, for_chain = for_chain))
}


## create function to reflect proposal boundaries at pars_min and pars_max
# this ensures the proposal is symetrical and we can simplify the MH step

reflect_proposal <- function(x, floor, cap) {
  interval <- cap - floor
  abs((x + interval - floor) %% (2 * interval) - interval) + floor
}




