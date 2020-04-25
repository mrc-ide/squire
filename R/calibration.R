#' Calibrate deaths
#'
#' @param deaths Number of observed deaths
#' @param R0 R0 to be passed to \code{\link{run_explicit_SEEIR_model}}.
#'   Default = 3
#' @param R0_scan Vector or R0 values to sample from to introduce uncertainty
#'   in predictions. Default = NULL, which will not scan. If provided, the first
#'   value in \code{R0} will be drawn from \code{R0_scan}
#' @param replicates Replicates to be passed to
#'   \code{\link{run_explicit_SEEIR_model}}. Default = 100
#' @param reporting_fraction Reporting fraction. Numeric for what proportion of
#'   the total deaths the reported deaths represent. E.g. 0.5 results in
#'   the model calibrating to twice the deaths provided by \code{deaths}
#' @param seeding_age_groups Age groups for seeding
#' @param min_seeding_cases Minimum seeding cases
#' @param max_seeding_cases Maximum seeding cases
#' @param ... Further aguments for \code{run_explicit_SEEIR_model()}
#' @inheritParams run_explicit_SEEIR_model
#'
#' @export
#' @return List of time adjusted squire_simulations
calibrate <- function(deaths,
                      reporting_fraction = 1,
                      country = NULL,
                      population = NULL,
                      contact_matrix_set = NULL,
                      seeding_age_groups = c("35-40", "40-45", "45-50", "50-55"),
                      min_seeding_cases = 5,
                      max_seeding_cases = 50,
                      R0 = 3,
                      R0_scan = NULL,
                      replicates = 100,
                      ...) {


  # argument checks
  assert_numeric(deaths)
  assert_numeric(reporting_fraction)
  assert_bounded(reporting_fraction, 0, 1)
  assert_greq(deaths, 1)
  assert_gr(R0[1], 1)

  # Handle country population args
  cpm <- parse_country_population_mixing_matrix(country = country,
                                                population = population,
                                                contact_matrix_set = contact_matrix_set)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

  # getting indices for relevant age groups where seeding cases occurred
  age_groups <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
                  "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
                  "70-75", "75-80", "80+")
  if (!all(seeding_age_groups %in% age_groups)) {
    stop("inputted age groups not valid")
  }
  age_group_indices <- which(age_groups %in% seeding_age_groups)
  num_age_groups <- length(age_group_indices)

  # adjust for reporting fraction
  true_deaths <- deaths / reporting_fraction

  # generating the seeding cases for each of the replicates
  E1_0 <- lapply(seq_len(replicates), function(x) {
    seeding_cases <- rep(0, length.out = length(population))
    raw_seeding_cases <- round(stats::runif(n = 1, min = min_seeding_cases, max = max_seeding_cases))
    seeding_cases[age_group_indices] <- as.vector(stats::rmultinom(1,
                                                            size = raw_seeding_cases,
                                                            prob = rep(1/num_age_groups,
                                                                       num_age_groups)))
    seeding_cases
  })

  # sample our R0_scan
  if (!is.null(R0_scan)) {

    # check is numeric
    assert_numeric(R0_scan)

    # sample for R0_scan
    if (length(R0_scan) == 1) {
      R0_scan <- rep(R0_scan, replicates)
    } else {
      R0_scan <- sample(R0_scan, replicates, TRUE)
    }

    R0[1] <- R0_scan[1]
  } else {
    R0_scan <- rep(R0[1], replicates)
  }


  # run model with fixed day step (to match up with daily deaths)
  r <- run_explicit_SEEIR_model(population = population,
                                contact_matrix_set = contact_matrix_set,
                                replicates = 1,
                                R0 = R0,
                                ...)

  # get model run outputs
  t <- seq(from = 1, to = r$parameters$time_period / r$parameters$dt)
  nt <- length(t)

  # get the index for looking up D and R
  index <- odin_index(r$model)

  # check that this reached the deaths
  while (sum(r$output[nt, index$D, 1]) < deaths) {
    r <- run_explicit_SEEIR_model(population = population,
                                  contact_matrix_set = contact_matrix_set,
                                  replicates = 1,
                                  R0 = R0,
                                  ...)
  }

  # assign to our results
  out <- list()
  out[[1]] <- r

  # what is the beta for updating in each rep
  beta <- r$model$contents()$beta_set

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
    while (sum(r$output[nt, index$D, 1]) < deaths) {
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



#' Calibrate via particle filter grid search using time series of deaths
#'
#' @param forecast = 0
#' @param reporting_fraction Reporting fraction. Numeric for what proportion of
#'   the total deaths the reported deaths represent. E.g. 0.5 results in
#'   the model calibrating to twice the deaths provided by \code{data$deaths}
#' @param ... Further aguments for the model parameter function. If using the
#'   \code{\link{explicit_model}} (default) this will be
#'   \code{parameters_explicit_SEEIR}.
#'
#' @inheritParams run_explicit_SEEIR_model
#' @inheritParams scan_R0_date
#'
#' @export
#' @return List of dated squire simulations
#'
calibrate_particle <- function(data,
                               R0_min,
                               R0_max,
                               R0_step,
                               first_start_date,
                               last_start_date,
                               day_step,
                               squire_model = explicit_model(),
                               pars_obs = NULL,
                               forecast = 0,
                               n_particles = 100,
                               reporting_fraction = 1,
                               R0_change = NULL,
                               date_R0_change = NULL,
                               date_contact_matrix_set = NULL,
                               date_ICU_bed_capacity_change = NULL,
                               date_hosp_bed_capacity_change = NULL,
                               replicates = 100,
                               country = NULL,
                               population = NULL,
                               contact_matrix_set = NULL,
                               ...) {

  # Asserts on arguments
  assert_dataframe(data)
  assert_numeric(R0_min)
  assert_numeric(R0_max)
  assert_numeric(R0_step)
  assert_date(first_start_date)
  assert_date(last_start_date)
  assert_numeric(day_step)
  assert_numeric(day_step)
  assert_numeric(n_particles)
  assert_numeric(reporting_fraction)
  assert_numeric(R0_change)
  assert_custom_class(squire_model, "squire_model")
  assert_bounded(reporting_fraction, 0, 1, inclusive_left = FALSE, inclusive_right = TRUE)
  if(!is.null(date_R0_change)) { assert_date(date_R0_change) }
  if(!is.null(date_contact_matrix_set)) { assert_date(date_contact_matrix_set) }
  if(!is.null(date_ICU_bed_capacity_change)) { assert_date(date_ICU_bed_capacity_change) }
  if(!is.null(date_hosp_bed_capacity_change)) { assert_date(date_hosp_bed_capacity_change) }

  # adjust for reporting fraction
  data$deaths <- (data$deaths/reporting_fraction)

  # build model parameters
  model_params <- squire_model$parameter_func(country = country,
                                              contact_matrix_set = contact_matrix_set,
                                              population = population,
                                              ...)

  # construct scan
  scan_results <- scan_R0_date(R0_min = R0_min,
                               R0_max = R0_max,
                               R0_step = R0_step,
                               first_start_date = first_start_date,
                               last_start_date = last_start_date,
                               day_step = day_step,
                               data = data,
                               model_params = model_params,
                               R0_change = R0_change,
                               date_R0_change = date_R0_change,
                               date_contact_matrix_set = date_contact_matrix_set,
                               date_ICU_bed_capacity_change = date_ICU_bed_capacity_change,
                               date_hosp_bed_capacity_change = date_hosp_bed_capacity_change,
                               squire_model = squire_model,
                               n_particles = n_particles)

  # carry out sims drawn from the grid
  res <- sample_grid_scan(scan_results = scan_results,
                          n_sample_pairs = replicates,
                          n_particle = n_particles,
                          forecast_days = forecast,
                          full_output = TRUE)

  # create a fake run object and fill in the required elements
  r <- squire_model$run_func(country = country,
                             contact_matrix_set = contact_matrix_set,
                             population = population,
                             replicates = 1,
                             time_period = 1,
                             ...)

  # first let's create the output
  names(res)[names(res) == "trajectories"] <- "output"
  dimnames(res$output) <- list(dimnames(res$output)[[1]], dimnames(r$output)[[2]], NULL)
  r$output <- res$output

  # second let's recreate the output
  r$model <- res$inputs$model$odin_model(
    user = res$inputs$model_params, unused_user_action = "ignore"
  )

  # we will add the interventions here so that we now what times are needed for projection
  r$interventions <- list(R0_change = R0_change,
                          date_R0_change = date_R0_change,
                          date_contact_matrix_set = date_contact_matrix_set,
                          date_ICU_bed_capacity_change = date_ICU_bed_capacity_change,
                          date_hosp_bed_capacity_change = date_hosp_bed_capacity_change)


  # as well as adding the scan_results so it's easy to draw from the scan again in the future
  r$scan_results <- scan_results

  # and add the parameters that changed between each simulation, i.e. drawn from gris
  r$replicate_parameters <- res$param_grid

  # and fix the replicates
  r$parameters$replicates <- replicates

  return(r)
}
