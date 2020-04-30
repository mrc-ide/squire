#' Calibrate via particle filter grid search using time series of deaths
#'
#' @param reporting_fraction Reporting fraction. Numeric for what proportion of
#'   the total deaths the reported deaths represent. E.g. 0.5 results in
#'   the model calibrating to twice the deaths provided by \code{data$deaths}
#' @param replicates Replicates to be run. Default = 100
#' @param forecast Number of days to forecast forward. Default = 0
#' @param ... Further aguments for the model parameter function. If using the
#'   \code{\link{explicit_model}} (default) this will be
#'   \code{parameters_explicit_SEEIR}.
#'
#' @inheritParams parameters_explicit_SEEIR
#' @inheritParams scan_R0_date
#'
#' @export
#' @return List of dated squire simulations
#'
calibrate <- function(data,
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
                      replicates = 100,
                      date_R0_change = NULL,
                      R0_change = NULL,
                      date_ICU_bed_capacity_change = NULL,
                      ICU_bed_capacity = NULL,
                      date_hosp_bed_capacity_change = NULL,
                      hosp_bed_capacity = NULL,
                      date_contact_matrix_set_change = NULL,
                      contact_matrix_set = NULL,
                      country = NULL,
                      population = NULL,
                      ...) {

  # Asserts on arguments
  assert_dataframe(data)
  assert_numeric(R0_min)
  assert_numeric(R0_max)
  assert_numeric(R0_step)
  assert_date(first_start_date)
  assert_date(last_start_date)
  assert_date(data$date)
  assert_numeric(day_step)
  assert_numeric(day_step)
  assert_numeric(n_particles)
  assert_numeric(reporting_fraction)
  assert_custom_class(squire_model, "squire_model")
  assert_bounded(reporting_fraction, 0, 1, inclusive_left = FALSE, inclusive_right = TRUE)
  assert_in("date", names(data))
  assert_in("deaths", names(data))

  # check grid params are okay
  if (as.Date(last_start_date) >= as.Date(data$date[1])) {
    stop("'last_start_date' must be earlier than the first date in data")
  }
  if (as.Date(first_start_date) >= as.Date(last_start_date)) {
    stop("'last_start_date' must be greater than 'first_start_date'")
  }
  # if (!all(as.Date(last_start_date) <
  #          as.Date(c(date_R0_change, date_ICU_bed_capacity_change,
  #                    date_hosp_bed_capacity_change, date_contact_matrix_set_change)))) {
  #   stop("'last_start_date' must be less than all date changes")
  # }

  # checks that dates are not in the future compared to our data
  if(!is.null(date_R0_change)) {
    assert_date(date_R0_change)
    if(as.Date(tail(date_R0_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_R0_chage is greater than the last date in data")
    }
    assert_same_length(R0_change, date_R0_change)
  }
  if(!is.null(date_contact_matrix_set_change)) {
    assert_date(date_contact_matrix_set_change)
    if(as.Date(tail(date_contact_matrix_set_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_contact_matrix_set_change is greater than the last date in data")
    }
    assert_same_length(contact_matrix_set, date_contact_matrix_set_change)
  }
  if(!is.null(date_ICU_bed_capacity_change)) {
    assert_date(date_ICU_bed_capacity_change)
    if(as.Date(tail(date_ICU_bed_capacity_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_ICU_bed_capacity_change is greater than the last date in data")
    }
    assert_same_length(ICU_bed_capacity, date_ICU_bed_capacity_change)
  }
  if(!is.null(date_hosp_bed_capacity_change)) {
    assert_date(date_hosp_bed_capacity_change)
    if(as.Date(tail(date_hosp_bed_capacity_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_hosp_bed_capacity_change is greater than the last date in data")
    }
    assert_same_length(hosp_bed_capacity, date_hosp_bed_capacity_change)
  }

  # make the date definitely a date
  data$date <- as.Date(as.character(data$date))

  # adjust for reporting fraction
  data$deaths <- (data$deaths/reporting_fraction)

  # build model parameters
  model_params <- squire_model$parameter_func(country = country,
                                              population = population,
                                              contact_matrix_set = contact_matrix_set,
                                              hosp_bed_capacity = hosp_bed_capacity,
                                              ICU_bed_capacity = ICU_bed_capacity,
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
                               date_contact_matrix_set_change = date_contact_matrix_set_change,
                               date_ICU_bed_capacity_change = date_ICU_bed_capacity_change,
                               date_hosp_bed_capacity_change = date_hosp_bed_capacity_change,
                               squire_model = squire_model,
                               n_particles = n_particles)

  # carry out sims drawn from the grid
  res <- sample_grid_scan(scan_results = scan_results,
                          n_sample_pairs = replicates,
                          n_particles = n_particles,
                          forecast_days = forecast + 1, # one because we will need the extra day to work out the difference for incidence
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
  r$model <- res$inputs$model$odin_model(
    user = res$inputs$model_params, unused_user_action = "ignore"
  )

  # we will add the interventions here so that we now what times are needed for projection
  r$interventions <- list(R0_change = R0_change,
                          date_R0_change = date_R0_change,
                          date_contact_matrix_set_change = date_contact_matrix_set_change,
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
