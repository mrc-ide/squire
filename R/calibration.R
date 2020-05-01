#' Calibrate via particle filter grid search using time series of deaths
#'
#' @param reporting_fraction Reporting fraction. Numeric for what proportion of
#'   the total deaths the reported deaths represent. E.g. 0.5 results in
#'   the model calibrating to twice the deaths provided by \code{data$deaths}
#' @param replicates Replicates to be run. Default = 100
#' @param forecast Number of days to forecast forward. Default = 0
#' @param baseline_hosp_bed_capacity The starting number of hospital beds before
#'   the epidemic started. Default = NULL, which will use the hospital beds data
#'   for the country provided. If no country is provided then this is 5/1000 of
#'   the population
#' @param hosp_bed_capacity Number of hospital beds at each date specified in
#'   \code{date_hosp_bed_capacity_change}. Must be same length as
#'   \code{date_hosp_bed_capacity_change}.
#' @param baseline_ICU_bed_capacity The starting number of ICU beds before
#'   the epidemic started. Default = NULL, which will use the hospital beds data
#'   for the country provided. If no country is provided then this is 3/100 of
#'   hospital beds
#' @param ICU_bed_capacity Number of ICU beds at each date specified in
#'   \code{date_ICU_bed_capacity_change}. Must be same length as
#'   \code{date_ICU_bed_capacity_change}.
#' @param baseline_contact_matrix The starting contact matrix prior to any changes
#'   due to interventions or otherwise. Default = NULL, which will use the contact
#'   matrix associated with the coutnry provided.
#' @param contact_matrix_set List of contact matrices to be used from the dates
#'   provided in \code{date_contact_matrix_set_change}.Must be same length as
#'   \code{date_contact_matrix_set_change}
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
                      baseline_ICU_bed_capacity = NULL,
                      ICU_bed_capacity = NULL,
                      date_hosp_bed_capacity_change = NULL,
                      baseline_hosp_bed_capacity = NULL,
                      hosp_bed_capacity = NULL,
                      date_contact_matrix_set_change = NULL,
                      baseline_contact_matrix = NULL,
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
  assert_numeric(n_particles)
  assert_numeric(reporting_fraction)
  assert_custom_class(squire_model, "squire_model")
  assert_bounded(reporting_fraction, 0, 1, inclusive_left = FALSE, inclusive_right = TRUE)
  assert_in("date", names(data))
  assert_in("deaths", names(data))
  assert_same_length(R0_change, date_R0_change)
  if(!is.null(contact_matrix_set)) {
    assert_list(contact_matrix_set)
  }
  assert_same_length(contact_matrix_set, date_contact_matrix_set_change)
  assert_same_length(ICU_bed_capacity, date_ICU_bed_capacity_change)
  assert_same_length(hosp_bed_capacity, date_hosp_bed_capacity_change)

  # check grid params are okay
  if (as.Date(last_start_date) >= as.Date(data$date[1])-1) {
    stop("'last_start_date' must be at least 2 days before the first date in data")
  }
  if (as.Date(first_start_date) >= as.Date(last_start_date)) {
    stop("'last_start_date' must be greater than 'first_start_date'")
  }

  # checks that dates are not in the future compared to our data
  if(!is.null(date_R0_change)) {
    assert_date(date_R0_change)
    if(as.Date(tail(date_R0_change,1)) > as.Date(tail(data$date, 1))) {
      stop("Last date in date_R0_change is greater than the last date in data")
    }
    if(as.Date(last_start_date) >= as.Date(head(date_R0_change, 1))) {
      stop("First date in date_R0_change is earlier than last_start_date")
    }
  }

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
    if(as.Date(last_start_date) >= as.Date(head(date_contact_matrix_set_change, 1))) {
      stop("First date in date_contact_matrix_set_change is earlier than last_start_date")
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
    if(as.Date(last_start_date) >= as.Date(head(date_ICU_bed_capacity_change, 1))) {
      stop("First date in date_ICU_bed_capacity_change is earlier than last_start_date")
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
    if(as.Date(last_start_date) >= as.Date(head(date_hosp_bed_capacity_change, 1))) {
      stop("First date in date_hosp_bed_capacity_change is earlier than last_start_date")
    }

    tt_hosp_beds <- c(0, seq_len(length(date_hosp_bed_capacity_change)))
    hosp_bed_capacity <- c(baseline_hosp_bed_capacity, hosp_bed_capacity)

  } else {
    tt_hosp_beds <- 0
    hosp_bed_capacity <- baseline_hosp_bed_capacity
  }

  # make the date definitely a date
  data$date <- as.Date(as.character(data$date))

  # adjust for reporting fraction
  data$deaths <- (data$deaths/reporting_fraction)

  # build model parameters
  model_params <- squire_model$parameter_func(country = country,
                                              population = population,
                                              contact_matrix_set = contact_matrix_set,
                                              tt_contact_matrix = tt_contact_matrix,
                                              hosp_bed_capacity = hosp_bed_capacity,
                                              tt_hosp_beds = tt_hosp_beds,
                                              ICU_bed_capacity = ICU_bed_capacity,
                                              tt_ICU_beds = tt_ICU_beds,
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
                          forecast_days = forecast ,
                          full_output = TRUE)

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
                             time_period = max(tt_contact_matrix,tt_hosp_beds,tt_ICU_beds,1),
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
                          contact_matrix_set = contact_matrix_set,
                          date_ICU_bed_capacity_change = date_ICU_bed_capacity_change,
                          ICU_bed_capacity = ICU_bed_capacity,
                          date_hosp_bed_capacity_change = date_hosp_bed_capacity_change,
                          hosp_bed_capacity = hosp_bed_capacity)


  # as well as adding the scan_results so it's easy to draw from the scan again in the future
  r$scan_results <- scan_results

  # and add the parameters that changed between each simulation, i.e. drawn from gris
  r$replicate_parameters <- res$param_grid

  # and fix the replicates
  r$parameters$replicates <- replicates
  r$parameters$time_period <- diff(range(r$output[,"time",]))

  return(r)
}
