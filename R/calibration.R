#' Generate death time seris
#'
#' @details Create a data frame for time series of
#'   deaths. If not provided, dummy data will
#'   be generated. The resulting data frame will be used
#'   when calibrating the model to fit to timeseries of
#'   deaths.
#'
#'   In the future maybe extend this to include ITU cases
#'   and general hospital cases.
#'
#' @param date Character or Date vector of time series
#' @param deaths Numeric vector of deaths
#' @param cases Numeric vector of deaths
#' @param reporting_quality When generating synthetic data, what is the
#'   reporting quality, i.e. probability of reporting a death prior to the current
#'   total
#'
#' @return Time series of deaths as \code{data.frame}
death_data_format <- function(date = NULL,
                              deaths = NULL,
                              cases = NULL,
                              reporting_quality = 0.2
){

  # If no dates are provided make up some data
  if (is.null(date)) {

    # how many dates do we have before the last
    n_dates <- sample(10, 1)
    date <- rev(c(Sys.Date(), Sys.Date() - (1:n_dates)))

    # how many total deaths by today
    n_deaths <- sample(10, 1)

    # what was the real death incidence prior to today
    real_death_cumulative <- c(rev(round(n_deaths * 2^(-(1:n_dates)/3))), n_deaths)
    incidence <- c(real_death_cumulative[1], diff(real_death_cumulative))

    # observed deaths
    deaths <- cumsum(stats::rbinom(incidence, incidence, reporting_quality))
    deaths[length(deaths)] <- n_deaths

    # cases
    cases <- round(deaths*stats::runif(length(deaths), 0.8, 1.2))

  } else {
    if(is.null(deaths)) {
      stop("Deaths is NULL. If date is provided, deaths must be provided")
    }
    if(is.null(cases)) {
      cases <- rep(NA, length(deaths))
    }
  }

  # order by date
  deaths <- deaths[order(date, decreasing = TRUE)]
  cases <- cases[order(date, decreasing = TRUE)]
  date <- sort(date, decreasing = TRUE)

  # check that deaths and cases are decreasing
  assert_decreasing(deaths[!is.na(deaths)])
  if (length(cases[!is.na(cases)]) > 0) {
    assert_decreasing(cases[!is.na(cases)])
  }

  # create df
  df <- data.frame("date" = as.Date(date),
                   "deaths" = deaths,
                   "cases" = cases)

  return(df)

}

#' Calibrate Model
#'
#' @details Fit the explicit_SEEIR model to time series of deaths
#'
#' @param data Data frame with 2 variables: date and deaths
#' @param country Character. Country data originates from.
#' @param reporting_fraction Numbeic. Fraction of deaths expected to have been reported. DEFAULT = 1
#' @param seeding_age_groups Character vector. Age groups seeding cases should be distributed into.
#' @param min_seeding_cases Numeric. Minimum number of seeding cases. DEFAULT = 5.
#' @param max_seeding_cases Numeric. Maximum number of seeding cases. DEFAULT = 50.
#' @param parse_output Logical. Should output be parsed ready for plotting.
#'   Default = TRUE
#' @param replicates Simulation Repetitions. Default = 10
#' @param dt Time Step. Default = 0.25
#' @param ... Other parameters to pass to \code{\link{run_explicit_SEEIR_model}}
#' @importFrom utils tail
#' @importFrom stats rbinom time rmultinom
#'
#' @export
#' @return List of formatted odin outputs, the data it is calibrated to and
#'   the parameter set used in calibration
calibrate <- function(data, country, reporting_fraction = 1,
                      seeding_age_groups = c("35-40", "40-45", "45-50", "50-55"),
                      min_seeding_cases = 5, max_seeding_cases = 50,
                      parse_output = TRUE, replicates = 100, dt = 0.5, ...) {

  # getting indices for relevant age groups where seeding cases occurred
  age_groups <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
                  "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
                  "70-75", "75-80", "80+")
  if (sum(!(seeding_age_groups %in% age_groups)) > 0) {
    stop("inputted age groups not valid")
  }
  age_group_indices <- which(age_groups %in% seeding_age_groups)
  num_age_groups <- length(age_group_indices)

  # assertions
  assert_dataframe(data)
  assert_string(country)
  if (!all(c("date", "deaths") %in% names(data))) {
    stop("data does not contain a date and/or a deaths column")
  }

  # get inputs
  data <- death_data_format(date = data$date,
                            deaths = data$deaths,
                            cases = data$cases)

  # adjust for reporting fraction
  data$true_deaths <- data$deaths / reporting_fraction

  # get population and mixing matrix for specific country
  pop <- get_population(country)
  contact_matrix <- get_mixing_matrix(country)

  # age_group indices corresponding to middle-aged travellers

  # generating the seeding cases for each of the replicates
  E1_0 <- lapply(seq_len(replicates), function(x) {
    seeding_cases <- rep(0, length = length(pop$n))
    raw_seeding_cases <- round(runif(n = 1, min = min_seeding_cases, max = max_seeding_cases))
    seeding_cases[age_group_indices] <- as.vector(rmultinom(1,
                                                            size = raw_seeding_cases,
                                                            prob = rep(1/num_age_groups, num_age_groups)))
    seeding_cases
  })

  # run model with fixed day step (to match up with daily deaths)
  r <- run_explicit_SEEIR_model(population = pop$n,
                                contact_matrix_set = contact_matrix,
                                replicates = 1,
                                dt = dt,
                                output_transform = FALSE,
                                ...)

  # create array for multiple model runs (with different seeds) to be stored
  r$output <- array(r$output, dim = c(nrow(r$output[,,1]), ncol(r$output[,,1]), replicates))

  # creating the vector of times to run the model over (matching the initial run)
  t <-  seq(from = 1, to = r$parameters$time_period/r$parameters$dt)

  # running and storing the model output for each of the different initial seeding cases
  for(i in 2:replicates) {
    r$mod$set_user(E1_0 = E1_0[[i]])
    r$output[, , i] <- r$mod$run(t, replicate = 1)
  }
  r$parameters$replicates <- replicates

  # get the index for looking up D
  index <- odin_index(r$model)

  # create the shifted date
  timings <- vapply(seq_len(replicates), function(x) {
    which.max(rowSums(r$output[,index$D,x]) >= data$true_deaths[1])
  }, FUN.VALUE = numeric(1))

  r$date <- vapply(seq_len(replicates), function(x) {
    data$date[1] + (r$output[,index$time,x] - (timings[x]*r$parameters$dt))
  }, FUN.VALUE = double(r$parameters$time_period/r$parameters$dt))

  # add the real data used
  r$data <- data

  # parse the output ready for plotting
  if (parse_output) {
    r <- calibrate_output_parsing(r)
  }

  return(r)

}

#' Format output of calibration for plotting
#'
#' @details Calibration output is taken to give time series of infections, cases,
#' cases requiring hospitilisation, case requiring critical care facilities. Used
#' in plotting for nowcasting reports.
#'
#' @param r Output of \code{\link{calibrate}}
#'
#' @return \code{list} with:
#' \itemize{
#'       \item{df:}{ Data frame of case numbers, hospital beds, icu beds and deaths }
#'       \item{data:}{ Raw data used in calibration}
#'       \item{parameters:}{ Parameters used in simulation}
#'       }
#'
calibrate_output_parsing <- function(r) {

  ## Assertions
  assert_custom_class(r, "squire_simulation")
  if(!"date" %in% names(r)) {
    stop("r needs date element")
  }

  # get the index for looking up D
  index <- odin_index(r$model)
  nt <- nrow(r$output)

  mv <- unlist(index[c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
                       "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2")])

  ox <- unlist(index[c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2",
                       "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1","IOxNotGetDie2")])


  # collet outputs as vectors
  mild_cases <- odin_sv(r$output[,index$n_E2_I,] - r$output[,index$n_E2_ICase1,],
                        replicates = r$parameters$replicates, nt = nt)
  hospital_cases <- odin_sv(r$output[,index$n_E2_ICase1,],
                            replicates = r$parameters$replicates, nt = nt)
  icu <- odin_sv(r$output[,mv,],
                 replicates = r$parameters$replicates, nt = nt)
  hospital_bed <- odin_sv(r$output[,ox,],
                          replicates = r$parameters$replicates, nt = nt)
  deaths <- odin_sv(r$output[,index$delta_D,],
                    replicates = r$parameters$replicates, nt = nt)

  # collect into a long data frame
  vars <- c("mild_cases", "hospital_cases", "deaths", "icu", "hospital_bed")
  df <- data.frame("date" = as.numeric(r$date),
                   "replicate" = as.numeric(mapply(rep, seq_len(r$parameters$replicates), nt)),
                   "variable" = as.character(mapply(rep, vars, nt*r$parameters$replicates)),
                   "value" = c(mild_cases, hospital_cases, deaths, icu, hospital_bed))

  ret <- list(df = df, data = r$data, parameters = r$parameters)
  class(ret) <- "squire_calibration"

  return(ret)
}

## Index locations of outputs in odin model
#' @noRd
odin_index <- function(model) {
  n_out <- environment(model$initialize)$private$n_out %||% 0
  n_state <- length(model$initial())
  model$transform_variables(seq_len(1L + n_state + n_out))
}


## Take odin state and calculate sum across ages in a replicate and vectorise
#' @noRd
odin_sv <- function(state, replicates, nt) {
  as.numeric(vapply(seq_len(replicates), function(x) {
    rowSums(state[,,x])
  }, FUN.VALUE = double(nt)))
}
