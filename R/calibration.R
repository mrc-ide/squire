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
#' @param reporting_quality When generating synthetic data, what is the
#'   reporting quality, i.e. probability of reporting a death prior to the current
#'   total
#'
#' @return Time series of deaths as \code{data.frame}
death_data_format <- function(date = NULL,
                              deaths = NULL,
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

  }

  # order by date
  deaths <- deaths[order(date, decreasing = TRUE)]

  # check that deaths are increasing
  assert_decreasing(deaths)

  # create df
  df <- data.frame("date" = as.Date(date),
                   "deaths" = deaths)

  return(df)

}


#' Calibrate Model
#'
#' @details Fit the explicit_SEEIR model to time series of deaths
#'
#' @param data Data frame with 2 variables: date and deaths
#' @param country Character. Country data originates from.
#' @param replicates Simulation Repetitions. Default = 100
#' @param ... Other parameters to pass to \code{\link{run_explicit_SEEIR_model}}
#' @importFrom utils tail
#' @importFrom stats rbinom time
#'
#' @return Long data frame of simulation replicates
calibrate <- function(data, country, replicates = 100, ...) {

  # assertions
  assert_dataframe(data)
  assert_string(country)
  if (!all(c("date", "deaths") %in% names(data))) {
    stop("data does not contain a date and/or a deaths column")
  }

  # get inputs
  data <- death_data_format(date = data$date, deaths = data$deaths)
  pop <- get_population(country)
  contact_matrix <- get_mixing_matrix(country)

  # run model with fixed day step (to match up with daily deaths)
  r <- run_explicit_SEEIR_model(population = pop$n,
                                baseline_contact_matrix = contact_matrix,
                                contact_matrix_set = contact_matrix,
                                replicates = replicates,
                                dt = 1,
                                ...)

  # wide output and group by Infection classes
  out <- wide_output(r$output)

  # Don't need to group by I
  # dplyr::mutate(E = .data$E1 + .data$E2,
  #               I = .data$IMild + .data$ICase1 + .data$ICase2 +
  #                 .data$IOxGetLive1 + .data$IOxGetLive2 + .data$IOxGetDie1 +
  #                 .data$IOxGetDie2 + .data$IOxNotGetLive1 + .data$IOxNotGetLive2 +
  #                 .data$IOxNotGetDie1 + .data$IOxNotGetDie2 + .data$IMVGetLive1 +
  #                 .data$IMVGetLive2 + .data$IMVGetDie1 + .data$IMVGetDie2 +
  #                 .data$IMVNotGetLive1 + .data$IMVNotGetLive2 + .data$IMVNotGetDie1 +
  #                 .data$IMVNotGetDie2 + .data$IRec1 + .data$IRec2)

  # reset the time
  l_dths <- data$deaths[1]
  l_date <- data$date[1]
  secs_in_day <- 24*60*60

  # timing of deaths in replicates
  timings <- dplyr::group_by(out, replicate, t) %>%
    dplyr::summarise(D_sum = sum(.data$D)) %>%
    dplyr::summarise(t = .data$t[which.max(.data$D_sum>=l_dths)])

  out <- dplyr::group_by(out, replicate) %>%
    dplyr::mutate(new_time = .data$t - timings$t[timings$replicate == replicate[1]],
                  date = l_date + .data$new_time)

  long <- tidyr::pivot_longer(out, .data$S:.data$D)

  return(long)
}
