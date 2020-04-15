
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
                              reporting_quality = 0.2){

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

    # Reported deaths
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

#' Format output of calibration for plotting
#'
#' @details Calibration output is taken to give time series of infections, cases,
#' cases requiring hospitilisation, case requiring critical care facilities. Used
#' in plotting for nowcasting reports.
#'
#' @param r Output of \code{\link{calibrate}}
#' @param date_0 Date of time 0, if specified a date column will be added
#'
#' @return \code{list} with:
#' \itemize{
#'       \item{df:}{ Data frame of case numbers, hospital beds, ICU beds and deaths }
#'       \item{data:}{ Raw data used in calibration}
#'       \item{parameters:}{ Parameters used in simulation}
#'       }
#'
calibrate_output_parsing <- function(r, date_0 = Sys.Date()) {

  ## Assertions
  assert_custom_class(r, "squire_simulation")

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
  ICU <- odin_sv(r$output[,mv,],
                 replicates = r$parameters$replicates, nt = nt)
  hospital <- odin_sv(r$output[,ox,],
                      replicates = r$parameters$replicates, nt = nt)
  deaths <- odin_sv(r$output[,index$delta_D,],
                    replicates = r$parameters$replicates, nt = nt)
  cumulative_deaths <- odin_sv(r$output[,index$D,],
                               replicates = r$parameters$replicates, nt = nt)


  # collect into a long data frame
  vars <- c("mild_cases", "hospital_cases", "deaths", "cumulative_deaths", "ICU", "hospital")
  df <- data.frame("date" = as.numeric(r$output[,index$time,]),
                   "replicate" = as.numeric(mapply(rep, seq_len(r$parameters$replicates), nt)),
                   "compartment" = as.character(mapply(rep, vars, nt*r$parameters$replicates)),
                   "y" = c(mild_cases, hospital_cases, deaths, cumulative_deaths, ICU, hospital))

  # Add date
  if(!is.null(date_0)){
    stopifnot(inherits(date_0, "Date"))
    df$date <- as.Date(df$date + date_0,
                       format = "%Y/%m/%d")
  }

  return(df)
}



#' @noRd
#' @importFrom stats median runif quantile
plot_calibration_cases <- function(df, data, forecast = 0) {

  # day
  df$day <- as.Date(as.character(df$date))

  # split to correct dates
  sub <- df[df$compartment %in% c("mild_cases", "hospital_cases") &
              df$date <=  Sys.Date() + forecast + 1,] %>%
    dplyr::group_by(.data$day, .data$replicate, .data$compartment) %>%
    dplyr::summarise(y = sum(.data$y), n=dplyr::n()) %>%
    dplyr::filter(.data$day <= Sys.Date() + forecast)

  pd_group <- dplyr::group_by(sub, .data$day, .data$compartment) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     ymax = .data$quants[[1]][3],
                     y = median(.data$y))

  # format cases
  data$cases <- rev(c(tail(data$cases,1), diff(rev(data$cases))))

  # Plot
  gg_cases <- ggplot2::ggplot(
    sub, ggplot2::aes(x = .data$day,
                      y = .data$y, col = .data$compartment,
                      group = interaction(.data$compartment, .data$replicate))) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_line(alpha = max(0.1, 1 / max(sub$replicate))) +
    ggplot2::geom_line(data = pd_group,
                       mapping = ggplot2::aes(group = .data$compartment),
                       size = 0.8) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(group = .data$compartment,
                                                ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                color = .data$compartment),
                         alpha = 0.2,
                         fill = NA,
                         linetype = "dashed",
                         size = 0.8,
                         show.legend = FALSE) +
    ggplot2::geom_point(data = data,
                        mapping = ggplot2::aes(x = .data$date, y = .data$cases, shape = "Cases"),
                        inherit.aes = FALSE) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Daily Cases") +
    ggplot2::scale_color_discrete(name = "Estimated", labels = c("Hospital Cases","Mild Cases")) +
    ggplot2::scale_fill_discrete(name = "Estimated", labels = c("Hospital Cases","Mild Cases")) +
    ggplot2::scale_shape_discrete(name = "Reported") +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))

  gg_cases

}

#' @noRd
#' @importFrom stats median runif
plot_calibration_cases_barplot <- function(df, data, forecast = 0) {

  # day
  df$day <- as.Date(as.character(df$date))

  # split to correct dates
  sub <- df[df$compartment %in% c("mild_cases", "hospital_cases") &
              df$date <=  Sys.Date() + forecast + 1,] %>%
    dplyr::group_by(.data$day, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y)) %>%
    dplyr::filter(.data$day <= Sys.Date() + forecast)


  pd_group <- dplyr::group_by(sub, .data$day) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.25, 0.5, 0.75, 0.975))),
                     ymin = .data$quants[[1]][1],
                     ymax = .data$quants[[1]][5],
                     yinner_min = .data$quants[[1]][2],
                     yinner_max = .data$quants[[1]][4],
                     y = median(.data$y))

  # format cases
  data$cases <- rev(c(tail(data$cases,1), diff(rev(data$cases))))

  # Plot
  gg_cases <- ggplot2::ggplot(sub, ggplot2::aes(x = .data$day,
                                                y = .data$y,
                                                col = .data$compartment)) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                fill = "Estimated"),
                         color = "white",
                         alpha = 0.2,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$yinner_min,
                                                ymax = .data$yinner_max,
                                                fill = "Estimated"),
                         color = "white",
                         alpha = 0.8,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_bar(data = data,
                      mapping = ggplot2::aes(x = .data$date, y = .data$cases,
                                             fill = "Reported"),
                      stat = "identity",
                      show.legend = TRUE,
                      inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::ylab("Daily Number of Infections") +
    ggplot2::theme_bw()  +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_fill_manual(name = "", labels = (c("Estimated", "Reported")),
                               values = (c("#3f8ea7","#c59e96"))) +
    ggplot2::scale_x_date(date_breaks = "2 week", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black")
    )

  gg_cases

}

#' @noRd
plot_calibration_healthcare <- function(df, data, forecast = 14) {

  # day
  df$day <- as.Date(as.character(df$date))

  # split to correct dates
  sub <- df[df$compartment %in%c("ICU", "hospital") &
              df$date <=  Sys.Date() + forecast + 1,] %>%
    dplyr::group_by(.data$day, .data$replicate, .data$compartment) %>%
    dplyr::summarise(y = sum(.data$y), n=dplyr::n()) %>%
    dplyr::filter(.data$day <= Sys.Date() + forecast)

  pd_group <- dplyr::group_by(sub, .data$day, .data$compartment) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     y = median(.data$y),
                     ymax = .data$quants[[1]][3])

  # Plot
  gg_healthcare <- ggplot2::ggplot(sub, ggplot2::aes(x = .data$day,
                                                     y = .data$y, col = .data$compartment,
                                                     group = interaction(.data$compartment, .data$replicate))) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_line(alpha = max(0.2, 1 / max(sub$replicate))) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(group = .data$compartment,
                                                ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                color = .data$compartment),
                         fill = NA,
                         alpha = 0.2,
                         linetype = "dashed",
                         size = 0.8,
                         show.legend = FALSE) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Healthcare Demand") +
    ggplot2::scale_color_discrete(name = "Estimated", labels = c("Hospital Beds", "ICU Beds")) +
    ggplot2::scale_fill_discrete(name = "Estimated", labels = c("Hospital Beds", "ICU Beds")) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d", limits = c(Sys.Date()-14, Sys.Date() + forecast)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black")) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

  gg_healthcare

}

#' @noRd
plot_calibration_healthcare_barplot <- function(df, data, what = "ICU", forecast = 14) {

  # day
  df$day <- as.Date(as.character(df$date))

  # split to correct dates
  sub <- df[df$compartment %in% what &
              df$date <=  Sys.Date() + forecast + 1,] %>%
    dplyr::group_by(.data$day, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y), n=dplyr::n()) %>%
    dplyr::filter(.data$day <= Sys.Date() + forecast)

  pd_group <- dplyr::group_by(sub, .data$day) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     y = median(.data$y),
                     ymax = .data$quants[[1]][3])

  # y axis
  if (what == "ICU") {
    title <- "ICU Demand"
  } else if(what == "hospital") {
    title <- "Hospital Bed Demand"
  }

  # Plot
  gg_healthcare <- ggplot2::ggplot(sub, ggplot2::aes(x = .data$day,
                                                     y = .data$y)) +
  ggplot2::geom_bar(data = pd_group,
                    mapping = ggplot2::aes(x = .data$day, y = .data$y, fill = "what"),
                    stat = "identity",
                    show.legend = TRUE,
                    inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::ylab(title) +
    ggplot2::theme_bw()  +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_fill_manual(name = "", labels = rev(c("Estimated")),
                               values = c("#3f8ea7")) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d", limits = c(Sys.Date()-7, Sys.Date() + forecast)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))

  gg_healthcare
}

#' @noRd
plot_calibration_deaths_barplot <- function(df, data, forecast = 14, cumulative = FALSE) {

  # day
  df$day <- as.Date(as.character(df$date))

  # split to correct dates
  if(!cumulative) {
    sub <- df[df$compartment == "deaths" &
                df$date <=  Sys.Date() + forecast + 1,]  %>%
      dplyr::group_by(.data$day, .data$replicate) %>%
      dplyr::summarise(y = sum(.data$y), n=dplyr::n()) %>%
      dplyr::filter(.data$day <= Sys.Date() + forecast)

    title <- "Daily Deaths"

    # format deaths
    data$deaths <- rev(c(tail(data$deaths,1), diff(rev(data$deaths))))

  } else {
    sub <- df[df$compartment == "cumulative_deaths" &
                df$date <=  Sys.Date() + forecast + 1,]  %>%
      dplyr::group_by(.data$day, .data$replicate) %>%
      dplyr::summarise(y = sum(.data$y), n=dplyr::n()) %>%
      dplyr::filter(.data$day <= Sys.Date() + forecast)

    title <- "Cumulative Deaths"
  }

  pd_group <- dplyr::group_by(sub, .data$day) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.25, 0.5, 0.75, 0.975))),
                     ymin = round(.data$quants[[1]][1]),
                     ymax = round(.data$quants[[1]][5]),
                     yinner_min = round(.data$quants[[1]][2]),
                     yinner_max = round(.data$quants[[1]][4]),
                     y = median(.data$y),
                     n = dplyr::n())



  # Plot
  gg_healthcare <- ggplot2::ggplot(sub,
                                   ggplot2::aes(x = .data$day,
                                                y = .data$y,
                                                fill = .data$compartment)) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                fill = "Estimated"),
                         color = "white",
                         alpha = 0.2,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$yinner_min,
                                                ymax = .data$yinner_max,
                                                fill = "Estimated"),
                         color = "white",
                         alpha = 0.8,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_bar(data = data,
                      mapping = ggplot2::aes(x = .data$date, y = .data$deaths,
                                             fill = "Reported"),
                      stat = "identity",
                      show.legend = TRUE,
                      inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_vline(xintercept = max(data$date[which(data$deaths != 0)]), linetype = "dotted") +
    ggplot2::theme_bw()  +
    ggplot2::ylab(title) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d",
                          limits = c(data$date[max(which(data$deaths>0))]-7, Sys.Date() + forecast)) +
    ggplot2::scale_fill_manual(name = "", labels = (c("Estimated", "Reported")),
                               values = (c("#3f8ea7","#c59e96"))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))

  gg_healthcare

}
