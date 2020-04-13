
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

#' @noRd
#' @importFrom stats median runif
plot_calibration_cases <- function(df, data, forecast = 0) {

  # split to correct dates
  sub <- df[df$compartment %in% c("n_E2_IMild", "n_E2_ICase1") &
              df$date <=  Sys.Date() + forecast,]

  pd_group <- dplyr::group_by(sub, .data$date, .data$compartment) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     ymax = .data$quants[[1]][3],
                     y = median(.data$y))

  # format cases
  data$cases <- rev(c(tail(data$cases,1), diff(rev(data$cases))))

  # Plot
  gg_cases <- ggplot2::ggplot(
    sub, ggplot2::aes(x = as.Date(.data$date, origin = origin),
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
    ggplot2::scale_color_discrete(name = "Predicted", labels = c("Hospital Cases","Mild Cases")) +
    ggplot2::scale_fill_discrete(name = "Predicted", labels = c("Hospital Cases","Mild Cases")) +
    ggplot2::scale_shape_discrete(name = "Observed") +
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

  # split to correct dates
  sub <- df[df$compartment %in% c("infections") &
              df$date <=  Sys.Date() + forecast,]

  pd_group <- dplyr::group_by(sub, .data$date) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.25, 0.5, 0.75, 0.975))),
                     ymin = .data$quants[[1]][1],
                     ymax = .data$quants[[1]][5],
                     yinner_min = .data$quants[[1]][2],
                     yinner_max = .data$quants[[1]][4],
                     y = median(.data$y))

  # format cases
  data$cases <- rev(c(tail(data$cases,1), diff(rev(data$cases))))

  # Plot
  gg_cases <- ggplot2::ggplot(sub, ggplot2::aes(x = .data$date,
                                                y = .data$y,
                                                col = .data$compartment)) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                fill = "Predicted"),
                         color = "white",
                         alpha = 0.2,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$yinner_min,
                                                ymax = .data$yinner_max,
                                                fill = "Predicted"),
                         color = "white",
                         alpha = 0.8,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_bar(data = data,
                      mapping = ggplot2::aes(x = .data$date, y = .data$cases,
                                             fill = "Observed"),
                      stat = "identity",
                      show.legend = TRUE,
                      inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::ylab("Daily Number of Infections") +
    ggplot2::theme_bw()  +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_fill_manual(name = "", labels = rev(c("Predicted", "Observed")),
                               values = rev(c("#3f8ea7","#c59e96"))) +
    ggplot2::scale_x_date(date_breaks = "2 week", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   legend.position = "top"
    )

  gg_cases

}

#' @noRd
plot_calibration_healthcare <- function(df, data, forecast = 14) {

  # split to correct dates
  sub <- df[df$compartment %in% c("ICU", "hospital", "deaths") &
              df$date <=  Sys.Date() + forecast,]

  pd_group <- dplyr::group_by(sub, .data$date, .data$compartment) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     y = median(.data$y),
                     ymax = .data$quants[[1]][3])

  # format cases
  data$deaths <- rev(c(tail(data$deaths,1), diff(rev(data$deaths))))


  # Plot
  gg_healthcare <- ggplot2::ggplot(sub, ggplot2::aes(x = as.Date(.data$date, origin = origin),
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
                         fill = NA,
                         alpha = 0.2,
                         linetype = "dashed",
                         size = 0.8,
                         show.legend = FALSE) +
    ggplot2::geom_point(data = data,
                        mapping = ggplot2::aes(x = .data$date, y = .data$deaths, shape = "Deaths"),
                        inherit.aes = FALSE) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Daily Deaths / Healthcare Demands") +
    ggplot2::scale_color_discrete(name = "Predicted", labels = c("Deaths", "Hospital Beds", "ICU Beds")) +
    ggplot2::scale_fill_discrete(name = "Predicted", labels = c("Deaths", "Hospital Beds", "ICU Beds")) +
    ggplot2::scale_shape_discrete(name = "Observed") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(date_breaks = "2 week", date_labels = "%b %d", limits = c(Sys.Date()-7, Sys.Date() + forecast)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))

  gg_healthcare

}


#' @noRd
plot_calibration_healthcare_barplot <- function(df, data, forecast = 14) {

  # split to correct dates
  sub <- df[df$compartment == "deaths" &
              df$date <=  Sys.Date() + forecast,]

  pd_group <- dplyr::group_by(sub, .data$date, .data$compartment) %>%
    dplyr::summarise(quants = list(quantile(.data$y, c(0.025, 0.25, 0.5, 0.75, 0.975))),
                     ymin = round(.data$quants[[1]][1]),
                     ymax = round(.data$quants[[1]][5]),
                     yinner_min = round(.data$quants[[1]][2]),
                     yinner_max = round(.data$quants[[1]][4]),
                     y = median(.data$y))

  # format cases
  data$deaths <- rev(c(tail(data$deaths,1), diff(rev(data$deaths))))

  # Plot
  gg_healthcare <- ggplot2::ggplot(sub,
                                   ggplot2::aes(x = as.Date(.data$date, origin = origin),
                                                y = .data$y, fill = .data$compartment,
                                                group = .data$compartment)) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                fill = "Predicted"),
                         color = "white",
                         alpha = 0.2,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(ymin = .data$yinner_min,
                                                ymax = .data$yinner_max,
                                                fill = "Predicted"),
                         color = "white",
                         alpha = 0.8,
                         size = 0,
                         show.legend = TRUE) +
    ggplot2::geom_bar(data = data,
                      mapping = ggplot2::aes(x = .data$date, y = .data$deaths,
                                             fill = "Observed"),
                      stat = "identity",
                      show.legend = TRUE,
                      inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_vline(xintercept = data$date[which(data$deaths != 0)], linetype = "dotted") +
    ggplot2::theme_bw()  +
    ggplot2::ylab("Daily Deaths") +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d",
                          limits = c(data$date[max(which(data$deaths>0))]-7, Sys.Date() + forecast)) +
    ggplot2::scale_fill_manual(name = "", labels = rev(c("Predicted", "Observed")),
                               values = rev(c("#3f8ea7","#c59e96"))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))

  gg_healthcare

}
