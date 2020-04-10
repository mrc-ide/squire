#' Calibrated Model PPlotting
#'
#' @details Fit the explicit_SEEIR model to time series of deaths
#'
#' @param x \code{squire_calibration} object produced from
#'   from \code{\link{calibrate_output_parsing}}
#' @param ... additional arguments affecting the plot produced.
#' @param what What plotting type are we plotting. Options are \code{cases}
#'   (default) or \code{healthcare}.
#' @param forecast How many days forward should forecast plots be provided.
#'   Default = 0 days
#' @param ... Other parameters to be passed to internal plotting functions.
#'
#' @importFrom utils tail
#' @importFrom stats rbinom time quantile
#'
#' @export
#' @return List of unformatted odin outputs with the date
plot.squire_calibration <- function(x, ...,
                                    what = "cases", forecast = 0) {

  # assert checks
  assert_string(what)

  # get the object for plotting
  df <- x$df
  data <- x$data

  # what are we plotting
  if(what == "cases") {
    gg <- plot_calibration_cases(df = df, data = data, forecast = forecast, ...)
  } else if (what == "healthcare") {
    gg <- plot_calibration_healthcare(df = df, data = data, forecast = forecast, ...)
  } else {
    stop("what must be one of cases or healthcare")
  }

  return(gg)
}


#' @noRd
#' @importFrom stats median runif
plot_calibration_cases <- function(df, data, forecast = 0) {

  # split to correct dates
  sub <- df[df$variable %in% c("mild_cases", "hospital_cases") &
              df$date <=  Sys.Date() + forecast,]

  pd_group <- dplyr::group_by(sub, .data$date, .data$variable) %>%
    dplyr::summarise(quants = list(quantile(.data$value, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     ymax = .data$quants[[1]][3],
                     value = median(.data$value))

  # format cases
  data$cases <- rev(c(tail(data$cases,1), diff(rev(data$cases))))

  # Plot
  gg_cases <- ggplot2::ggplot(
    sub, ggplot2::aes(x = as.Date(.data$date, origin = "1970-01-01 UTC"),
                      y = .data$value, col = .data$variable,
                      group = interaction(.data$variable, .data$replicate))) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_line(alpha = max(0.1, 1 / max(sub$replicate))) +
    ggplot2::geom_line(data = pd_group,
                       mapping = ggplot2::aes(group = .data$variable),
                       size = 0.8) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(group = .data$variable,
                                                ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                color = .data$variable),
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
    ggplot2::theme_bw()

  invisible(gg_cases)

}


#' @noRd
plot_calibration_healthcare <- function(df, data, forecast = 14) {

  # split to correct dates
  sub <- df[df$variable %in% c("icu", "hospital_bed", "deaths") &
              df$date <=  Sys.Date() + forecast,]

  pd_group <- dplyr::group_by(sub, .data$date, .data$variable) %>%
    dplyr::summarise(quants = list(quantile(.data$value, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     value = median(.data$value),
                     ymax = .data$quants[[1]][3])

  # format cases
  data$deaths <- rev(c(tail(data$deaths,1), diff(rev(data$deaths))))

  # Plot
  gg_healthcare <- ggplot2::ggplot(
    sub, ggplot2::aes(x = as.Date(.data$date, origin = "1970-01-01 UTC"),
                      y = .data$value, col = .data$variable,
                      group = interaction(.data$variable, .data$replicate))) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_line(alpha = max(0.1, 1 / max(sub$replicate))) +
    ggplot2::geom_line(data = pd_group,
                       mapping = ggplot2::aes(group = .data$variable),
                       size = 0.8) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(group = .data$variable,
                                                ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                color = .data$variable),
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
    ggplot2::xlim(c(Sys.Date()-7, Sys.Date()+forecast))

  invisible(gg_healthcare)

}
