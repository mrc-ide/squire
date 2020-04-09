#' Calibrate Model
#'
#' @details Fit the explicit_SEEIR model to time series of deaths
#'
#' @param squire_calibration \code{squire_calibration} object produced from
#'   from \code{\link{calibrate_output_parsing}}
#' @param what What plotting type are we plotting. Options are \code{cases}
#'   (default) or \code{healthcare}.
#'
#' @importFrom utils tail
#' @importFrom stats rbinom time quantile
#'
#' @return List of unformatted odin outputs with the date
plot.squire_calibration <- function(squire_calibration, what = "cases") {

  # assert checks
  assert_custom_class(squire_calibration, "squire_calibration")
  assert_string(what)

  # get the object for plotting
  df <- squire_calibration$df
  data <- squire_calibration$data

  # what are we plotting
  if(what == "cases") {
    gg <- plot_calibration_cases(df, data)
  } else if (what == "healthcare") {
    gg <- plot_calibration_healthcare(df, data)
  } else {
    stop("what must be one of cases or healthcare")
  }

  return(gg)
}


#' @noRd
plot_calibration_cases <- function(df, data, forward = 0) {

  # split to correct dates
  sub <- df[df$variable %in% c("mild_cases", "hospital_cases") &
              df$date <=  Sys.Date() + forward,]

  pd_group <- dplyr::group_by(sub, .data$date, .data$variable) %>%
    dplyr::summarise(quants = list(quantile(.data$value, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     ymax = .data$quants[[1]][3],
                     value = mean(.data$value))

  # format cases
  data$cases <- rev(c(tail(data$cases,1), diff(rev(data$cases))))

  # Plot
  gg_cases <- ggplot2::ggplot(
    sub, ggplot2::aes(x = as.Date(.data$date, origin = "1970-01-01 UTC"),
                      y = .data$value, col = .data$variable,
                      group = interaction(.data$variable, .data$replicate))) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_line(alpha = max(0.4, 1 / max(sub$replicate))) +
    ggplot2::geom_line(data = pd_group,
                       mapping = ggplot2::aes(group = .data$variable),
                       size = 0.8) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(group = .data$variable,
                                                ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                fill = .data$variable),
                         alpha = 0.2,
                         size = 0.8) +
    ggplot2::geom_point(data = data,
                        mapping = ggplot2::aes(x = .data$date, y = .data$cases),
                        inherit.aes = FALSE) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Cumulative Cases") +
    ggplot2::scale_color_discrete(name = "", labels = c("Hospital Cases","Mild Cases")) +
    ggplot2::scale_fill_discrete(name = "", labels = c("Hospital Cases","Mild Cases")) +
    ggplot2::theme_bw()

  invisible(gg_cases)

}


#' @noRd
plot_calibration_healthcare <- function(df, data, forward = 14) {

  # split to correct dates
  sub <- df[df$variable %in% c("icu", "hospital_bed", "deaths") &
              df$date <=  Sys.Date() + forward,]

  pd_group <- dplyr::group_by(sub, .data$date, .data$variable) %>%
    dplyr::summarise(quants = list(quantile(.data$value, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     value = mean(.data$value),
                     ymax = .data$quants[[1]][3])

  # Plot
  gg_healthcare <- ggplot2::ggplot(
    sub, ggplot2::aes(x = as.Date(.data$date, origin = "1970-01-01 UTC"),
                      y = .data$value, col = .data$variable,
                      group = interaction(.data$variable, .data$replicate))) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed") +
    ggplot2::geom_line(alpha = max(0.4, 1 / max(sub$replicate))) +
    ggplot2::geom_line(data = pd_group,
                       mapping = ggplot2::aes(group = .data$variable),
                       size = 0.8) +
    ggplot2::geom_ribbon(data = pd_group,
                         mapping = ggplot2::aes(group = .data$variable,
                                                ymin = .data$ymin,
                                                ymax = .data$ymax,
                                                fill = .data$variable),
                         alpha = 0.2,
                         size = 0.8) +
    ggplot2::geom_point(data = data,
               mapping = ggplot2::aes(x = .data$date, y = c(.data$deaths[1],diff(.data$deaths))),
               inherit.aes = FALSE) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Cumulative Deaths") +
    ggplot2::scale_color_discrete(name = "", labels = c("Deaths", "Hospital Beds", "ICU Beds")) +
    ggplot2::scale_fill_discrete(name = "", labels = c("Deaths", "Hospital Beds", "ICU Beds")) +
    ggplot2::theme_bw() +
    ggplot2::xlim(c(Sys.Date()-7, Sys.Date()+forward))

  invisible(gg_healthcare)

}
