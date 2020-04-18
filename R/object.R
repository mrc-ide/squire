#' Check object is an squire_simulation
#'
#' @param x an onject
check_squire <- function(x){
  if(!methods::is(x)[1] == "squire_simulation"){
    stop("Object must be a squire_simulation")
  }
}

#' squire simulation summary
#'
#' @param object An squire_simulation object
#' @param ... additional arguments affecting the summary produced.
#'
#' @export
summary.squire_simulation <- function(object, ...){
  cat(crayon::cyan("*~*~*~*~ squire simulation ~*~*~*~*\n"))
  cat(crayon::green("Total population ="), sum(object$parameters$population), "\n")
  cat(crayon::green("Number of age-clases ="), length(object$parameters$population), "\n")
  t <- object$parameters$time_period
  t <- ifelse(t > 365, paste(round(t / 365, 2), "years"),
              paste(t, "days"))
  cat(crayon::green("Simulation period = "), t, "\n")
  cat(crayon::green("R0 ="), object$parameters$R0, "\n")
}

#' squire simulation print
#'
#' @param x An iccm_simulation object
#' @param ... additional arguments affecting the summary produced.
#'
#' @export
print.squire_simulation <- function(x, ...){
  summary(x)
  invisible(x)
}


#' @noRd
squire_simulation_plot_prep <- function(x,
                                        var_select,
                                        q = c(0.025, 0.975),
                                        summary_f = mean,
                                        x_var = "t",
                                        ...) {

  pd <- format_output(x, var_select = var_select, ...)

  pd <- pd %>%
    dplyr::mutate(x = .data[[x_var]])

  # t sometimes seems to be being rounded weirdly
  if(x_var == "t") {
    pd$x <- round(pd$x, ceiling(log10(1/x$parameters$dt)))
  }

  # Format summary data
  pds <- pd %>%
    dplyr::group_by(.data$x, .data$compartment) %>%
    dplyr::summarise(ymin = stats::quantile(.data$y, q[1]),
                     ymax = stats::quantile(.data$y, q[2]),
                     y = summary_f(.data$y))

  return(list(pd = pd, pds = pds))

}

#' squire simulation plot
#'
#' @param x An iccm_simulation object
#' @param replicates Plot replicates
#' @param summarise Logical, add summary line
#' @param ci logical add confidence interval ribbon
#' @param q Quantiles for upper and lower of interval ribbon
#' @param var_select Vector of variable names to plot (default is all)
#' @param summary_f Function to summarise each compartment
#'   passed to the \code{fun} argument of \code{\link[ggplot2]{stat_summary}}
#' @param x_var X variable to use for plotting (default is \code{"t"},
#'   but can be set to, \code{"date"}, if \code{date_0} provided), which will
#'   cause the date to be plotted rather than time.
#' @param ... additional arguments affecting the plot produced.
#'
#' @export
plot.squire_simulation <- function(x, var_select = NULL,
                                   replicates = FALSE,
                                   summarise = TRUE,
                                   ci = TRUE,
                                   q = c(0.025, 0.975),
                                   summary_f = mean,
                                   x_var = "t", ...){


  # summarise accordingly
  pds <- squire_simulation_plot_prep(x = x,
                                     x_var = x_var,
                                     var_select = var_select,
                                     q = q,
                                     summary_f = summary_f,
                                     ...)
  pd <- pds$pd
  pds <- pds$pds

  # Plot
  p <- ggplot2::ggplot()

  # Add lines for individual draws
  if(replicates){
    p <- p + ggplot2::geom_line(data = pd,
                                ggplot2::aes(x = .data$x,
                                             y = .data$y,
                                             col = .data$compartment,
                                             group = interaction(.data$compartment, .data$replicate)),
                                alpha = max(0.2, 1 / x$parameters$replicates))
  }

  if(summarise){
    if(x$parameters$replicates < 10){
      warning("Summary statistic estimated from <10 replicates")
    }
    p <- p + ggplot2::geom_line(data = pds,
                                ggplot2::aes(x = .data$x, y = .data$y,
                                             col = .data$compartment))
  }

  if(ci){
    if(x$parameters$replicates < 10){
      warning("Confidence bounds estimated from <10 replicates")
    }
    p <- p + ggplot2::geom_ribbon(data = pds,
                                  ggplot2::aes(x = .data$x,
                                               ymin = .data$ymin,
                                               ymax = .data$ymax,
                                                    fill = .data$compartment),
                                  alpha = 0.25, col = NA)
  }

  # Add remaining formatting
  p <- p +
    ggplot2::scale_color_discrete(name = "") +
    ggplot2::scale_fill_discrete(guide = FALSE) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("N") +
    ggplot2::theme_bw() +
    ggplot2::guides(col = ggplot2::guide_legend(ncol = 2))

  return(p)
}
