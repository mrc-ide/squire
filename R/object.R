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

#' Upper CI plot helper
#'
#' @param y Vector of values
#'
#' @return Upper quantile
uci <- function(y){
  quantile(y, 0.975)
}
#' Lower CI plot helper
#'
#' @param y Vector of values
#'
#' @return Lower quantile
lci <- function(y){
  quantile(y, 0.025)
}

#' squire simulation plot
#'
#' @param x An iccm_simulation object
#' @param replicates Plot replicates
#' @param ci Plot 2.5 and 97.5 quantiles for each compartment
#' @param var_select Vector of variable names to plot (default is all)
#' @param summary_f Function to summarise each compartment  passed to the \code{fun} argument of \code{\link[ggplot2]{stat_summary}}
#' @param ... additional arguments affecting the plot produced.
#'
#' @export
plot.squire_simulation <- function(x, replicates = FALSE, ci = FALSE,
                                   var_select = NULL,
                                   summary_f = mean, ...){

  # Check output is transformed
  if(!is.null(x$parameters$output_transform)){
    if(!x$parameters$output_transform) {
      stop("Plotting does not work with untransformed output, please run
           the model with output_transform = TRUE")
    }
  }

  # Convert output to long format
  pd <- long_output(x$output, var_select) %>%
    dplyr::group_by(.data$t, .data$compartment, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y))

  # Plot
  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$t, y = .data$y, col = .data$compartment,
                                        fill = .data$compartment,
                                        group = interaction(.data$compartment, .data$replicate)))
  # Add lines for individual draws
  if(replicates){
    p <- p + ggplot2::geom_line(alpha = max(0.2, 1 / x$parameters$replicates))
  }

  # Add summary across groups
  if(!is.null(summary_f)){
    if(x$parameters$replicates < 10){
      warning("Summary statistic estimated from <10 replicates")
    }
    p <- p + ggplot2::stat_summary(fun = summary_f, geom = "line",
                                   ggplot2::aes(group = .data$compartment))
  }

  # Add 2.5%, 97.5% quantile ribbon
  if(ci){
    if(x$parameters$replicates < 10){
      warning("Confidence bounds estimated from <10 replicates")
    }
    p <- p + ggplot2::stat_summary(ggplot2::aes(group = .data$compartment),
                                   col = NA, geom = 'ribbon', fun.max = uci,
                                   fun.min = lci, alpha = 0.3)
  }

  # Add remaining formatting
  p <- p +
    ggplot2::scale_color_discrete(name = "") +
    ggplot2::scale_fill_discrete(guide = FALSE) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("N") +
    ggplot2::theme_bw()
  return(p)
}
