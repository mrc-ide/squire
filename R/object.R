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

#' squire simulation plot
#'
#' @param x An iccm_simulation object
#' @param ... additional arguments affecting the plot produced.
#'
#' @export
plot.squire_simulation <- function (x, ...){

  # Convert output to long format
  pd <- long_output(x$output) %>%
    dplyr::group_by(.data$t, .data$compartment, .data$replicate) %>%
    dplyr::summarise(y = sum(.data$y))

  pd_group <- dplyr::group_by(pd, .data$t, .data$compartment) %>%
    dplyr::summarise(y = mean(.data$y))
  # Plot
  ggplot2::ggplot(pd, ggplot2::aes(x = .data$t, y = .data$y, col = .data$compartment,
                                   group = interaction(.data$compartment, .data$replicate))) +
    ggplot2::geom_line(alpha = max(0.2, 1 / x$parameters$replicates)) +
    ggplot2::geom_line(data = pd_group,
                       mapping = ggplot2::aes(group = .data$compartment),
                       size = 1.2) +
    ggplot2::scale_color_discrete(name = "") +
    ggplot2::xlab("Time") +
    ggplot2::ylab("N") +
    ggplot2::theme_bw()

}
