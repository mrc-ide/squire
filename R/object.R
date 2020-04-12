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
#' @param replicates Plot replicates
#' @param summarise Logical, add summary line
#' @param ci logical add confidence interval ribbon
#' @param q Quantiles for upper and lower of interval ribbon
#' @param var_select Vector of variable names to plot (default is all)
#' @param summary_f Function to summarise each compartment
#'   passed to the \code{fun} argument of \code{\link[ggplot2]{stat_summary}}
#' @param ... additional arguments affecting the plot produced.
#'
#' @export
plot.squire_simulation <- function(x, replicates = FALSE,
                                   summarise = TRUE,
                                   ci = TRUE,
                                   q = c(0.025, 0.975),
                                   var_select = NULL,
                                   summary_f = mean, ...){


  # Select variables
  if(!is.null(var_select)){
    if(!all(var_select %in% names(x$output))){
      stop("Selected variable are not all present in output")
    }
    x$output <- x$output[c("time", var_select)]
  }
  # Format data
  pd <- quick_long(x)

  # Format summary data
  pds <- pd %>%
    dplyr::group_by(.data$t, .data$compartment) %>%
    dplyr::summarise(ymin = quantile(.data$y, q[1]),
              ymax = quantile(.data$y, q[2]),
              y = summary_f(.data$y))

  # Plot
  p <- ggplot2::ggplot()

  # Add lines for individual draws
  if(replicates){
    p <- p + ggplot2::geom_line(data = pd,
                                ggplot2::aes(x = .data$t,
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
                                ggplot2::aes(x = .data$t, y = .data$y,
                                             col = .data$compartment))
  }

  if(ci){
    if(x$parameters$replicates < 10){
      warning("Confidence bounds estimated from <10 replicates")
    }
    p <- p + ggplot2::geom_ribbon(data = pds,
                                  ggplot2::aes(x = .data$t,
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
    ggplot2::theme_bw()


  return(p)

}

#' squire simulation plot
#'
#' @param x A \code{squire_untransform} object from
#'    \code{\link{untransformed_output}}
#' @param ... additional arguments affecting the plot produced.
#'
#' @export
plot.squire_untransform <- function (x, ...){

  pd_group <- dplyr::group_by(x$df, .data$time, .data$variable) %>%
    dplyr::summarise(quants = list(quantile(.data$value, c(0.025, 0.5, 0.975))),
                     ymin = .data$quants[[1]][1],
                     ymax = .data$quants[[1]][3],
                     value = median(.data$value))

  # Plot
  ggplot2::ggplot(
    x$df, ggplot2::aes(x = .data$time, y = .data$value, col = .data$variable,
                      group = interaction(.data$variable, .data$replicate))) +
    ggplot2::geom_line(alpha = max(0.1, 1 / x$r$parameters$replicates)) +
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
    ggplot2::xlab("Time") +
    ggplot2::ylab("Value") +
    ggplot2::theme_bw()

}


#' Explicit Output from non transformed run
#'
#' @details Parse output of \code{\link{run_explicit_SEEIR_model}} when
#' \code{output_transform} is FALSE
#'
#' @param r Output of \code{\link{run_explicit_SEEIR_model}} with
#'   \code{output_transform} is FALSE
#' @param compartments Specific compartments to extract. Default = NULL
#' @param deaths Should incident deaths be include. Default = TRUE
#' @param cases Should incident cases be include. Default = TRUE
#' @param beds Should ICU bed and hospital bed demand be include. Default = TRUE
#'
#' @return Data frame of case numbers, hospital beds, icu beds and deaths
#' @export
untransformed_output <- function(r, compartments = NULL,
                            deaths = TRUE,
                            cases = TRUE,
                            beds = TRUE) {

  ## Assertions
  assert_custom_class(r, "squire_simulation")
  if (r$parameters$output_transform) {
    stop("r must be created with run_explicit_SEEIR_model(..., output_transform = FALSE)")
  }
  if(is.null(compartments) && !deaths && !cases && !beds) {
    stop("Must provide compartment or have one of deaths, cases, beds as TRUE")
  }

  # get the index
  index <- odin_index(r$model)
  nt <- nrow(r$output)

  # index of icus
  mv <- unlist(index[c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
                       "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2")])

  # index of beds
  ox <- unlist(index[c("IOxGetLive1","IOxGetLive2","IOxGetDie1","IOxGetDie2",
                       "IOxNotGetLive1","IOxNotGetLive2","IOxNotGetDie1","IOxNotGetDie2")])

  # collet outputs as vectors
  time <- r$output[,index$time,1]

  # summaris
  mild_cases <- odin_sv(r$output[,index$n_E2_I,] - r$output[,index$n_E2_ICase1,],
                        replicates = r$parameters$replicates, nt = nt)
  hospital_cases <- odin_sv(r$output[,index$n_E2_ICase1,],
                            replicates = r$parameters$replicates, nt = nt)
  icu <- odin_sv(r$output[,mv,],
                 replicates = r$parameters$replicates, nt = nt)
  hospital_bed <- odin_sv(r$output[,ox,],
                          replicates = r$parameters$replicates, nt = nt)
  death_incidence <- odin_sv(r$output[,index$delta_D,],
                    replicates = r$parameters$replicates, nt = nt)

  # any specific compartment
  if (!is.null(compartments)) {
    indices <- index[compartments]
    values <- lapply(indices, function(x) {
      odin_sv(r$output[,x,], replicates = r$parameters$replicates, nt = nt)
    })
  } else {
    values <- list()
    compartments <- character(0)
  }

  # collect into a long data frame
  vars <- c("mild_cases", "hospital_cases", "deaths", "icu", "hospital_bed", compartments)
  df <- data.frame("time" = time,
                   "replicate" = as.numeric(mapply(rep, seq_len(r$parameters$replicates), nt)),
                   "variable" = as.character(mapply(rep, vars, nt*r$parameters$replicates)),
                   "value" = c(mild_cases, hospital_cases, death_incidence, icu, hospital_bed,
                               unlist(values)))

  # remove unrequested
  if (!cases) {
    df <- df[!df$variable %in% c("mild_cases", "hospital_cases"),]
  }
  if (!deaths) {
    df <- df[!df$variable %in% c("deaths"),]
  }
  if (!beds) {
    df <- df[!df$variable %in% c("icu", "hospital_bed"),]
  }

  gcu <- list("r" = r, "df" = df)
  class(gcu) <- "squire_untransform"
  return(gcu)

}
