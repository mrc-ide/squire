#' Extract All Model Ouputs
#'
#' @param x \code{squire_simulation} object representing the output from running
#'   \code{run_explicit_SEEIR_model(output_transform = TRUE)}
#'
#' @return output
#' @export
#'
# #' @examples

extract_all_outputs <- function(x) {

  # check input
  assert_custom_class(x, "squire_simulation")

  # Check output is transformed
  if(!is.null(x$parameters$output_transform)){
    if(!x$parameters$output_transform) {
      stop("Plotting does not work with untransformed output, please run
           the model with output_transform = TRUE")
    }
  }

  model_output <- x$output

  # Aggregate Model Outputs Over Compartments Referring to Same State
  t <- model_output$time[, 1]
  S <- model_output$S
  E <- model_output$E1 + model_output$E2
  IMild <- model_output$IMild
  ICase <- model_output$ICase1 + model_output$ICase2
  IOx <- model_output$IOxGetLive1 + model_output$IOxGetLive2 + model_output$IOxGetDie1 +
    model_output$IOxGetDie2 + model_output$IOxNotGetLive1 + model_output$IOxNotGetLive2 +
    model_output$IOxNotGetDie1 + model_output$IOxNotGetDie2
  IMV <- model_output$IMVGetLive1 + model_output$IMVGetLive2 + model_output$IMVGetDie1 +
    model_output$IMVGetDie2 + model_output$IMVNotGetLive1 + model_output$IMVNotGetLive2 +
    model_output$IMVNotGetDie1 + model_output$IMVNotGetDie2
  IRec <- model_output$IRec1 + model_output$IRec2
  R <- model_output$R
  D <- model_output$D

  # Output Aggregated Model Outputs as List
  output <- list(t = t,
                 S = S,
                 E = E,
                 IMild = IMild,
                 ICase = ICase,
                 IOx = IOx,
                 IMV = IMV,
                 IRec = IRec,
                 R = R,
                 D = D)

  return(output)
}



#' Extract Relevant Model Ouputs
#'
#' @inheritParams extract_all_outputs
#' @param output_required Character for specifc output you want out of the model
#'  c("infections", "deaths", "hospital" or "ICU")
#'
#' @return output
#' @export
#'
# #' @examples

extract_specific_output <- function(x, output_required) {

  # check input
  assert_custom_class(x, "squire_simulation")

  model_output <- x$output

  possible_outputs <- c("infections", "deaths", "hospital", "ICU")
  if (!(output_required %in% possible_outputs)) {
    stop("output_required must equal one of infections, deaths, hospital or ICU")
  }

  if (output_required == "infections") {

    infection_incidence <- model_output$n_E2_I
    output <- infection_incidence

  } else if (output_required == "deaths") {

    death_incidence <- model_output$delta_D
    output <- death_incidence

  } else if (output_required == "hospital") {

    hosp_occ <- model_output$IOxGetLive1 + model_output$IOxGetLive2 +
      model_output$IOxGetDie1 + model_output$IOxGetDie2 +
      model_output$IRec1 + model_output$IRec2
    output <- hosp_occ

  } else if (output_required == "ICU") {

    ICU_occ <- model_output$IMVGetLive1 + model_output$IMVGetLive2 +
      model_output$IMVGetDie1 + model_output$IMVGetDie2
    output <- ICU_occ

  }

  return(output)
}
