#' Extract Relevant Model Ouputs
#'
#' @param model_output squire_simulation object representing the output from running
#'   run_explicit_SEEIR
#' @param output_required The specific output you want out of the model ("infections", "deaths",
#'   "hospital" or "ICU")
#'
#' @return output
#' @export
#'
# #' @examples

extract_output <- function(model_output, output_required) {

  possible_outputs <- c("infections", "deaths", "hospital", "ICU")
  if (!(output_required %in% possible_outputs)) {
    stop("output_required must equal one of infections, deaths, hospital or ICU")
  }

  if (is(model_output) != "squire_simulation") {
    stop("model_output must be a squire_simulation object")
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
