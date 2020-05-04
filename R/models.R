#' Create an explicit model
#'
#' @title Explicit SEEIR model creation.
#'
#' We will use this structure to ensure that model fitting is flexible in the
#' future as more models are added
#'
#' @export
explicit_model <- function() {

  model_class <- "explicit_SEEIR_model"
  compare_model <- function(model, pars_obs, data) {
    compare_output(model, pars_obs, data, type=model_class)
  }

  explicit_model <- list(odin_model = explicit_SEIR,
                         generate_beta_func = beta_est_explicit,
                         parameter_func = parameters_explicit_SEEIR,
                         run_func = run_explicit_SEEIR_model,
                         compare_model = compare_model)
  class(explicit_model) <- c(model_class, "stochastic", "squire_model")
  explicit_model

}

#' Create a simple  model
#'
#' @title Simple SEEIR model creation.
#'
#' We will use this structure to ensure that model fitting is flexible in the
#' future as more models are added
#'
simple_model <- function() {

  model_class <- "simple_SEEIR_model"
  compare_model <- function(model, pars_obs, data) {
    compare_output(model, pars_obs, data, type=model_class)
  }

  simple_model <- list(odin_model = simple_SEIR,
                         generate_beta_func = beta_est_simple,
                         parameter_func = parameters_simple_SEEIR,
                         run_func = run_simple_SEEIR_model,
                         compare_model = compare_model)
  class(simple_model) <- c(model_class, "stochastic", "squire_model")
  simple_model

}

#' Create a simple  model
#'
#' @title Simple SEEIR model creation.
#'
#' We will use this structure to ensure that model fitting is flexible in the
#' future as more models are added
#'
deterministic_model <- function() {

  model_class <- "explicit_SEEIR_model"
  compare_model <- function(model, pars_obs, data) {
    compare_output(model, pars_obs, data, type=model_class)
  }

  # these two params have different names and this is the quickest fix
  param_func <- function(...){
    parms <- parameters_explicit_SEEIR(...)
    parms$gamma_R <- parms$gamma_IMild
    parms$gamma_hosp <- parms$gamma_ICase
    parms$hosp_bed_capacity <- parms$hosp_beds
    parms$ICU_bed_capacity <- parms$ICU_beds
    class(parms) <- c("explicit_SEEIR_parameters", "squire_parameters")
    return(parms)
  }

  det_model <- list(odin_model = explicit_SEIR_deterministic,
                       generate_beta_func = beta_est_explicit,
                       parameter_func = param_func,
                       run_func = run_deterministic_SEIR_model,
                       compare_model = compare_model)
  class(det_model) <- c(model_class, "deterministic", "squire_model")
  det_model

}
