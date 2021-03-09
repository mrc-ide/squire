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

  parameter_func <-  function(..., tt_vaccine, max_vaccine) {

    # build model parameters with no vaccine being passed through as
    # this is not the vaccine model
    parameters_explicit_SEEIR(...)

  }

  run_func <-  function(country,
                        contact_matrix_set,
                        tt_contact_matrix,
                        hosp_bed_capacity,
                        tt_hosp_beds,
                        ICU_bed_capacity,
                        tt_ICU_beds,
                        max_vaccine,
                        tt_vaccine,
                        population,
                        replicates,
                        day_return,
                        time_period,
                        ...) {

    # build model run with no vaccine being passed through as
    # this is not the vaccine model
    run_explicit_SEEIR_model(country = country,
                             contact_matrix_set = contact_matrix_set,
                             tt_contact_matrix = tt_contact_matrix,
                             hosp_bed_capacity = hosp_bed_capacity,
                             tt_hosp_beds = tt_hosp_beds,
                             ICU_bed_capacity = ICU_bed_capacity,
                             tt_ICU_beds = tt_ICU_beds,
                             population = population,
                             replicates = replicates,
                             day_return = day_return,
                             time_period = time_period,
                             ...)
  }


  explicit_model <- list(odin_model = explicit_SEIR,
                         generate_beta_func = beta_est_explicit,
                         parameter_func = parameter_func,
                         run_func = run_func,
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

  parameter_func <-  function(..., tt_vaccine, max_vaccine) {

    # build model parameters with no vaccine being passed through as
    # this is not the vaccine model
    parameters_explicit_SEEIR(...)
  }

  run_func <-  function(country,
                        contact_matrix_set,
                        tt_contact_matrix,
                        hosp_bed_capacity,
                        tt_hosp_beds,
                        ICU_bed_capacity,
                        tt_ICU_beds,
                        max_vaccine,
                        tt_vaccine,
                        population,
                        replicates,
                        day_return,
                        time_period,
                        ...) {

    # build model run with no vaccine being passed through as
    # this is not the vaccine model
    run_deterministic_SEIR_model(country = country,
                             contact_matrix_set = contact_matrix_set,
                             tt_contact_matrix = tt_contact_matrix,
                             hosp_bed_capacity = hosp_bed_capacity,
                             tt_hosp_beds = tt_hosp_beds,
                             ICU_bed_capacity = ICU_bed_capacity,
                             tt_ICU_beds = tt_ICU_beds,
                             population = population,
                             replicates = replicates,
                             day_return = day_return,
                             time_period = time_period,
                             ...)
  }

  det_model <- list(odin_model = explicit_SEIR_deterministic,
                       generate_beta_func = beta_est_explicit,
                       parameter_func = parameter_func,
                       run_func = run_func,
                       compare_model = compare_model)
  class(det_model) <- c(model_class, "deterministic", "squire_model")
  det_model

}
