#' Estimate beta parameter
#'
#' @param duration_infectiousness Duration of infectiousness (days)
#' @param mixing_matrix Mixing matrix
#' @param R0 Basic reproduction number
#'
#' @return Beta parameter
#' @export
#'
# #' @examples
beta_est_simple <- function(duration_infectiousness, mixing_matrix, R0) {
  if(length(duration_infectiousness) > 1){
    stop("duration_infectiousness must be of length = 1")
  }
  if(!is.numeric(duration_infectiousness) | length(duration_infectiousness) > 1 |
     duration_infectiousness < 0){
    stop("duration_infectiousness must be a positive numeric value")
  }
  if(!is.numeric(R0) | any(R0 < 0)){
    stop("R0 must be a positive numeric value")
  }
  if(!is.matrix(mixing_matrix)){
    stop("mixing_matrix must be a matrix")
  }

  ng_eigen <- Re(eigen(mixing_matrix)$values[1])
  beta <- R0/(ng_eigen * duration_infectiousness)
  return(beta)
}

#' Compute age-adjusted eigenvalue for mixing matrix
#'
#' @param dur_IMild Duration of mild infectiousness (days)
#' @param dur_ICase Delay between symptom onset and requiring hospitalisation (days)
#' @param prob_hosp Probability of hospitilisation by ages
#' @param mixing_matrix Mixing matrix
#'
#' @return Eigenvalue
#' @export
#'
adjusted_eigen <- function(dur_IMild, dur_ICase, prob_hosp, mixing_matrix) {
  # assertions
  assert_single_pos(dur_ICase, zero_allowed = FALSE)
  assert_single_pos(dur_IMild, zero_allowed = FALSE)
  assert_numeric(prob_hosp)
  assert_numeric(mixing_matrix)
  assert_square_matrix(mixing_matrix)
  assert_same_length(mixing_matrix[,1], prob_hosp)

  if(sum(is.na(prob_hosp)) > 0) {
    stop("prob_hosp must not contain NAs")
  }

  if(sum(is.na(mixing_matrix)) > 0) {
    stop("mixing_matrix must not contain NAs")
  }

  relative_R0_by_age <- prob_hosp*dur_ICase + (1-prob_hosp)*dur_IMild
  Re(eigen(mixing_matrix*relative_R0_by_age)$values[1])
}

#' Estimate beta parameter for explicit model
#'
#' @param dur_IMild Duration of mild infectiousness (days)
#' @param dur_ICase Delay between symptom onset and requiring hospitalisation (days)
#' @param prob_hosp Probability of hospitilisation by ages
#' @param mixing_matrix Mixing matrix
#' @param R0 Basic reproduction number
#'
#' @return Beta parameter
#' @export
#'
# #' @examples
beta_est_explicit <- function(dur_IMild, dur_ICase, prob_hosp, mixing_matrix, R0) {
  assert_pos(R0, zero_allowed = FALSE)
  R0 / adjusted_eigen(dur_IMild, dur_ICase, prob_hosp, mixing_matrix)
}

#' Estimate beta for a squire model using model parameters
#' @param squire_model A squire model. Default = \code{explicit_SEEIR()}
#' @param model_params Squire model parameters, from a call to
#'   \code{generate_parameters()}.
#' @param R0 Basic reproduction number
#'
#' @return Beta parameter
beta_est <- function(squire_model, model_params, R0) {

  # asserts
  assert_custom_class(squire_model, "squire_model")
  assert_custom_class(model_params, "squire_parameters")
  assert_pos(R0)

  # Calculate beta
  if (class(squire_model)[1] == "explicit_SEEIR_model") {

    mat <- process_contact_matrix_scaled_age(model_params$contact_matrix_set[[1]],
                                             model_params$population)

    assert_custom_class(model_params, "explicit_SEEIR_parameters")
    new_beta <- squire_model$generate_beta_func(dur_IMild = 1/model_params$gamma_IMild,
                                                dur_ICase = 2/model_params$gamma_ICase,
                                                prob_hosp = model_params$prob_hosp,
                                                mixing_matrix = mat,
                                                R0 = R0)

  } else if (class(squire_model)[1] == "simple_SEEIR_model") {

    mat <- process_contact_matrix(model_params$contact_matrix_set[[1]],
                                             model_params$population)

    assert_custom_class(model_params, "simple_SEEIR_parameters")
    new_beta <- squire_model$generate_beta_func(duration_infectiousness = 1/model_params$gamma_I,
                                                mixing_matrix = mat,
                                                R0 = R0)

  } else if (class(squire_model)[1] == "apothecary_model") {

    mat <- process_contact_matrix_scaled_age(model_params$contact_matrix_set[[1]],
                                             model_params$population)

    new_beta <- squire_model$generate_beta_func(dur_IAsymp = 1/model_params$gamma_IAsymp,
                                                dur_IMild = 1/model_params$gamma_IMild,
                                                dur_ICase = 2/model_params$gamma_ICase,
                                                rel_inf_asymp = model_params$rel_inf_asymp,
                                                rel_inf_mild = model_params$rel_inf_mild,
                                                prob_asymp = model_params$prob_asymp,
                                                prob_hosp = model_params$prob_hosp,
                                                mixing_matrix = mat,
                                                R0 = R0)
  }

  return(new_beta)

}
