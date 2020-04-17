#' Provide projections from calibrated simulations
#'
#' @param r Calibrated \code{{squire_simulation}} object.
#' @param mitigation Numeric vector for changes to R0 since calibration.
#' @param tt_mitigation Change time points for mitigation
#'
#' @export
projections <- function(r, mitigation, tt_mitigation) {

  assert_numeric(mitigation)
  assert_int(tt_mitigation)
  assert_custom_class(r, "squire_simulation")

  # odin model keys
  index <- odin_index(r$model)
  initials <- seq_along(r$model$initial()) + 1L
  ds <- dim(r$output)

  # what state time point do we want
  state_pos <- vapply(seq_len(ds[3]), function(x) {
    which(r$output[,"time",x] == 0)
  }, FUN.VALUE = numeric(1))

  # what are the remaining time points
  t_steps <- lapply(state_pos, function(x) {
    tail(seq_len(ds[1]), ds[1] - x)
  })

  # conduct mitigation runs
  out <- lapply(seq_len(ds[3]), function(x) {

    # work out what the last R0 was for this replicate
    last <- tail(which(r$parameters$tt_R0 < state_pos[x]), 1)
    last_R0 <- r$parameters$R0[last]

    # create new betas going forwards
    beta <- beta_est_explicit(dur_IMild = r$parameters$dur_IMild,
                              dur_ICase = r$parameters$dur_ICase,
                              prob_hosp = r$parameters$prob_hosp,
                              mixing_matrix = process_contact_matrix_scaled_age(
                                r$parameters$contact_matrix_set[[1]],
                                r$parameters$population),
                              R0 = last_R0*mitigation)

    # change these user params
    r$model$set_user(tt_beta = tt_mitigation)
    r$model$set_user(beta_set = beta)

    # run the model (N.B. every time i try to use the return_minimal or similar to speed this up it segfaults or similar)
    r$model$run(step = t_steps[[x]],
                y = as.numeric(r$output[state_pos[x], initials, x, drop=TRUE]),
                use_names = TRUE,
                replicate = 1)

  })

  for(i in seq_len(ds[3])) {
    r$output[t_steps[[i]], -1, i] <- out[[i]][, -1, 1]
  }

  return(r)

}
