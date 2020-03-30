

run_SEEIR_model <- function(R0 = 3, tt_R0 = 0,
                            dt = 0.1,
                            S_init = NULL, E0_init = 0, I_init = 1, R_init = 0,
                            dur_E  = 4.58, dur_I = 2.09,
                            population,
                            baseline_contact_matrix = contact_matrices[[1]],
                            contact_matrix_set, tt_contact_matrix = 0,
                            time_period = 365, replicates = 10) {

  # Initialise S
  if(is.null(S_init)){
    S_init <- population - 1
  }

  # Convert and Generate Parameters As Required
  gamma_E <- 2 * 1 / dur_E
  gamma_I <- 1 / dur_I
  beta_set <- beta_est(dur_I, baseline_contact_matrix, R0)

  # Convert contact matrices to input matrices
  matrices_set <- matrix_set(contact_matrix_set, population)

  N_age <- nrow(baseline_contact_matrix)
  if(length(E0_init) != N_age){
    E0_init = rep(E0_init, N_age)
  }
  if(length(I_init) != N_age){
    I_init = rep(I_init, N_age)
  }
  if(length(R_init) != N_age){
    R_init = rep(R_init, N_age)
  }

  # Collate Parameters Into List
  pars <- list(S0 = S_init, E0 = E0_init, I0 = I_init, R0 = R_init,
               gamma_E = gamma_E, gamma_I = gamma_I,
               tt_beta = tt_R0, beta_set = beta_set,
               N_age = N_age,
               tt_matrix = tt_contact_matrix, mix_mat_set = matrices_set,
               dt = dt)

  # Running the Model
  mod <- SEIR(user = pars)
  t <- seq(from = 1, to = time_period/dt)
  m <- mod$run(t, replicate = replicates)
  results <- mod$transform_variables(m)
  return(results)
}
