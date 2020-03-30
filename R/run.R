#' Run the SEEIR model
#'
#' @param R0 Basic reproduction number
#' @param tt_R0 Change time points for R0
#' @param dt Time step
#' @param S_init Initial value for S
#' @param E_init Initial value for E
#' @param I_init Initial value for I
#' @param R_init Initial value for R
#' @param dur_E Mean duration of incubation period (days)
#' @param dur_I Mean duration of infectious period (days)
#' @param population Population vector (for each age group)
#' @param baseline_contact_matrix Contact matrix (in absence of interventions). Used in estimation
#' of beta from R0 vector
#' @param contact_matrix_set Contact matrices used in simulation
#' @param tt_contact_matrix Time change points for matrix change
#' @param time_period Length of simulation
#' @param replicates Number of replicates
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' \dontrun{
#' m1 <- run_SEEIR_model(population = rep(1000, 16), dt = 1,
#' R0 = 2, baseline_contact_matrix = contact_matrices[[1]],
#' contact_matrix_set=contact_matrices[[1]])
#' }
run_SEEIR_model <- function(R0 = 3, tt_R0 = 0,
                            dt = 0.1,
                            S_init = NULL, E_init = 0, I_init = 1, R_init = 0,
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
  if(length(E_init) != N_age){
    E_init = rep(E_init, N_age)
  }
  if(length(I_init) != N_age){
    I_init = rep(I_init, N_age)
  }
  if(length(R_init) != N_age){
    R_init = rep(R_init, N_age)
  }

  # Collate Parameters Into List
  pars <- list(S0 = S_init, E0 = E_init, I0 = I_init, R0 = R_init,
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
