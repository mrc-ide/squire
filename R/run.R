#' Run the SEEIR model
#'
#' @param R0 Basic reproduction number
#' @param tt_R0 Change time points for R0
#' @param dt Time step
#' @param init Data.frame of initial conditions
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
#' pop <- get_population("Afghanistan")
#' m1 <- run_SEEIR_model(population = pop$n, dt = 1,
#' R0 = 2, baseline_contact_matrix = contact_matrices[[1]],
#' contact_matrix_set=contact_matrices[[1]])
#' }
run_SEEIR_model <- function(R0 = 3, tt_R0 = 0,
                            dt = 0.1,
                            init = NULL,
                            dur_E  = 4.58, dur_I = 2.09,
                            population,
                            baseline_contact_matrix,
                            contact_matrix_set, tt_contact_matrix = 0,
                            time_period = 365, replicates = 10) {

  # Initialise initial conditions
  init <- init_check(init, population)
  # Standardise contact matrix set
  if(is.matrix(contact_matrix_set)){
    contact_matrix_set <- list(contact_matrix_set)
  }
  # Input checks
  mc <- matrix_check(population, baseline_contact_matrix, contact_matrix_set)
  stopifnot(length(R0) == length(tt_R0))
  stopifnot(length(contact_matrix_set) == length(tt_contact_matrix))
  tc <- lapply(list(tt_R0, tt_contact_matrix), check_time_change, time_period)
  pn1 <- pos_num(dt, "dt")
  pn2 <- pos_num(dur_E, "dur_E")
  pn3 <- pos_num(dur_I, "dur_I")
  pn4 <- pos_num(time_period, "time_period")
  pn5 <- pos_num(replicates, "replicates")

  # Convert and Generate Parameters As Required
  gamma_E <- 2 * 1 / dur_E
  gamma_I <- 1 / dur_I
  beta_set <- beta_est(dur_I, baseline_contact_matrix, R0)

  # Convert contact matrices to input matrices
  matrices_set <- matrix_set(contact_matrix_set, population)

  # Collate Parameters Into List
  pars <- list(S0 = init$S, E0 = init$E, E02 = init$E2, I0 = init$I, R0 = init$R,
               gamma_E = gamma_E, gamma_I = gamma_I,
               tt_beta = tt_R0, beta_set = beta_set,
               N_age = length(population),
               tt_matrix = tt_contact_matrix, mix_mat_set = matrices_set,
               dt = dt)

  # Running the Model
  mod <- SEIR(user = pars)
  t <- seq(from = 1, to = time_period/dt)
  m <- mod$run(t, replicate = replicates)
  results <- mod$transform_variables(m)

  # Summarise inputs
  parameters = list(R0 = R0, tt_R0 = tt_R0,
                dt = dt,
                init = init,
                dur_E  = dur_E, dur_I = dur_I,
                population = population,
                baseline_contact_matrix = baseline_contact_matrix,
                contact_matrix_set = contact_matrix_set,
                tt_contact_matrix = tt_contact_matrix,
                time_period = time_period, replicates = replicates)

  out <- list(output = results, parameters = parameters)
  out <- structure(out, class = "squire_simulation")
  return(out)
}
