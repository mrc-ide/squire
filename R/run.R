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
run_SEEIR_model <- function(R0 = 3,
                            tt_R0 = 0,
                            dt = 0.1,
                            init = NULL,
                            dur_E  = 4.58,
                            dur_I = 2.09,
                            population,
                            baseline_contact_matrix,
                            contact_matrix_set,
                            tt_contact_matrix = 0,
                            time_period = 365,
                            replicates = 10) {

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

#' -----------------------------------------------------------------------------
#' Run the explicit SEEIR model
#'
#' @param population Population vector (for each age group).
#' @param baseline_contact_matrix Contact matrix (in absence of interventions).
#'   Used in estimation of beta from R0 vector
#' @param contact_matrix_set Contact matrices used in simulation
#' @param tt_contact_matrix Time change points for matrix change. Default = 0
#' @param R0 Basic Reproduction Number. Default = 3
#' @param tt_R0 Change time points for R0. Default = 0
#' @param beta_set Alternative parameterisation via beta rather than R0.
#'   Default = NULL, which causes beta to be estimated from R0
#' @param time_period Length of simulation. Default = 365
#' @param dt Time Step. Default = 0.25
#' @param replicates  Number of replicates. Default = 10
#' @param init Data.frame of initial conditions. Default = NULL
#' @param prob_hosp probability of hospitalisation by age.
#'   Default = c(0.001127564, 0.000960857, 0.001774408, 0.003628171,
#'   0.008100662, 0.015590734, 0.024597885, 0.035377529,
#'   0.04385549, 0.058495518, 0.08747709, 0.109730508,
#'   0.153943118, 0.177242143, 0.221362219, 0.267628264)
#' @param prob_severe Probability of developing severe symptoms by age.
#'   Default = c(3.73755e-05, 3.18497e-05, 5.88166e-05, 0.000120264,
#'   0.000268514, 0.000516788, 0.00081535, 0.001242525,
#'   0.001729275, 0.002880196, 0.00598205, 0.010821894,
#'   0.022736324, 0.035911156, 0.056362032, 0.081467057)
#' @param prob_non_severe_death Probability of death from non severe infection.
#'   Default = c(0.0125702, 0.0125702, 0.0125702, 0.0125702,
#'   0.0125702, 0.0125702, 0.0125702, 0.013361147,
#'   0.015104687, 0.019164124, 0.027477519, 0.041762108,
#'   0.068531658, 0.105302319, 0.149305732, 0.20349534)
#' @param prob_severe_death Probability of death from severe infection.
#'   Default = c(0.5, 0.5, 0.5, 0.5,
#'   0.5, 0.5, 0.5, 0.5,
#'   0.5, 0.5, 0.5, 0.5,
#'   0.5, 0.5, 0.5, 0.5)
#' @param dur_E Mean duration of incubation period (days). Default = 4.58
#' @param dur_R Mean duration of mild infection (days). Default = 2.09
#' @param dur_hosp Mean duration of hospitilsation (days). Default = 5
#' @param dur_ox Mean duration of case requiring oxygen (days). Default = 6
#' @param dur_mv Mean duration of mechanical ventilation (days). Default = 5.5
#' @param dur_rec Mean duration of case recovering after ICU (days).  Default = 6
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' \dontrun{
#' pop <- get_population("Afghanistan")
#' m1 <- run_explicit_SEEIR_model(R0 = 2,
#' population = pop$n, dt = 1,
#' baseline_contact_matrix = contact_matrices[[1]],
#' contact_matrix_set=contact_matrices[[1]])
#' }
run_explicit_SEEIR_model <- function(

  # demography
  population,
  baseline_contact_matrix,
  contact_matrix_set,
  tt_contact_matrix = 0,

  # transmission
  R0 = 3,
  tt_R0 = 0,
  beta_set = NULL,

  # initial state, duration, reps
  time_period = 365,
  dt = 0.25,
  replicates = 10,
  init = NULL,

  # parameters
  prob_hosp = c(0.001127564, 0.000960857, 0.001774408, 0.003628171,
                0.008100662, 0.015590734, 0.024597885, 0.035377529,
                0.04385549, 0.058495518, 0.08747709, 0.109730508,
                0.153943118, 0.177242143, 0.221362219, 0.267628264),
  prob_severe = c(3.73755E-05, 3.18497E-05, 5.88166E-05, 0.000120264,
                  0.000268514, 0.000516788, 0.00081535, 0.001242525,
                  0.001729275, 0.002880196, 0.00598205, 0.010821894,
                  0.022736324, 0.035911156, 0.056362032, 0.081467057),
  prob_non_severe_death = c(0.0125702, 0.0125702, 0.0125702, 0.0125702,
                            0.0125702, 0.0125702, 0.0125702, 0.013361147,
                            0.015104687, 0.019164124, 0.027477519, 0.041762108,
                            0.068531658, 0.105302319, 0.149305732, 0.20349534),
  prob_severe_death = c(0.5, 0.5, 0.5, 0.5,
                        0.5, 0.5, 0.5, 0.5,
                        0.5, 0.5, 0.5, 0.5,
                        0.5, 0.5, 0.5, 0.5),
  dur_E  = 4.58,
  dur_R = 2.09,
  dur_hosp = 5,
  dur_ox = 6,
  dur_mv = 5.5,
  dur_rec = 6) {

  # Grab function arguments
  args <- as.list(environment())

  # Initail state and matrix formatting
  # ----------------------------------------------------------------------------

  # Initialise initial conditions
  init <- init_check_explicit(init, population)

  # Standardise contact matrix set
  if(is.matrix(contact_matrix_set)){
    contact_matrix_set <- list(contact_matrix_set)
  }

  # Convert contact matrices to input matrices
  matrices_set <- matrix_set(contact_matrix_set, population)

  # Input checks
  # ----------------------------------------------------------------------------
  mc <- matrix_check(population, baseline_contact_matrix, contact_matrix_set)
  stopifnot(length(R0) == length(tt_R0))
  stopifnot(length(contact_matrix_set) == length(tt_contact_matrix))
  tc <- lapply(list(tt_R0, tt_contact_matrix), check_time_change, time_period)
  pn1 <- pos_num(dt, "dt")
  pn2 <- pos_num(dur_E, "dur_E")
  pn3 <- pos_num(dur_R, "dur_R")
  pn4 <- pos_num(dur_hosp, "dur_hosp")
  pn5 <- pos_num(dur_ox, "dur_ox")
  pn6 <- pos_num(dur_mv, "dur_mv")
  pn7 <- pos_num(dur_rec, "dur_rec")
  pn8 <- pos_num(time_period, "time_period")
  pn9 <- pos_num(replicates, "replicates")

  assert_length(prob_hosp, length(population))
  assert_length(prob_severe, length(population))
  assert_length(prob_non_severe_death, length(population))
  assert_length(prob_severe_death, length(population))

  # Convert and Generate Parameters As Required
  # ----------------------------------------------------------------------------
  gamma_E = 2 * 1/dur_E
  gamma_R = 1/dur_R
  gamma_hosp = 2 * 1/dur_hosp
  gamma_ox = 2 * 1/dur_ox
  gamma_mv = 2 * 1/dur_mv
  gamma_rec = 2 * 1/dur_rec

  if (is.null(beta_set)) {
  beta_set <- beta_est_explicit(dur_R = dur_R, dur_hosp = dur_hosp,
                                prob_hosp = prob_hosp,
                                mixing_matrix = baseline_contact_matrix,
                                R0 = R0)
  }

  # Collate Parameters Into List
  pars <- list(N_age = length(population),
               S_0 = init$S,
               E1_0 = init$E1,
               E2_0 = init$E2,
               IMild_0 = init$IMild,
               ICase1_0 = init$ICase1,
               ICase2_0 = init$ICase2,
               IOx1_0 = init$IOx1,
               IOx2_0 = init$IOx1,
               IMV1_0 = init$IMV1,
               IMV2_0 = init$IMV2,
               IRec1_0 = init$IRec1,
               IRec2_0 = init$IRec2,
               R_0 = init$R,
               D_0 = init$D,
               gamma_E = gamma_E,
               gamma_R = gamma_R,
               gamma_hosp = gamma_hosp,
               gamma_ox = gamma_ox,
               gamma_mv = gamma_mv,
               gamma_rec = gamma_rec,
               prob_hosp = prob_hosp,
               prob_severe = prob_severe,
               prob_non_severe_death = prob_non_severe_death,
               prob_severe_death = prob_severe_death,
               tt_matrix = tt_contact_matrix,
               mix_mat_set = matrices_set,
               tt_beta = tt_R0,
               beta_set = beta_set,
               dt = dt)


  # Running the Model
  mod <- explict_SEIR(user = pars)
  t <- seq(from = 1, to = time_period/dt)
  m <- mod$run(t, replicate = replicates)
  results <- mod$transform_variables(m)

  # Summarise inputs
  parameters <- args
  parameters$beta_set <- beta_set

  out <- list(output = results, parameters = parameters)
  out <- structure(out, class = "squire_simulation")
  return(out)
}
