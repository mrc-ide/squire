#' Return the default probabilities for modelling
#' @return list of default probabilities
default_probs <- function() {
  prob_hosp <- c(
    0.000744192, 0.000634166,0.001171109, 0.002394593, 0.005346437 ,
    0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
    0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
    0.176634654 ,0.180000000)
  list(
    prob_hosp = prob_hosp,
    prob_severe = c(
      0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
      0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
      0.103612417, 0.149427991, 0.223777304,	0.306985918,
      0.385779555, 0.461217861, 0.709444444),
    prob_non_severe_death_treatment = c(
      0.0125702,	0.0125702,	0.0125702,	0.0125702,
      0.0125702,	0.0125702,	0.0125702,	0.013361147,
      0.015104687,	0.019164124,	0.027477519,	0.041762108,
      0.068531658,	0.105302319,	0.149305732,	0.20349534,	0.5804312),
    prob_non_severe_death_no_treatment = rep(0.6, length(prob_hosp)),
    prob_severe_death_treatment = rep(0.5, length(prob_hosp)),
    prob_severe_death_no_treatment = rep(0.95, length(prob_hosp)),
    p_dist = rep(1, length(prob_hosp))
  )
}

probs <- default_probs()

#' Run the SEEIR model
#'
#' @param R0 Basic reproduction number
#' @param tt_R0 Change time points for R0
#' @param dt Time step
#' @param init Data.frame of initial conditions
#' @param dur_E Mean duration of incubation period (days)
#' @param dur_I Mean duration of infectious period (days)
#' @param population Population vector (for each age group)
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
#' m1 <- run_simple_SEEIR_model(population = pop$n, dt = 1,
#' R0 = 2,
#' contact_matrix_set=contact_matrices[[1]])
#' }
run_simple_SEEIR_model <- function(R0 = 3,
                            tt_R0 = 0,
                            dt = 0.1,
                            init = NULL,
                            dur_E  = 4.58,
                            dur_I = 2.09,
                            population,
                            contact_matrix_set,
                            tt_contact_matrix = 0,
                            time_period = 365,
                            replicates = 10) {

  # Initialise initial conditions
  pars <- parameters_simple_SEEIR(R0=R0,
                                  tt_R0=tt_R0,
                                  dt=dt,
                                  init=init,
                                  dur_E=dur_E,
                                  dur_I=dur_I,
                                  population=population,
                                  contact_matrix_set=contact_matrix_set,
                                  tt_contact_matrix=tt_contact_matrix,
                                  time_period=time_period)

  # Running the Model
  mod <- simple_SEIR(user = pars, unused_user_action = "ignore")
  t <- seq(from = 1, to = time_period/dt)
  results <- mod$run(t, replicate = replicates)

  # Summarise inputs
  parameters = list(R0 = R0, tt_R0 = tt_R0,
                dt = dt,
                init = init,
                dur_E  = dur_E, dur_I = dur_I,
                population = population,
                contact_matrix_set = contact_matrix_set,
                tt_contact_matrix = tt_contact_matrix,
                time_period = time_period, replicates = replicates)

  out <- list(output = results, parameters = parameters, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)
}

# -----------------------------------------------------------------------------
#' Run the explicit SEEIR model
#'
#' @details All durations are in days.
#'
#' @param population Population vector (for each age group). Default = NULL,
#'   which will cause population to be sourced from \code{country}
#' @param country Character for country beign simulated. WIll be used to
#'   generate \code{population} and \code{contact_matrix_set} if
#'   unprovided. Either \code{country} or \code{population} and
#'   \code{contact_matrix_set} must be provided.
#' @param contact_matrix_set Contact matrices used in simulation. Default =
#'   NULL, which will generate this based on the \code{country}.
#' @param tt_contact_matrix Time change points for matrix change. Default = 0
#' @param R0 Basic Reproduction Number. Default = 3
#' @param tt_R0 Change time points for R0. Default = 0
#' @param beta_set Alternative parameterisation via beta rather than R0.
#'   Default = NULL, which causes beta to be estimated from R0
#' @param time_period Length of simulation. Default = 365
#' @param dt Time Step. Default = 0.1
#' @param day_return Logical, do we want to return outut after each day rather
#'   than each dt. Default = FALSE
#' @param replicates  Number of replicates. Default = 10
#' @param seeding_cases Initial number of cases seeding the epidemic
#' @param seed Random seed used for simulations. Deafult = runif(1, 0, 10000)
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
#' @param prob_non_severe_death_treatment Probability of death from non severe
#'   treated infection.
#'   Default = c(0.0125702, 0.0125702, 0.0125702, 0.0125702,
#'   0.0125702, 0.0125702, 0.0125702, 0.013361147,
#'   0.015104687, 0.019164124, 0.027477519, 0.041762108,
#'   0.068531658, 0.105302319, 0.149305732, 0.20349534)
#' @param prob_severe_death_treatment Probability of death from severe infection
#'   that is treated. Default = rep(0.5, 16)
#' @param prob_non_severe_death_no_treatment Probability of death in non severe
#'   hospital inections that aren't treated
#' @param prob_severe_death_no_treatment Probability of death from severe infection
#'   that is not treated. Default = rep(0.95, 16)
#' @param p_dist Preferentiality of age group receiving treatment relative to
#'   other age groups when demand exceeds healthcare capacity.
#' @param dur_E Mean duration of incubation period (days). Default = 4.6
#' @param dur_IMild Mean duration of mild infection (days). Default = 2.1
#' @param dur_ICase Mean duration from symptom onset to hospitil admission (days).
#'   Default = 4.5
#' @param dur_get_ox_survive Mean duration of oxygen given survive. Default = 5
#' @param dur_get_ox_die Mean duration of oxygen given death. Default = 5
#' @param dur_not_get_ox_survive Mean duration without oxygen given survive.
#'   Default = 5
#' @param dur_not_get_ox_die Mean duration without  oxygen given death.
#'  Default = 5
#' @param dur_get_mv_survive Mean duration of ventilation given survive.
#'   Default = 7.3
#' @param dur_get_mv_die Mean duration of ventilation given death. Default = 6
#' @param dur_not_get_mv_survive Mean duration without ventilation given
#'   survive. Default = 7.3
#' @param dur_not_get_mv_die Mean duration without ventilation given
#'   death. Default = 1
#' @param dur_rec Duration of recovery after coming off ventilation. Default = 2
#' @param hosp_bed_capacity General bed capacity. Can be single number of vector if capacity time-varies.
#' @param ICU_bed_capacity ICU bed capacity. Can be single number of vector if capacity time-varies.
#' @param tt_hosp_beds Times at which hospital bed capacity changes (Default = 0 = doesn't change)
#' @param tt_ICU_beds Times at which ICU bed capacity changes (Default = 0 = doesn't change)
#' @param seeding_cases Initial number of cases seeding the epidemic
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' \dontrun{
#' pop <- get_population("Afghanistan")
#' m1 <- run_explicit_SEEIR_model(R0 = 2,
#' population = pop$n, dt = 1,
#' contact_matrix_set=contact_matrices[[1]])
#' }
run_explicit_SEEIR_model <- function(

  # demography
  country = NULL,
  population = NULL,
  tt_contact_matrix = 0,
  contact_matrix_set = NULL,

  # transmission
  R0 = 3,
  tt_R0 = 0,
  beta_set = NULL,

  # initial state, duration, reps
  time_period = 365,
  dt = 0.1,
  day_return = FALSE,
  replicates = 10,
  init = NULL,
  seed = stats::runif(1, 0, 100000000),

  # parameters
  # probabilities
  prob_hosp = probs$prob_hosp,
  prob_severe = probs$prob_severe,
  prob_non_severe_death_treatment = probs$prob_non_severe_death_treatment,
  prob_non_severe_death_no_treatment = probs$prob_non_severe_death_no_treatment,
  prob_severe_death_treatment = probs$prob_severe_death_treatment,
  prob_severe_death_no_treatment = probs$prob_severe_death_no_treatment,
  p_dist = probs$p_dist,

  # durations
  dur_E  = 4.6,
  dur_IMild = 2.1,
  dur_ICase = 4.5,

  dur_get_ox_survive = 9.5,
  dur_get_ox_die = 7.6,
  dur_not_get_ox_survive = 9.5*0.5,
  dur_not_get_ox_die = 7.6*0.5,

  dur_get_mv_survive = 11.3,
  dur_get_mv_die = 10.1,
  dur_not_get_mv_survive = 11.3*0.5,
  dur_not_get_mv_die = 1,

  dur_rec = 3.4,

  # health system capacity
  hosp_bed_capacity = NULL,
  ICU_bed_capacity = NULL,
  tt_hosp_beds = 0,
  tt_ICU_beds = 0,

  seeding_cases = NULL

  ) {

  # Grab function arguments
  args <- as.list(environment())
  set.seed(seed)

  # create parameter list
  pars <- parameters_explicit_SEEIR(country=country,
                                    population=population,
                                    tt_contact_matrix=tt_contact_matrix,
                                    contact_matrix_set=contact_matrix_set,
                                    R0=R0,
                                    tt_R0=tt_R0,
                                    beta_set=beta_set,
                                    time_period=time_period,
                                    dt=dt,
                                    init=init,
                                    seeding_cases=seeding_cases,
                                    prob_hosp=prob_hosp,
                                    prob_severe=prob_severe,
                                    prob_non_severe_death_treatment=prob_non_severe_death_treatment,
                                    prob_non_severe_death_no_treatment=prob_non_severe_death_no_treatment,
                                    prob_severe_death_treatment=prob_severe_death_treatment,
                                    prob_severe_death_no_treatment=prob_severe_death_no_treatment,
                                    p_dist=p_dist,
                                    dur_E=dur_E,
                                    dur_IMild=dur_IMild,
                                    dur_ICase=dur_ICase,
                                    dur_get_ox_survive=dur_get_ox_survive,
                                    dur_get_ox_die=dur_get_ox_die,
                                    dur_not_get_ox_survive=dur_not_get_ox_survive,
                                    dur_not_get_ox_die=dur_not_get_ox_die,
                                    dur_get_mv_survive=dur_get_mv_survive,
                                    dur_get_mv_die=dur_get_mv_die,
                                    dur_not_get_mv_survive=dur_not_get_mv_survive,
                                    dur_not_get_mv_die=dur_not_get_mv_die,
                                    dur_rec=dur_rec,
                                    hosp_bed_capacity=hosp_bed_capacity,
                                    ICU_bed_capacity=ICU_bed_capacity,
                                    tt_hosp_beds=tt_hosp_beds,
                                    tt_ICU_beds=tt_ICU_beds)

  # Running the Model
  mod <- explicit_SEIR(user = pars, unused_user_action = "ignore")
  t <- seq(from = 1, to = time_period/dt)

  # if we ar doing day return then proceed in steps of day length
  # We also will do an extra day so we know the numebr of infections/deaths
  # that would happen in the last day
  if (day_return) {
    t <- round(seq(1/dt, length(t)+(1/dt), by=1/dt))
  }
  results <- mod$run(t, replicate = replicates)

  # Summarise inputs
  parameters <- args
  parameters$population <- pars$population
  parameters$hosp_bed_capacity <- pars$hosp_beds
  parameters$ICU_bed_capacity <- pars$ICU_beds
  parameters$beta_set <- pars$beta_set
  parameters$seeding_cases <- pars$E1_0
  parameters$contact_matrix_set <- pars$contact_matrix_set

  out <- list(output = results, parameters = parameters, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)
}

#' Run the deterministic explicit SEIR model
#'
#' @param population Population vector (for each age group). Default = NULL,
#'   which will cause population to be sourced from \code{country}
#' @param contact_matrix Contact matrix to use in the simulation.
#' @param tt_R0 Time change points for R0
#' @param R0_set The values of R0 to use for the simulation
#' @param time_period Length of simulation. Default = 365
#' @param hosp_bed_capacity General bed capacity. Can be single number of vector if capacity time-varies.
#' @param ICU_bed_capacity ICU bed capacity. Can be single number of vector if capacity time-varies.
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' \dontrun{
#' pop <- get_population("Afghanistan")
#' m <- get_mixing_matrix("Afghanistan")
#' run_deterministic_SEIR_model(pop$n, m, [0, 50], [3, 3/2], 365, 100000,
#' 1000000)
#' }
run_deterministic_SEIR_model <- function(
  population,
  contact_matrix,
  tt_R0,
  R0_set,
  time_period,
  hosp_bed_capacity,
  ICU_bed_capacity
  ) {

  default_params <- parameters_explicit_SEEIR(
    population = population,
    tt_contact_matrix = 0,
    contact_matrix_set = contact_matrix,
    R0 = R0_set,
    tt_R0 = tt_R0,
    dt = 1,
    init = NULL,
    seeding_cases = NULL,
    hosp_bed_capacity = hosp_bed_capacity,
    ICU_bed_capacity = ICU_bed_capacity,
    tt_hosp_beds = 0,
    tt_ICU_beds = 0
  )

  seed <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

  pars <- list(
    N_age = length(population),
    S_0 = population - seed,
    E1_0 = seed,
    E2_0 = rep(0, length(population)),
    IMild_0 = rep(0, length(population)),
    ICase1_0 = rep(0, length(population)),
    ICase2_0 = rep(0, length(population)),
    IOxGetLive1_0 = rep(0, length(population)),
    IOxGetLive2_0 = rep(0, length(population)),
    IOxGetDie1_0 = rep(0, length(population)),
    IOxGetDie2_0 = rep(0, length(population)),
    IOxNotGetLive1_0 = rep(0, length(population)),
    IOxNotGetLive2_0 = rep(0, length(population)),
    IOxNotGetDie1_0 = rep(0, length(population)),
    IOxNotGetDie2_0 = rep(0, length(population)),
    IMVGetLive1_0 = rep(0, length(population)),
    IMVGetLive2_0 = rep(0, length(population)),
    IMVGetDie1_0 = rep(0, length(population)),
    IMVGetDie2_0 = rep(0, length(population)),
    IMVNotGetLive1_0 = rep(0, length(population)),
    IMVNotGetLive2_0 = rep(0, length(population)),
    IMVNotGetDie1_0 = rep(0, length(population)),
    IMVNotGetDie2_0 = rep(0, length(population)),
    IRec1_0 = rep(0, length(population)),
    IRec2_0 = rep(0, length(population)),
    R_0 = rep(0, length(population)),
    D_0 = rep(0, length(population)),
    gamma_E = default_params$gamma_E,
    gamma_R = default_params$gamma_IMild,
    gamma_hosp = default_params$gamma_ICase,
    gamma_get_ox_survive = default_params$gamma_get_ox_survive,
    gamma_get_ox_die = default_params$gamma_get_ox_die,
    gamma_not_get_ox_survive = default_params$gamma_not_get_ox_survive,
    gamma_not_get_ox_die = default_params$gamma_not_get_ox_die,
    gamma_get_mv_survive = default_params$gamma_get_mv_survive,
    gamma_get_mv_die = default_params$gamma_get_mv_die,
    gamma_not_get_mv_survive = default_params$gamma_not_get_mv_survive,
    gamma_not_get_mv_die = default_params$gamma_not_get_mv_die,
    gamma_rec = default_params$gamma_rec,
    prob_hosp = default_params$prob_hosp,
    prob_severe = default_params$prob_severe,
    prob_non_severe_death_treatment = default_params$prob_non_severe_death_treatment,
    prob_non_severe_death_no_treatment = default_params$prob_non_severe_death_no_treatment,
    prob_severe_death_treatment = default_params$prob_severe_death_treatment,
    prob_severe_death_no_treatment = default_params$prob_severe_death_no_treatment,
    p_dist = default_params$p_dist,
    hosp_bed_capacity = hosp_bed_capacity,
    ICU_bed_capacity = ICU_bed_capacity,
    tt_matrix = default_params$tt_matrix,
    mix_mat_set = default_params$mix_mat_set,
    tt_beta = default_params$tt_beta,
    beta_set = default_params$beta_set
  )

  mod <- explicit_SEIR_deterministic(user = pars)
  t <- seq(from = 0, to = time_period - 1)
  results <- mod$run(t)
  out <- list(output = results, parameters = pars, model = mod)
  structure(out, class = "squire_simulation")
}
