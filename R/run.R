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
  init <- init_check(init, population)
  # Standardise contact matrix set
  if(is.matrix(contact_matrix_set)){
    contact_matrix_set <- list(contact_matrix_set)
  }

  # populate contact matrix set if not provided
  if (length(contact_matrix_set) == 1) {
    baseline <- contact_matrix_set[[1]]
    contact_matrix_set <- vector("list", length(tt_contact_matrix))
    for(i in seq_along(tt_contact_matrix)) {
      contact_matrix_set[[i]] <- baseline
    }
  }

  # Input checks
  mc <- matrix_check(population, contact_matrix_set)
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
  beta_set <- beta_est_simple(dur_I, contact_matrix_set[[1]], R0)

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
  mod <- simple_SEIR(user = pars)
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
#' @param dt Time Step. Default = 0.5
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
  replicates = 10,
  init = NULL,
  seed = stats::runif(1, 0, 100000000),

  # parameters
  # probabilities
  prob_hosp = c(0.000744192, 0.000634166,0.001171109, 0.002394593, 0.005346437 ,
                0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
                0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
                0.176634654 ,0.180000000),
  prob_severe = c(0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
                  0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
                  0.103612417, 0.149427991, 0.223777304,	0.306985918,
                  0.385779555, 0.461217861, 0.709444444),
  prob_non_severe_death_treatment = c(0.0125702,	0.0125702,	0.0125702,	0.0125702,
                                      0.0125702,	0.0125702,	0.0125702,	0.013361147,
                                      0.015104687,	0.019164124,	0.027477519,	0.041762108,
                                      0.068531658,	0.105302319,	0.149305732,	0.20349534,	0.5804312),
  prob_non_severe_death_no_treatment = rep(0.6, length(prob_hosp)),
  prob_severe_death_treatment = rep(0.5, length(prob_hosp)),
  prob_severe_death_no_treatment = rep(0.95, length(prob_hosp)),
  p_dist = rep(1, length(prob_hosp)),

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


  # Handle country population args
  cpm <- parse_country_population_mixing_matrix(country = country,
                                                population = population,
                                                contact_matrix_set = contact_matrix_set)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

  # Standardise contact matrix set
  if(is.matrix(contact_matrix_set)){
    contact_matrix_set <- list(contact_matrix_set)
  }

  # populate contact matrix set if not provided
  if (length(contact_matrix_set) == 1) {
      baseline <- contact_matrix_set[[1]]
      contact_matrix_set <- vector("list", length(tt_contact_matrix))
      for(i in seq_along(tt_contact_matrix)) {
        contact_matrix_set[[i]] <- baseline
      }
  }


  # populate hospital and ICU bed capacity if not provided
  if (is.null(hosp_bed_capacity)) {
    if (!is.null(country)) {
      beds <- get_healthcare_capacity(country)
      hosp_beds <- beds$hosp_beds
      hosp_bed_capacity <- round(rep(round(hosp_beds * sum(population)/1000), length(tt_hosp_beds)))
    } else {
      hosp_bed_capacity <- round(5 * sum(population)/1000)
    }
  }
  if (is.null(ICU_bed_capacity)) {
    if (!is.null(country)) {
      beds <- get_healthcare_capacity(country)
      ICU_beds <- beds$ICU_beds
      ICU_bed_capacity <- round(rep(round(ICU_beds * sum(population)/1000), length(tt_ICU_beds)))
    } else {
      ICU_bed_capacity <- round(3 * hosp_bed_capacity/100)
    }
  }

  # Initial state and matrix formatting
  # ----------------------------------------------------------------------------

  # Initialise initial conditions
  if (!is.null(seeding_cases)) {
    assert_int(seeding_cases)
    mod_init <- init_check_explicit(init, population, seeding_cases)
  } else {
    mod_init <- init_check_explicit(init, population)
  }

  # Convert contact matrices to input matrices
  matrices_set <- matrix_set_explicit(contact_matrix_set, population)

  # Input checks
  # ----------------------------------------------------------------------------
  mc <- matrix_check(population[-1], contact_matrix_set)
  stopifnot(length(R0) == length(tt_R0))
  stopifnot(length(contact_matrix_set) == length(tt_contact_matrix))
  stopifnot(length(hosp_bed_capacity) == length(tt_hosp_beds))
  stopifnot(length(ICU_bed_capacity) == length(tt_ICU_beds))
  tc <- lapply(list(tt_R0/dt, tt_contact_matrix/dt), check_time_change, time_period/dt)
  tc2 <- lapply(list(tt_hosp_beds/dt, tt_ICU_beds/dt), check_time_change, time_period/dt)

  assert_pos(dt)
  assert_pos(dur_E)
  assert_pos(dur_IMild)
  assert_pos(dur_ICase)
  assert_pos(dur_get_ox_survive)
  assert_pos(dur_get_ox_die)
  assert_pos(dur_not_get_ox_survive)
  assert_pos(dur_not_get_ox_die)
  assert_pos(dur_get_mv_survive)
  assert_pos(dur_get_mv_die)
  assert_pos(dur_not_get_mv_survive)
  assert_pos(dur_not_get_mv_die)
  assert_pos(time_period)
  assert_pos(replicates)
  assert_pos(hosp_bed_capacity)
  assert_pos(ICU_bed_capacity)

  assert_length(prob_hosp, length(population))
  assert_length(prob_severe, length(population))
  assert_length(prob_non_severe_death_treatment, length(population))
  assert_length(prob_non_severe_death_no_treatment, length(population))
  assert_length(prob_severe_death_treatment, length(population))
  assert_length(prob_severe_death_no_treatment, length(population))
  assert_length(p_dist, length(population))

  assert_numeric(prob_hosp, length(population))
  assert_numeric(prob_severe, length(population))
  assert_numeric(prob_non_severe_death_treatment, length(population))
  assert_numeric(prob_non_severe_death_no_treatment, length(population))
  assert_numeric(prob_severe_death_treatment, length(population))
  assert_numeric(prob_severe_death_no_treatment, length(population))
  assert_numeric(p_dist, length(population))

  assert_leq(prob_hosp, 1)
  assert_leq(prob_severe, 1)
  assert_leq(prob_non_severe_death_treatment, 1)
  assert_leq(prob_non_severe_death_no_treatment, 1)
  assert_leq(prob_severe_death_treatment, 1)
  assert_leq(prob_severe_death_no_treatment, 1)
  assert_leq(p_dist, 1)

  assert_greq(prob_hosp, 0)
  assert_greq(prob_severe, 0)
  assert_greq(prob_non_severe_death_treatment, 0)
  assert_greq(prob_non_severe_death_no_treatment, 0)
  assert_greq(prob_severe_death_treatment, 0)
  assert_greq(prob_severe_death_no_treatment, 0)
  assert_greq(p_dist, 0)


  # Convert and Generate Parameters As Required
  # ----------------------------------------------------------------------------

  # durations
  gamma_E = 2 * 1/dur_E
  gamma_IMild = 1/dur_IMild
  gamma_ICase = 2 * 1/dur_ICase
  gamma_get_ox_survive = 2 * 1/dur_get_ox_survive
  gamma_get_ox_die = 2 * 1/dur_get_ox_die
  gamma_not_get_ox_survive = 2 * 1/dur_not_get_ox_survive
  gamma_not_get_ox_die = 2 * 1/dur_not_get_ox_die
  gamma_get_mv_survive = 2 * 1/dur_get_mv_survive
  gamma_get_mv_die = 2 * 1/dur_get_mv_die
  gamma_not_get_mv_survive = 2 * 1/dur_not_get_mv_survive
  gamma_not_get_mv_die = 2 * 1/dur_not_get_mv_die
  gamma_rec = 2 * 1/dur_rec

  if (is.null(beta_set)) {
  beta_set <- beta_est_explicit(dur_IMild = dur_IMild,
                                dur_ICase = dur_ICase,
                                prob_hosp = prob_hosp,
                                mixing_matrix = process_contact_matrix_scaled_age(contact_matrix_set[[1]], population),
                                R0 = R0)
  }

  # normalise to sum to 1
  p_dist <- p_dist/mean(p_dist)

  # Collate Parameters Into List
  pars <- list(N_age = length(population),
               S_0 = mod_init$S,
               E1_0 = mod_init$E1,
               E2_0 = mod_init$E2,
               IMild_0 = mod_init$IMild,
               ICase1_0 = mod_init$ICase1,
               ICase2_0 = mod_init$ICase2,
               IOxGetLive1_0 = mod_init$IOxGetLive1,
               IOxGetLive2_0 = mod_init$IOxGetLive2,
               IOxGetDie1_0 = mod_init$IOxGetDie1,
               IOxGetDie2_0 = mod_init$IOxGetDie2,
               IOxNotGetLive1_0 = mod_init$IOxNotGetLive1,
               IOxNotGetLive2_0 = mod_init$IOxNotGetLive2,
               IOxNotGetDie1_0 = mod_init$IOxNotGetDie1,
               IOxNotGetDie2_0 = mod_init$IOxNotGetDie2,
               IMVGetLive1_0 = mod_init$IMVGetLive1,
               IMVGetLive2_0 = mod_init$IMVGetLive2,
               IMVGetDie1_0 = mod_init$IMVGetDie1,
               IMVGetDie2_0 = mod_init$IMVGetDie2,
               IMVNotGetLive1_0 = mod_init$IMVNotGetLive1,
               IMVNotGetLive2_0 = mod_init$IMVNotGetLive2,
               IMVNotGetDie1_0 = mod_init$IMVNotGetDie1,
               IMVNotGetDie2_0 = mod_init$IMVNotGetDie2,
               IRec1_0 = mod_init$IRec1,
               IRec2_0 = mod_init$IRec2,
               R_0 = mod_init$R,
               D_0 = mod_init$D,
               gamma_E = gamma_E,
               gamma_IMild = gamma_IMild,
               gamma_ICase = gamma_ICase,
               gamma_get_ox_survive = gamma_get_ox_survive,
               gamma_get_ox_die = gamma_get_ox_die,
               gamma_not_get_ox_survive = gamma_not_get_ox_survive,
               gamma_not_get_ox_die = gamma_not_get_ox_die,
               gamma_get_mv_survive = gamma_get_mv_survive,
               gamma_get_mv_die = gamma_get_mv_die,
               gamma_not_get_mv_survive = gamma_not_get_mv_survive,
               gamma_not_get_mv_die = gamma_not_get_mv_die,
               gamma_rec = gamma_rec,
               prob_hosp = prob_hosp,
               prob_severe = prob_severe,
               prob_non_severe_death_treatment = prob_non_severe_death_treatment,
               prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
               prob_severe_death_treatment = prob_severe_death_treatment,
               prob_severe_death_no_treatment = prob_severe_death_no_treatment,
               p_dist = p_dist,
               hosp_beds = hosp_bed_capacity,
               ICU_beds = ICU_bed_capacity,
               tt_hosp_beds = round(tt_hosp_beds/dt),
               tt_ICU_beds = round(tt_ICU_beds/dt),
               tt_matrix = round(tt_contact_matrix/dt),
               mix_mat_set = matrices_set,
               tt_beta = round(tt_R0/dt),
               beta_set = beta_set,
               dt = dt)

  # Running the Model
  mod <- explicit_SEIR(user = pars, unused_user_action = "ignore")
  t <- seq(from = 1, to = time_period/dt)
  results <- mod$run(t, replicate = replicates)

  # Summarise inputs
  parameters <- args
  parameters$population <- population
  parameters$hosp_bed_capacity <- hosp_bed_capacity
  parameters$ICU_bed_capacity <- ICU_bed_capacity
  parameters$beta_set <- beta_set
  parameters$seeding_cases <- mod_init$E1
  parameters$contact_matrix_set <- contact_matrix_set

  out <- list(output = results, parameters = parameters, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)
}
