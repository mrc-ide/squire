# -----------------------------------------------------------------------------
#' Parmeters for the simple SEEIR model
#'
#' @param R0 Basic reproduction number
#' @param tt_R0 Change time points for R0
#' @param dt Time step
#' @param init Data.frame of initial conditions
#' @param dur_E Mean duration of incubation period (days)
#' @param dur_I Mean duration of infectious period (days) in simple model
#' @param population Population vector (for each age group)
#' @param contact_matrix_set Contact matrices used in simulation
#' @param tt_contact_matrix Time change points for matrix change
#' @param time_period Length of simulation
#' @param day_return Logical, do we want to return outut after each day rather
#'   than each dt. Default = FALSE
#'
#' @return Paramater List
#' @export
#'
parameters_simple_SEEIR <- function(R0 = 3,
                                   tt_R0 = 0,
                                   dt = 0.1,
                                   init = NULL,
                                   dur_E  = 4.58,
                                   dur_I = 2.09,
                                   day_return = FALSE,
                                   population,
                                   contact_matrix_set,
                                   tt_contact_matrix = 0,
                                   time_period = 365) {

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

  # Convert and Generate Parameters As Required
  gamma_E <- 2 * 1 / dur_E
  gamma_I <- 1 / dur_I
  beta_set <- beta_est_simple(dur_I, contact_matrix_set[[1]], R0)

  # Convert contact matrices to input matrices
  matrices_set <- matrix_set(contact_matrix_set, population)

  # Collate Parameters Into List
  pars <- list(S0 = init$S,
               E0 = init$E,
               E02 = init$E2,
               I0 = init$I,
               R0 = init$R,
               gamma_E = gamma_E,
               gamma_I = gamma_I,
               tt_beta = tt_R0,
               beta_set = beta_set,
               N_age = length(population),
               tt_matrix = tt_contact_matrix,
               mix_mat_set = matrices_set,
               contact_matrix_set = contact_matrix_set,
               population = population,
               day_return = day_return,
               dt = dt)

  class(pars) <- c("simple_SEEIR_parameters", "squire_parameters")
  return(pars)

}




# Get ICU bed capacity
#' @noRd
get_ICU_bed_capacity <- function(country) {

    beds <- get_healthcare_capacity(country)
    ICU_beds <- beds$ICU_beds
    population <- get_population(country)$n
    ICU_bed_capacity <- round(ICU_beds * sum(population)/1000)
    ICU_bed_capacity

}

# Get hospital bed capacity
#' @noRd
get_hosp_bed_capacity <- function(country = NULL) {

    beds <- get_healthcare_capacity(country)
    population <- get_population(country)$n
    hosp_beds <- beds$hosp_beds
    hosp_bed_capacity <- round(hosp_beds * sum(population)/1000)

}
# -----------------------------------------------------------------------------
#' Parmaters for explicit SEEIR model
#'
#' @details All durations are in days.
#'
#' @section Parameter Updates:
#'
#' Parameters detailing the age-dependent probability of disease
#' severity and durations of hospital durations have been updated in v0.5.0
#' of \code{squire} to reflect the changing understanding of COVID-19 transmission.
#' Parameter arguments are by default equal to \code{NULL}, which
#' causes the new updated parameters specified in \code{\link{default_probs}}
#' and \code{\link{default_durations}} to be used. If any provided parameters
#' are not \code{NULL}, these will be used. In order to ease previous fits and
#' code, function argument \code{walker_params} will use the parameters described
#' in \href{https://science.sciencemag.org/content/369/6502/413}{Walker et al. Science. 2020}
#' which can be viewed within the function \code{\link{parse_country_severity}}
#'
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
#' @param init Data.frame of initial conditions. Default = NULL
#' @param seeding_cases Initial number of cases seeding the epidemic
#' @param prob_hosp Probability of hospitalisation by age.
#'   Default, NULL, will use
#'   \code{c(0.000840764, 0.001182411, 0.001662887, 0.002338607,
#'   0.003288907, 0.004625365, 0.006504897, 0.009148183, 0.012865577,
#'   0.018093546, 0.025445917, 0.035785947, 0.050327683, 0.0707785,
#'   0.099539573, 0.1399878, 0.233470395)}
#' @param prob_severe Probability of developing severe symptoms by age.
#'   Default, NULL, will use
#'   \code{c(0.000840764, 0.001182411, 0.001662887, 0.002338607,
#'   0.003288907, 0.004625365, 0.006504897, 0.009148183, 0.012865577,
#'   0.018093546, 0.025445917, 0.035785947, 0.050327683, 0.0707785,
#'   0.099539573, 0.1399878, 0.233470395)}
#' @param prob_non_severe_death_treatment Probability of death from non severe
#'   treated infection. Default, NULL, will use
#'   \code{c(0.181354223, 0.181354223, 0.181354223, 0.137454906,
#'   0.121938236, 0.122775613, 0.136057441, 0.160922182, 0.196987378,
#'   0.242011054, 0.289368845, 0.326537862, 0.337229819, 0.309082553,
#'   0.243794865, 0.160480254, 0.057084366)}
#' @param prob_severe_death_treatment Probability of death from severe infection
#'   that is treated. Default, NULL, will use
#'   \code{c(0.226668959, 0.252420241, 0.281097009, 0.413005389,
#'   0.518451493, 0.573413613, 0.576222065, 0.54253573, 0.493557696,
#'   0.447376527, 0.416666608, 0.411186639, 0.443382594, 0.538718871,
#'   0.570434076, 0.643352843, 0.992620047)}
#' @param prob_non_severe_death_no_treatment Probability of death in non severe
#'   hospital inections that aren't treated. Default, NULL, will use
#'   \code{rep(0.5, 17)}
#' @param prob_severe_death_no_treatment Probability of death from severe infection
#'   that is not treated. Default, NULL, will use
#'   \code{rep(0.95, 17)}
#' @param p_dist Preferentiality of age group receiving treatment relative to
#'   other age groups when demand exceeds healthcare capacity.
#' @param dur_E Mean duration of incubation period (days). Default = 4.6
#' @param dur_IMild Mean duration of mild infection (days). Default = 2.1
#' @param dur_ICase Mean duration from symptom onset to hospitil admission (days).
#'   Default = 4.5
#' @param dur_get_ox_survive Mean duration of oxygen given survive. Default = 9
#' @param tt_dur_get_ox_survive Times at which dur_get_ox_survive changes
#'   (Default = 0 = doesn't change)
#' @param dur_get_ox_die Mean duration of oxygen given death. Default = 9
#' @param tt_dur_get_ox_die Times at which dur_get_ox_die changes
#'   (Default = 0 = doesn't change)
#' @param dur_not_get_ox_survive Mean duration without oxygen given survive.
#'   Default = 4.5
#' @param dur_not_get_ox_die Mean duration without  oxygen given death.
#'  Default = 4.5
#' @param dur_get_mv_survive Mean duration of ventilation given survive.
#'   Default = 15.3
#' @param tt_dur_get_mv_survive Times at which dur_get_mv_survive changes
#'   (Default = 0 = doesn't change)
#' @param dur_get_mv_die Mean duration of ventilation given death. Default = 11.3
#' @param tt_dur_get_mv_die Times at which dur_get_mv_die changes
#'   (Default = 0 = doesn't change)
#' @param dur_not_get_mv_survive Mean duration without ventilation given
#'   survive. Default = 7.65
#' @param dur_not_get_mv_die Mean duration without ventilation given
#'   death. Default = 1
#' @param dur_rec Duration of recovery after coming off ventilation. Default = 3
#' @param hosp_bed_capacity General bed capacity. Can be single number or vector if capacity time-varies.
#' @param ICU_bed_capacity ICU bed capacity. Can be single number or vector if capacity time-varies.
#' @param tt_hosp_beds Times at which hospital bed capacity changes (Default = 0 = doesn't change)
#' @param tt_ICU_beds Times at which ICU bed capacity changes (Default = 0 = doesn't change)
#'
#' @param walker_params Boolean for using parameters in Walker et al. Default = FALSE,
#'   which uses parameter update as of November 2020. For full information see
#'   parameters vignette
#'
#' @return Paramater List
#' @export
#'
parameters_explicit_SEEIR <- function(

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
  init = NULL,
  seeding_cases = NULL,

  # parameters/probabilities
  prob_hosp = NULL,
  prob_severe = NULL,
  prob_non_severe_death_treatment = NULL,
  prob_non_severe_death_no_treatment = NULL,
  prob_severe_death_treatment = NULL,
  prob_severe_death_no_treatment = NULL,
  p_dist = probs$p_dist,
  walker_params = FALSE,

  # durations
  dur_E  = 4.6,
  dur_IMild = 2.1,
  dur_ICase = 4.5,

  dur_get_ox_survive = NULL,
  tt_dur_get_ox_survive = NULL,

  dur_get_ox_die = NULL,
  tt_dur_get_ox_die = NULL,

  dur_not_get_ox_survive = NULL,
  dur_not_get_ox_die = NULL,

  dur_get_mv_survive = NULL,
  tt_dur_get_mv_survive = NULL,

  dur_get_mv_die = NULL,
  tt_dur_get_mv_die = NULL,

  dur_not_get_mv_survive = NULL,
  dur_not_get_mv_die = NULL,

  dur_rec = NULL,

  # health system capacity
  hosp_bed_capacity = NULL,
  ICU_bed_capacity = NULL,
  tt_hosp_beds = 0,
  tt_ICU_beds = 0

) {

  # Handle country population args
  cpm <- parse_country_population_mixing_matrix(country = country,
                                                population = population,
                                                contact_matrix_set = contact_matrix_set)
  country <- cpm$country
  population <- cpm$population
  contact_matrix_set <- cpm$contact_matrix_set

  # Handle severity parameters and possible 80+ demographic adjustment
  severity_params <- parse_country_severity(country = country,
                                            prob_hosp = prob_hosp,
                                            prob_severe = prob_severe,
                                            prob_non_severe_death_treatment = prob_non_severe_death_treatment,
                                            prob_severe_death_treatment = prob_severe_death_treatment,
                                            prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
                                            prob_severe_death_no_treatment = prob_severe_death_no_treatment,
                                            walker_params = walker_params)

  prob_hosp <- severity_params$prob_hosp
  prob_severe <- severity_params$prob_severe
  prob_non_severe_death_treatment <- severity_params$prob_non_severe_death_treatment
  prob_severe_death_treatment <- severity_params$prob_severe_death_treatment
  prob_non_severe_death_no_treatment <- severity_params$prob_non_severe_death_no_treatment
  prob_severe_death_no_treatment <- severity_params$prob_severe_death_no_treatment

  # Handle duration of hospitalisation parameters
  hosp_duration_params <- parse_hospital_duration(dur_get_ox_survive = dur_get_ox_survive,
                                                  tt_dur_get_ox_survive = tt_dur_get_ox_survive,
                                                  dur_get_ox_die = dur_get_ox_die,
                                                  tt_dur_get_ox_die = tt_dur_get_ox_die,
                                                  dur_not_get_ox_survive = dur_not_get_ox_survive,
                                                  dur_not_get_ox_die = dur_not_get_ox_die,
                                                  dur_get_mv_survive = dur_get_mv_survive,
                                                  tt_dur_get_mv_survive = tt_dur_get_mv_survive,
                                                  dur_get_mv_die = dur_get_mv_die,
                                                  tt_dur_get_mv_die = tt_dur_get_mv_die,
                                                  dur_not_get_mv_survive = dur_not_get_mv_survive,
                                                  dur_not_get_mv_die = dur_not_get_mv_die,
                                                  dur_rec = dur_rec,
                                                  walker_params = walker_params)

  dur_get_ox_survive <- hosp_duration_params$dur_get_ox_survive
  tt_dur_get_ox_survive <- hosp_duration_params$tt_dur_get_ox_survive
  dur_get_ox_die <- hosp_duration_params$dur_get_ox_die
  tt_dur_get_ox_die <- hosp_duration_params$tt_dur_get_ox_die
  dur_not_get_ox_survive <- hosp_duration_params$dur_not_get_ox_survive
  dur_not_get_ox_die <- hosp_duration_params$dur_not_get_ox_die
  dur_get_mv_survive <- hosp_duration_params$dur_get_mv_survive
  tt_dur_get_mv_survive <- hosp_duration_params$tt_dur_get_mv_survive
  dur_get_mv_die <- hosp_duration_params$dur_get_mv_die
  tt_dur_get_mv_die <- hosp_duration_params$tt_dur_get_mv_die
  dur_not_get_mv_survive <- hosp_duration_params$dur_not_get_mv_survive
  dur_not_get_mv_die <- hosp_duration_params$dur_not_get_mv_die
  dur_rec <- hosp_duration_params$dur_rec

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
      hosp_bed_capacity <- rep(round(hosp_beds * sum(population)/1000), length(tt_hosp_beds))
    } else {
      hosp_bed_capacity <- round(5 * sum(population)/1000)
    }
  }
  if (is.null(ICU_bed_capacity)) {
    if (!is.null(country)) {
      beds <- get_healthcare_capacity(country)
      ICU_beds <- beds$ICU_beds
      ICU_bed_capacity <- rep(round(ICU_beds * sum(population)/1000), length(tt_ICU_beds))
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
  mc <- matrix_check(population[-1], contact_matrix_set)

  # Input checks
  # ----------------------------------------------------------------------------
  stopifnot(length(R0) == length(tt_R0))
  stopifnot(length(contact_matrix_set) == length(tt_contact_matrix))
  stopifnot(length(hosp_bed_capacity) == length(tt_hosp_beds))
  stopifnot(length(ICU_bed_capacity) == length(tt_ICU_beds))
  stopifnot(length(dur_get_mv_survive) == length(tt_dur_get_mv_survive))
  stopifnot(length(dur_get_ox_survive) == length(tt_dur_get_ox_survive))
  stopifnot(length(dur_get_mv_die) == length(tt_dur_get_mv_die))
  stopifnot(length(dur_get_ox_die) == length(tt_dur_get_ox_die))

  tc <- lapply(list(tt_R0/dt, tt_contact_matrix/dt,
                    tt_hosp_beds/dt, tt_ICU_beds/dt,
                    tt_dur_get_mv_survive/dt,
                    tt_dur_get_ox_survive/dt,
                    tt_dur_get_mv_die/dt,
                    tt_dur_get_ox_die/dt
                    ),
               check_time_change, time_period/dt)

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
    baseline_matrix <- process_contact_matrix_scaled_age(contact_matrix_set[[1]], population)
    beta_set <- beta_est_explicit(dur_IMild = dur_IMild,
                                  dur_ICase = dur_ICase,
                                  prob_hosp = prob_hosp,
                                  mixing_matrix = baseline_matrix,
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
               mix_mat_set = matrices_set,
               hosp_beds = hosp_bed_capacity,
               ICU_beds = ICU_bed_capacity,
               beta_set = beta_set,
               tt_hosp_beds = round(tt_hosp_beds/dt),
               tt_ICU_beds = round(tt_ICU_beds/dt),
               tt_matrix = round(tt_contact_matrix/dt),
               tt_beta = round(tt_R0/dt),
               tt_dur_get_mv_survive = round(tt_dur_get_mv_survive/dt),
               tt_dur_get_ox_survive = round(tt_dur_get_ox_survive/dt),
               tt_dur_get_mv_die = round(tt_dur_get_mv_die/dt),
               tt_dur_get_ox_die = round(tt_dur_get_ox_die/dt),
               dt = dt,
               population = population,
               contact_matrix_set = contact_matrix_set,
               dur_get_ox_survive = dur_get_ox_survive,
               dur_get_ox_die = dur_get_ox_die,
               dur_not_get_ox_survive = dur_not_get_ox_survive,
               dur_not_get_ox_die = dur_not_get_ox_die,
               dur_get_mv_survive = dur_get_mv_survive,
               dur_get_mv_die = dur_get_mv_die,
               dur_not_get_mv_survive = dur_not_get_mv_survive,
               dur_not_get_mv_die = dur_not_get_mv_die,
               dur_rec = dur_rec)

  class(pars) <- c("explicit_SEEIR_parameters", "squire_parameters")

  return(pars)

}
