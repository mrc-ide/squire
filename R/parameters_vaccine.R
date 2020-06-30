
#' Vaccine parameters
#'
#' @details All durations are in days.
#'
#' @inheritParams run_vaccine
parameters_vaccine <- function(

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
  dur_E,
  dur_IMild,
  dur_ICase,

  dur_get_ox_survive,
  dur_get_ox_die,
  dur_not_get_ox_survive,
  dur_not_get_ox_die,

  dur_get_mv_survive,
  dur_get_mv_die,
  dur_not_get_mv_survive,
  dur_not_get_mv_die,

  dur_rec,
  dur_R,
  vaccination_target,
  dur_V,
  vaccine_efficacy_infection,
  vaccine_efficacy_disease,
  max_vaccine,
  tt_vaccine,
  dur_vaccine_delay,

  # health system capacity
  hosp_bed_capacity,
  ICU_bed_capacity,
  tt_hosp_beds,
  tt_ICU_beds,

  framework

) {

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
    mod_init <- init_check_vaccine(init, population, seeding_cases)
  } else {
    mod_init <- init_check_vaccine(init, population)
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
  stopifnot(length(max_vaccine) == length(tt_vaccine))
  tc <- lapply(list(tt_R0/dt, tt_contact_matrix/dt), check_time_change, time_period/dt)
  tc2 <- lapply(list(tt_hosp_beds/dt, tt_ICU_beds/dt), check_time_change, time_period/dt)
  stopifnot(all(vaccination_target %in% 0:1))

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
  assert_pos(dur_R)
  assert_pos(dur_V)
  assert_pos(time_period)
  assert_pos(hosp_bed_capacity)
  assert_pos(ICU_bed_capacity)
  assert_pos(max_vaccine)
  assert_pos(dur_vaccine_delay)

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
  gamma_R <- 2 * 1/dur_R
  gamma_V <- 2 * 1/dur_V
  gamma_SVac <- 2 * 1 / dur_vaccine_delay
  gamma_RVac <- 2 * 1 / dur_vaccine_delay

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

  # Format vaccine-specific parameters
  vaccine_efficacy_infection = 1 - vaccine_efficacy_infection
  prob_hosp_vaccine = (1 - vaccine_efficacy_disease) * prob_hosp

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
               R1_0 = mod_init$R1,
               R2_0 = mod_init$R2,
               D_0 = mod_init$D,
               V1_0 = mod_init$V1,
               V2_0 = mod_init$V2,
               EVac1_0 = mod_init$EVac1,
               EVac2_0 = mod_init$EVac2,
               SVac1_0 = mod_init$SVac1,
               SVac2_0 = mod_init$SVac2,
               RVac1_0 = mod_init$RVac1,
               RVac2_0 = mod_init$RVac2,
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
               gamma_R = gamma_R,
               gamma_V = gamma_V,
               prob_hosp = prob_hosp,
               prob_severe = prob_severe,
               prob_non_severe_death_treatment = prob_non_severe_death_treatment,
               prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
               prob_severe_death_treatment = prob_severe_death_treatment,
               prob_severe_death_no_treatment = prob_severe_death_no_treatment,
               p_dist = p_dist,
               hosp_beds = hosp_bed_capacity,
               ICU_beds = ICU_bed_capacity,
               tt_hosp_beds = tt_hosp_beds,
               tt_ICU_beds = tt_ICU_beds,
               tt_matrix = tt_contact_matrix,
               mix_mat_set = matrices_set,
               tt_beta = tt_R0,
               beta_set = beta_set,
               dt = dt,
               population = population,
               contact_matrix_set = contact_matrix_set,
               vaccination_target = vaccination_target,
               max_vaccine = max_vaccine,
               vaccine_efficacy_infection = vaccine_efficacy_infection,
               prob_hosp_vaccine = prob_hosp_vaccine,
               tt_vaccine = tt_vaccine,
               gamma_SVac = gamma_SVac,
               gamma_RVac = gamma_RVac)

  if(framework == "stochastic"){
    pars$tt_vaccine <- round(pars$tt_vaccine / dt)
    pars$tt_beta <- round(pars$tt_beta / dt)
    pars$tt_matrix <- round(pars$tt_matrix / dt)
    pars$tt_ICU_beds <- round(pars$tt_ICU_beds / dt)
    pars$tt_hosp_beds <- round(pars$tt_hosp_beds / dt)
  }

  class(pars) <- c("explicit_SEEIR_parameters", "squire_parameters")

  return(pars)
}
