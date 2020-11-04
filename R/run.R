#' Return the default probabilities for modelling
#' @return list of default probabilities
default_probs <- function() {
  prob_hosp <- c(
    0.000840764, 0.001182411, 0.001662887, 0.002338607, 0.003288907,
    0.004625365, 0.006504897, 0.009148183, 0.012865577, 0.018093546,
    0.025445917, 0.035785947, 0.050327683, 0.0707785, 0.099539573,
    0.1399878, 0.233470395)
  list(
    prob_hosp = prob_hosp,
    prob_severe = c(
      0.181354223, 0.181354223, 0.181354223, 0.137454906, 0.121938236,
      0.122775613, 0.136057441, 0.160922182, 0.196987378, 0.242011054,
      0.289368845, 0.326537862, 0.337229819, 0.309082553, 0.243794865,
      0.160480254, 0.057084366),
    prob_non_severe_death_treatment = c(
      0.012553468, 0.013979635, 0.015567823, 0.016454101, 0.017999605,
      0.020063626, 0.022686491, 0.026012496, 0.030268714, 0.035709645,
      0.042416637, 0.049842448, 0.056400332, 0.060249095, 0.122602446,
      0.184472303, 0.340529746),
    prob_non_severe_death_no_treatment = rep(0.5, length(prob_hosp)),
    prob_severe_death_treatment = c(
      0.226668959, 0.252420241, 0.281097009, 0.413005389, 0.518451493,
      0.573413613, 0.576222065, 0.54253573, 0.493557696, 0.447376527,
      0.416666608, 0.411186639, 0.443382594, 0.538718871, 0.570434076,
      0.643352843, 0.992620047),
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
#' @param day_return Logical, do we want to return outut after each day rather
#'   than each dt. Default = FALSE
#' @param replicates Number of replicates
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' \dontrun{
#' pop <- get_population("Afghanistan", simple_SEIR = TRUE)
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
                            day_return = FALSE,
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
                                  day_return = day_return,
                                  time_period=time_period)

  # Running the Model
  mod <- simple_SEIR(user = pars, unused_user_action = "ignore")
  t <- seq(from = 1, to = time_period/dt)

  if (day_return) {
    t <- round(seq(1/dt, length(t)+(1/dt), by=1/dt))
  }

  results <- mod$run(t, replicate = replicates)

  # Summarise inputs
  parameters = list(R0 = R0, tt_R0 = tt_R0,
                dt = dt,
                init = init,
                dur_E  = dur_E, dur_I = dur_I,
                population = population,
                contact_matrix_set = contact_matrix_set,
                tt_contact_matrix = tt_contact_matrix,
                day_return = day_return,
                time_period = time_period, replicates = replicates)

  out <- list(output = results, parameters = parameters, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)
}

# -----------------------------------------------------------------------------
#' Run the explicit SEEIR model
#'
#' @details All durations are in days.
#' @inheritParams parameters_explicit_SEEIR
#' @param day_return Logical, do we want to return outut after each day rather
#'   than each dt. Default = FALSE
#' @param replicates Number of replicates
#' @param seed Random Number Seed.
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
  prob_hosp = NULL,
  prob_severe = NULL,
  prob_non_severe_death_treatment = NULL,
  prob_non_severe_death_no_treatment = NULL,
  prob_severe_death_treatment = NULL,
  prob_severe_death_no_treatment = NULL,
  p_dist = probs$p_dist,
  walker = FALSE,

  # durations
  dur_E  = 4.6,
  dur_IMild = 2.1,
  dur_ICase = 4.5,

  tt_dur_get_ox_survive = 0,
  dur_get_ox_survive = 9.5,

  dur_get_ox_die = 7.6,
  dur_not_get_ox_survive = 9.5*0.5,
  dur_not_get_ox_die = 7.6*0.5,

  dur_get_mv_survive = 11.3,

  dur_get_mv_die = 10.1,
  tt_dur_get_mv_survive = 0,

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
                                    walker=walker,
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
                                    tt_ICU_beds=tt_ICU_beds,
                                    tt_dur_get_mv_survive=tt_dur_get_mv_survive,
                                    tt_dur_get_ox_survive=tt_dur_get_ox_survive)

  # Running the Model
  mod <- explicit_SEIR(user = pars, unused_user_action = "ignore")
  t <- seq(from = 1, to = time_period/dt)

  # if we ar doing day return then proceed in steps of day length
  # We also will do an extra day so we know the numebr of infections/deaths
  # that would happen in the last day
  if (day_return) {
    t <- round(seq(1/dt, length(t), by=1/dt))
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
  parameters$prob_hosp <- pars$prob_hosp
  parameters$prob_severe <- pars$prob_severe
  parameters$prob_non_severe_death_treatment <- pars$prob_non_severe_death_treatment
  parameters$prob_severe_death_treatment <- pars$prob_severe_death_treatment
  parameters$prob_non_severe_death_no_treatment <- pars$prob_non_severe_death_no_treatment
  parameters$prob_severe_death_no_treatment <- pars$prob_severe_death_no_treatment

  out <- list(output = results, parameters = parameters, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)
}

#' Run the deterministic explicit SEIR model
#'
#' @inheritParams run_explicit_SEEIR_model
#' @param mod_gen An odin model generation function. Default:
#' `explicit_SEIR_deterministic`
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' \dontrun{
#' pop <- get_population("Afghanistan")
#' m <- get_mixing_matrix("Afghanistan")
#' run_deterministic_SEIR_model(pop$n, m, c(0, 50), c(3, 3/2), 365, 100000,
#' 1000000)
#' }
run_deterministic_SEIR_model <- function(

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
  prob_hosp = NULL,
  prob_severe = NULL,
  prob_non_severe_death_treatment = NULL,
  prob_non_severe_death_no_treatment = NULL,
  prob_severe_death_treatment = NULL,
  prob_severe_death_no_treatment = NULL,
  p_dist = probs$p_dist,

  # durations
  dur_E  = 4.6,
  dur_IMild = 2.1,
  dur_ICase = 4.5,

  dur_get_ox_survive = 9.5,
  tt_dur_get_ox_survive = 0,

  dur_get_ox_die = 7.6,
  dur_not_get_ox_survive = 9.5*0.5,
  dur_not_get_ox_die = 7.6*0.5,

  dur_get_mv_survive = 11.3,
  tt_dur_get_mv_survive = 0,

  dur_get_mv_die = 10.1,
  dur_not_get_mv_survive = 11.3*0.5,
  dur_not_get_mv_die = 1,

  dur_rec = 3.4,

  # health system capacity
  hosp_bed_capacity = NULL,
  ICU_bed_capacity = NULL,
  tt_hosp_beds = 0,
  tt_ICU_beds = 0,

  seeding_cases = NULL,
  mod_gen = explicit_SEIR_deterministic
  ) {

  # replicates has to be 1
  replicates <- 1

  # Grab function arguments
  args <- as.list(environment())
  set.seed(seed)

  # create parameter list
  pars <- parameters_explicit_SEEIR(country=country,
                                    population=population,
                                    tt_contact_matrix=tt_contact_matrix*dt,
                                    contact_matrix_set=contact_matrix_set,
                                    R0=R0,
                                    tt_R0=tt_R0*dt,
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
                                    tt_hosp_beds=tt_hosp_beds*dt,
                                    tt_ICU_beds=tt_ICU_beds*dt,
                                    tt_dur_get_mv_survive=tt_dur_get_mv_survive*dt,
                                    tt_dur_get_ox_survive=tt_dur_get_ox_survive*dt)

  # handling time variables for js
  pars$tt_beta <- I(pars$tt_beta)
  pars$beta_set <- I(pars$beta_set)
  pars$tt_hosp_beds <- I(pars$tt_hosp_beds)
  pars$hosp_beds <- I(pars$hosp_beds)
  pars$tt_ICU_beds <- I(pars$tt_ICU_beds)
  pars$ICU_beds <- I(pars$ICU_beds)
  pars$tt_matrix <- I(pars$tt_matrix)
  pars$gamma_get_mv_survive <- I(pars$gamma_get_mv_survive)
  pars$tt_dur_get_mv_survive <- I(pars$tt_dur_get_mv_survive)
  pars$gamma_get_ox_survive <- I(pars$gamma_get_ox_survive)
  pars$tt_dur_get_ox_survive <- I(pars$tt_dur_get_ox_survive)


  # Running the Model
  mod <- mod_gen(user = pars, unused_user_action = "ignore")
  t <- seq(from = 1, to = time_period, by = dt)

  # if we ar doing day return then proceed in steps of day length
  # We also will do an extra day so we know the numebr of infections/deaths
  # that would happen in the last day
  if (day_return) {
    t <- round(seq(from = 1, to = time_period))
  }
  results <- mod$run(t, replicate = replicates)

  # coerce to array
  results <- array(results, dim = c(dim(results),1), dimnames = dimnames(results))

  # Summarise inputs
  parameters <- args
  parameters$population <- pars$population
  parameters$hosp_bed_capacity <- pars$hosp_beds
  parameters$ICU_bed_capacity <- pars$ICU_beds
  parameters$beta_set <- pars$beta_set
  parameters$seeding_cases <- pars$E1_0
  parameters$contact_matrix_set <- pars$contact_matrix_set
  parameters$prob_hosp <- pars$prob_hosp
  parameters$prob_severe <- pars$prob_severe
  parameters$prob_non_severe_death_treatment <- pars$prob_non_severe_death_treatment
  parameters$prob_severe_death_treatment <- pars$prob_severe_death_treatment
  parameters$prob_non_severe_death_no_treatment <- pars$prob_non_severe_death_no_treatment
  parameters$prob_severe_death_no_treatment <- pars$prob_severe_death_no_treatment

  out <- list(output = results, parameters = parameters, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)

}
