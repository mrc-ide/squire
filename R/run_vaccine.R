
#' Return the default vaccine parameters for modelling
#' @return list of default vaccine parameters
default_vaccine_pars <- function() {
  list(dur_R = 365,
       vaccination_target = rep(1, 17),
       dur_V = 365,
       vaccine_efficacy_infection = rep(0.95, 17),
       vaccine_efficacy_disease = rep(0.95, 17),
       max_vaccine = 1000,
       tt_vaccine = 0,
       dur_vaccine_delay = 14)
}

vaccine_pars <- default_vaccine_pars()


#' Run the deterministic explicit SEIR model with vaccination
#'
#' @inheritParams run_explicit_SEEIR_model
#' @param dur_R Mean duration of natural immunity (days)
#' @param vaccination_target Index of age group targets for vaccination. Must be 0
#' (not vaccinated) or 1 (vaccinated) for each age group.
#' @param dur_V Mean duration of vaccine-derived immunity (days)
#' @param vaccine_efficacy_infection Efficacy of vaccine against infection (by age).
#' An efficacy of 1 will reduce FOI by 100 percent, an efficacy of 0.2 will reduce FOI by 20 percent etc.
#' @param vaccine_efficacy_disease Efficacy of vaccine against severe (requiring hospitilisatin) disease (by age).
#' An efficacy of 1 will reduce the probability of hospitalisation by 100 percent,
#' an efficacy of 0.2 will reduce the probability of hospitalisation by 20 percent etc.
#' @param max_vaccine The maximum number of individuals who can be vaccinated per day.
#' @param tt_vaccine Time change points for vaccine capacity (\code{max_vaccine}).
#' @param dur_vaccine_delay Mean duration of period from vaccination to vaccine protection.
#' @param framework Model framework to run: stochastic or deterministic.
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' \dontrun{
#' pop <- get_population("Afghanistan")
#' m <- get_mixing_matrix("Afghanistan")
#' output <- run_deterministic_SEIR_vaccine_model(
#'   population = pop$n,contact_matrix_set = m,
#'   hosp_bed_capacity = 100000,
#'   ICU_bed_capacity = 1000000,
#'   day_return = TRUE
#' )
#' }
run_vaccine <- function(
  
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
  
  # vaccine
  dur_R = vaccine_pars$dur_R,
  vaccination_target = vaccine_pars$vaccination_target,
  dur_V = vaccine_pars$dur_V,
  vaccine_efficacy_infection = vaccine_pars$vaccine_efficacy_infection,
  vaccine_efficacy_disease = vaccine_pars$vaccine_efficacy_disease,
  max_vaccine = vaccine_pars$max_vaccine,
  tt_vaccine = vaccine_pars$tt_vaccine,
  dur_vaccine_delay = vaccine_pars$dur_vaccine_delay,
  
  # health system capacity
  hosp_bed_capacity = NULL,
  ICU_bed_capacity = NULL,
  tt_hosp_beds = 0,
  tt_ICU_beds = 0,
  
  seeding_cases = NULL,
  framework = "deterministic"
) {
  
  
  # Grab function arguments
  args <- as.list(environment())
  set.seed(seed)
  
  if(framework == "deterministic"){
    replicates <- 1
    mod_gen = explicit_SEIR_vaccine_deterministic
  } else {
    mod_gen = explicit_SEIR_vaccine_stochastic
  }
  
  # create parameter list
  pars <- parameters_vaccine(country=country,
                             population=population,
                             tt_contact_matrix=tt_contact_matrix * dt,
                             contact_matrix_set=contact_matrix_set,
                             R0=R0,
                             tt_R0=tt_R0 * dt,
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
                             dur_R = dur_R,
                             hosp_bed_capacity=hosp_bed_capacity,
                             ICU_bed_capacity=ICU_bed_capacity,
                             tt_hosp_beds=tt_hosp_beds * dt,
                             tt_ICU_beds=tt_ICU_beds * dt,
                             vaccination_target = vaccination_target,
                             dur_V = dur_V,
                             vaccine_efficacy_infection = vaccine_efficacy_infection,
                             vaccine_efficacy_disease = vaccine_efficacy_disease,
                             max_vaccine = max_vaccine,
                             tt_vaccine = tt_vaccine * dt,
                             dur_vaccine_delay = dur_vaccine_delay)
  
  # handling time variables for js
  pars$tt_beta <- I(pars$tt_beta)
  pars$beta_set <- I(pars$beta_set)
  pars$tt_hosp_beds <- I(pars$tt_hosp_beds)
  pars$hosp_beds <- I(pars$hosp_beds)
  pars$tt_ICU_beds <- I(pars$tt_ICU_beds)
  pars$ICU_beds <- I(pars$ICU_beds)
  pars$tt_matrix <- I(pars$tt_matrix)
  pars$tt_vaccine <- I(pars$tt_vaccine)
  
  
  # Running the Model
  mod <- mod_gen(user = pars, unused_user_action = "ignore")
  t <- seq(from = 1, to = time_period, by = dt)
  
  # if we ar doing day return then proceed in steps of day length
  # We also will do an extra day so we know the numebr of infections/deaths
  # that would happen in the last day
  if (day_return) {
    t <- round(seq(1/dt, length(t)+(1/dt), by=1/dt))
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
  
  out <- list(output = results, parameters = parameters, model = mod)
  out <- structure(out, class = "squire_simulation")
  return(out)
  
}
