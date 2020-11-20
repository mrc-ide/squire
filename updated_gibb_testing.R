# Loading Required Libraries
library(lubridate); library(squire); library(tictoc); library(dplyr); library(tidyverse);
library(apothecary)

# Load apothecary stuff
devtools::load_all()

ecdc <- readRDS("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Inputs/ecdc_all.rds")
interventions <- readRDS("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Inputs/google_brt.rds")
pars_init <- readRDS("N:/Charlie/apothecary_fitting/apothecary/analysis_Figure3/Inputs/pars_init.rds")

# Loading In Example Initial Parameters and Defining Country and Date Fitting Being Applied To
pars_init <- readRDS("cluster_running/Inputs/pars_init.rds")
can_parms <- pars_init$CAN
iso3c <- "CAN"
date <- "2020-11-17"

# Loading in ECDC/Worldometer Deaths Data
if (iso3c %in% c("BOL", "ITA", "FRA", "ECU", "CHL", "COD", "ESP", "IRN",
                 "JPN", "GUF","KGZ", "PER", "MEX", "HKG", "MAC", "TWN",
                 "SDN")) {
  ecdc <- readRDS("cluster_running/Inputs/worldometers_all.rds")
} else {
  ecdc <- readRDS("cluster_running/Inputs/ecdc_all.rds")
}

# Removing Deaths Followed by 21 Days of No Deaths
country <- squire::population$country[match(iso3c, squire::population$iso3c)[1]]
df <- ecdc[which(ecdc$countryterritoryCode == iso3c),]
if(sum(df$deaths>0)>1) {
  if(tail(diff(which(df$deaths>0)),1) > 21) {
    df$deaths[tail(which(df$deaths>0),1)] <- 0
  }
}

data <- df[,c("dateRep", "deaths", "cases")]
names(data)[1] <- "date"
data <- data[order(data$date),]
data$date <- as.Date(data$date)

# Removing Dates Up to First Death After This
first_report <- which(data$deaths>0)[1]
missing <- which(data$deaths == 0 | is.na(data$deaths))
to_remove <- missing[missing<first_report]
if(length(to_remove) > 0) {
  if(length(to_remove) == (nrow(data)-1)) {
    data <- data[-head(to_remove,-1),]
  } else {
    data <- data[-to_remove,]
  }
}

# Loading in BRT Mobility Data and Processing
interventions <- readRDS("cluster_running/Inputs/google_brt.rds")
R0_change <- interventions[[iso3c]]$C
date_R0_change <- interventions[[iso3c]]$date
R0_change <- R0_change[as.Date(date_R0_change) <= date]
date_R0_change <- date_R0_change[as.Date(date_R0_change) <= date]

# Setting Up the Fortnightly Splines
date_0 <- date
Rt_rw_duration <- 14 # change to pars_init
last_shift_date <- as.Date(can_parms$date_Meff_change) + 7
remaining_days <- as.Date(date_0) - last_shift_date - 21
rw_needed <- as.numeric(round(remaining_days/Rt_rw_duration))
pars_init_rw <- as.list(can_parms[grep("Rt_rw_\\d",names(can_parms))])
if (is.null(can_parms)) {
  pars_init_rw <- as.list(rep(0, rw_needed))
} else {
  pars_init_rw <- as.list(can_parms[grep("Rt_rw_\\d",names(can_parms))])
  if(length(pars_init_rw) < rw_needed) {
    pars_init_rw[[rw_needed]] <- 0
  }
  pars_init_rw <- lapply(pars_init_rw, function(x) {
    if(is.null(x)){
      return(0)
    } else {
      return(x)
    }})
}
pars_min_rw <- as.list(rep(-5, rw_needed))
pars_max_rw <- as.list(rep(5, rw_needed))
pars_discrete_rw <- as.list(rep(FALSE, rw_needed))
names(pars_init_rw) <- names(pars_min_rw) <- names(pars_max_rw) <- names(pars_discrete_rw) <- paste0("Rt_rw_", seq_len(rw_needed))


# PMCMC Prior Bounds, Initial Parameters and Observation Model Parameters
n_chains <- 1
if (n_chains == 1) {
  pars_init <- list('start_date' = can_parms$start_date,
                    'R0' = can_parms$R0,
                    'Meff' = can_parms$Meff,
                    'Meff_pl' = can_parms$Meff_pl,
                    "Rt_shift" = 0,
                    "Rt_shift_scale" = can_parms$Rt_shift_scale)
} else {
  pars_init <- list(
    list('start_date' = as.Date(can_parms$start_date) - 6,
         'R0' = can_parms$R0 * 0.6,
         'Meff' = can_parms$Meff * 1.3,
         'Meff_pl' = can_parms$Meff_pl,
         "Rt_shift" = 0,
         "Rt_shift_scale" = can_parms$Rt_shift_scale),
    list('start_date' = as.Date(can_parms$start_date) - 3,
         'R0' = can_parms$R0 * 0.8,
         'Meff' = can_parms$Meff * 1.3,
         'Meff_pl' = can_parms$Meff_pl * 0.9,
         "Rt_shift" = 0,
         "Rt_shift_scale" = can_parms$Rt_shift_scale),
    list('start_date' = as.Date(can_parms$start_date),
         'R0' = can_parms$R0,
         'Meff' = can_parms$Meff,
         'Meff_pl' = can_parms$Meff_pl,
         "Rt_shift" = 0,
         "Rt_shift_scale" = can_parms$Rt_shift_scale),
    list('start_date' = as.Date(can_parms$start_date) + 4,
         'R0' = can_parms$R0 * 1.3,
         'Meff' = can_parms$Meff * 0.8,
         'Meff_pl' = can_parms$Meff_pl * 0.6,
         "Rt_shift" = 0,
         "Rt_shift_scale" = can_parms$Rt_shift_scale))
}

pars_min <- list('start_date' = "2020-01-15",  # come back to this and sort so that we can define off start_date initial
                 'R0' = 1.3,
                 'Meff' = 0.5,
                 'Meff_pl' = 0,
                 "Rt_shift" = 0,
                 "Rt_shift_scale" = 0.1)
pars_max <- list('start_date' = "2020-02-29", # come back to this and sort so that we can define off start_date initial
                 'R0' = 6,
                 'Meff' = 10,
                 'Meff_pl' = 1,
                 "Rt_shift" = 0.001,
                 "Rt_shift_scale" = 10)
pars_discrete <- list('start_date' = TRUE,
                      'R0' = FALSE,
                      'Meff' = FALSE,
                      'Meff_pl' = FALSE,
                      "Rt_shift" = FALSE,
                      "Rt_shift_scale" = FALSE)
pars_obs <- list(phi_cases = 1,
                 k_cases = 2,
                 phi_death = 1,
                 k_death = 2,
                 exp_noise = 1e6)
pars_obs$treated_deaths_only <- FALSE

# add in the spline list
if (n_chains > 1) {
  pars_init <- lapply(pars_init, append, pars_init_rw)
} else {
  pars_init <- append(pars_init, pars_init_rw)
}
pars_min <- append(pars_min, pars_min_rw)
pars_max <- append(pars_max, pars_max_rw)
pars_discrete <- append(pars_discrete, pars_discrete_rw)

# Proposal Covariance Matrix
# proposal_kernel <- diag(length(names(pars_init))) * 0.3
# rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)
# proposal_kernel["start_date", "start_date"] <- 1.5 # consider changing
if (n_chains == 1) {
  proposal_kernel <- diag(length(names(pars_init)) - 1) * 0.3
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)[-1]
} else {
  proposal_kernel <- diag(length(names(pars_init[[1]])) - 1) * 0.3
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init[[1]])[-1]
}

# MCMC Functions - Prior and Likelihood Calculation
logprior <- function(pars){
  ret <- dunif(x = pars[["start_date"]], min = -55, max = -10, log = TRUE) +
    dnorm(x = pars[["R0"]], mean = 3, sd = 1, log = TRUE) +
    dnorm(x = pars[["Meff"]], mean = 3, sd = 3, log = TRUE) +
    dunif(x = pars[["Meff_pl"]], min = 0, max = 1, log = TRUE) +
    dnorm(x = pars[["Rt_shift"]], mean = 0, sd = 1, log = TRUE) +
    dunif(x = pars[["Rt_shift_scale"]], min = 0.1, max = 10, log = TRUE)

  # get rw spline parameters
  if(any(grepl("Rt_rw", names(pars)))) {
    Rt_rws <- pars[grepl("Rt_rw", names(pars))]
    for (i in seq_along(Rt_rws)) {
      ret <- ret + dnorm(x = Rt_rws[[i]], mean = 0, sd = 0.2, log = TRUE)
    }
  }
  return(ret)
}
calc_lprior <- logprior

log_likelihood <- calc_loglikelihood
calc_ll <- function(pars) {
  X <- log_likelihood(pars = pars,
                      data = data,
                      squire_model = apothecary_deterministic_model(),
                      model_params = model_params,
                      interventions = interventions,
                      pars_obs = pars_obs,
                      n_particles = n_particles,
                      forecast_days = 0,
                      Rt_args = Rt_args,
                      return = "ll"
  )
  X
}

model_params <- apothecary_deterministic_model()$parameter_func(country = "Canada",
                                                                population = NULL,
                                                                dt = 1,
                                                                contact_matrix_set = NULL,
                                                                tt_contact_matrix = 0,
                                                                hosp_bed_capacity = 100000000,
                                                                tt_hosp_beds = 0,
                                                                ICU_bed_capacity = 100000000,
                                                                tt_ICU_beds = 0)
interventions <- list(R0_change = R0_change,
                      date_R0_change = date_R0_change,
                      date_contact_matrix_set_change = NULL,
                      contact_matrix_set = model_params$contact_matrix_set,
                      date_ICU_bed_capacity_change = NULL,
                      ICU_bed_capacity = 100000000,
                      date_hosp_bed_capacity_change = NULL,
                      hosp_bed_capacity = 100000000)
Rt_args = squire:::Rt_args_list(
  plateau_duration = 7,
  date_Meff_change = can_parms$date_Meff_change,
  scale_Meff_pl = TRUE,
  Rt_shift_duration = 7,
  Rt_rw_duration = Rt_rw_duration)

n_mcmc <- 1000
inputs <- list(
  data = data,
  n_mcmc = n_mcmc,
  model_params = model_params,
  interventions = interventions,
  pars_obs = pars_obs,
  Rt_args = Rt_args,
  squire_model = apothecary_deterministic_model(),
  pars = list(pars_obs = pars_obs,
              pars_init = list(pars_init),
              pars_min = pars_min,
              pars_max = pars_max,
              proposal_kernel = proposal_kernel,
              pars_discrete = pars_discrete),
  n_particles = 1)

tic()
x <- run_mcmc_chain(inputs = inputs,
                    curr_pars = pars_init,
                    calc_lprior = calc_lprior,
                    calc_ll = calc_ll,
                    n_mcmc = n_mcmc,
                    first_data_date = data$date[1],
                    output_proposals = FALSE,
                    required_acceptance_ratio = 0.20,
                    start_adaptation = 200,
                    proposal_kernel = proposal_kernel,
                    pars_discrete = pars_discrete,
                    pars_min = pars_min,
                    pars_max = pars_max)
toc()
