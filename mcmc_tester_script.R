# Load Libraries
library(tictoc)

# Load squire
devtools::load_all()

# Simulate Data
country <- "Algeria"
true_start_date <- as.Date("2020-02-01")
true_R0 <- 3
true_Meff <- 2
true_Meff_pl <- 2
date_Meff_change <- as.Date("2020-04-20")
R0_change <- c(seq(true_R0, 0.5, -0.5), 1.5)/true_R0
tt_R0 <- c(0, 40, 50, 65, 70, 80, 100)
date_R0_change <- true_start_date + tail(tt_R0, -1)
date_switch <- which(date_R0_change >= date_Meff_change)
get <- squire::run_explicit_SEEIR_model(country,
                                        R0 = exp(log(true_R0) - c(true_Meff*(1-R0_change[1:(min(date_switch)-1)]),
                                                                  true_Meff_pl*(1-R0_change[min(date_switch):length(R0_change)]))),
                                        tt_R0 = tt_R0,
                                        replicates = 1,
                                        day_return = TRUE,
                                        time_period = 120,
                                        dt = 0.25,
                                        hosp_bed_capacity = 10000000,
                                        ICU_bed_capacity = 10000000)

index <- squire:::odin_index(get$model)
deaths <- c(0, diff(rowSums(get$output[,index$D,1])))
plot(deaths)
dates <- seq.Date(true_start_date, by = 1, length.out = length(deaths))
df <- data.frame("date" = dates, "deaths" = deaths)
df <- tail(df, 100)
date_R0_change <- true_start_date + tail(c(0, 40, 50, 65, 70, 80, 100), -1)
R0_change <- head(R0_change, -1)

# PMCMC Parameters
pars_init = list('start_date' = as.Date("2020-02-03"), 'R0' = 3, 'Meff' = 2, 'Meff_pl' = 5)
pars_min = list('start_date' = as.Date("2020-01-31"), 'R0' = 1, 'Meff' = 1, 'Meff_pl' = 1)
pars_max = list('start_date' = as.Date("2020-02-15"), 'R0' = 4, 'Meff' = 8, 'Meff_pl' = 8)
pars_discrete = list('start_date' = TRUE, 'R0' = FALSE, 'Meff' = FALSE, 'Meff_pl' = FALSE)
pars_obs = list(phi_cases = 0.1, k_cases = 2, phi_death = 1, k_death = 2, exp_noise = 1e6)

# Covriance Matrix
proposal_kernel <- diag(length(names(pars_init))) * 0.05
rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)
proposal_kernel["start_date", "start_date"] <- 1

# Model Parameters and Interventions
contact_matrix <- get_mixing_matrix(country)
model_params <- explicit_model()$parameter_func(country = country,
                                                population = get_population(country)$n,
                                                dt = 1/4,
                                                contact_matrix_set = contact_matrix,
                                                tt_contact_matrix = 0,
                                                hosp_bed_capacity = 10000000,
                                                tt_hosp_beds = 0,
                                                ICU_bed_capacity = 10000000,
                                                tt_ICU_beds = 0)
interventions <- list(R0_change = R0_change,
                      date_R0_change = date_R0_change,
                      date_Meff_change = date_Meff_change,
                      date_contact_matrix_set_change = NULL,
                      contact_matrix_set = contact_matrix,
                      date_ICU_bed_capacity_change = NULL,
                      ICU_bed_capacity = 10000000,
                      date_hosp_bed_capacity_change = NULL,
                      hosp_bed_capacity = 10000000)
inputs <- list(data = df,
               n_mcmc = 10,
               model_params = model_params,
               interventions = interventions,
               pars_obs = pars_obs,
               squire_model = explicit_model(),
               pars = list(pars_obs = pars_obs,
                           pars_init = pars_init,
                           pars_min = pars_min,
                           pars_max = pars_max,
                           proposal_kernel = proposal_kernel,
                           pars_discrete = pars_discrete),
               n_particles = 40)

# MCMC Functions - Prior and Likelihood Calculation
logprior <- function(pars){
  assert_in(names(pars), c("start_date", "R0", "Meff", "Meff_pl")) # good sanity check
  ret <- dunif(x = pars[["start_date"]], min = -22, max = -2, log = TRUE) +
    dnorm(x = pars[["R0"]], mean = 2.5, sd = 1, log = TRUE) +
    dnorm(x = pars[["Meff"]], mean = 1.8, sd = 2, log = TRUE) +
    dnorm(x = pars[["Meff_pl"]], mean = 5.6, sd = 2, log = TRUE)
  return(ret)
}

calc_ll <- function(pars) {
  X <- calc_loglikelihood(pars = pars,
                          data = df,
                          squire_model = explicit_model(),
                          model_params = model_params,
                          interventions = interventions,
                          pars_obs = pars_obs,
                          n_particles = 40,
                          forecast_days = 0,
                          return = "ll")
  return(X)
}

# Checking that True Parameters Give a Decent Likelihood and Other Parameters = Worse
true_pars <- list('start_date' = as.Date("2020-02-01"), 'R0' = 3, 'Meff' = 2, 'Meff_pl' = 2)
true <- calc_ll(true_pars)$log_likelihood
true_pars$start_date <- -10
logprior(true_pars)

worse_pars <- list('start_date' = as.Date("2020-02-01"), 'R0' = 3, 'Meff' = 2, 'Meff_pl' = 0)
worse <- calc_ll(worse_pars)$log_likelihood
worse_pars$start_date <- -10
logprior(worse_pars)

# Running a Single MCMC Chain
tic()
niter <- 500
x <- run_mcmc_chain(inputs = inputs,
                    curr_pars = pars_init,
                    calc_lprior = logprior,
                    calc_ll = calc_ll,
                    n_mcmc = niter,
                    first_data_date = df$date[1],
                    output_proposals = FALSE,
                    required_acceptance_ratio = 0.20,
                    start_covariance_adaptation = 2000,
                    start_scaling_factor_adaptation = 2000,
                    initial_scaling_factor = 1,
                    proposal_kernel = proposal_kernel,
                    pars_discrete = pars_discrete,
                    pars_min = pars_min,
                    pars_max = pars_max)
toc()

plot(x$results[1:niter, 1])
plot(x$results[1:niter, 2])
plot(x$results[1:niter, 3])
plot(x$results[1:niter, 4])

cov(cbind(as.numeric(x$results[1:niter, 1]), x$results[1:niter, 2:4]))



table(x$results[, 1])
x$acceptance_ratio
x$scaling_factor

cov <- x$covariance_matrix
element <- lapply(cov, function(x) {
  tmp <- x[4, 4]
})
plot(unlist(element))


