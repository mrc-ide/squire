
#------------------------------------------------
test_that("drjacoby overall test", {

  set.seed(12)

  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 3,
                   'Meff'           = 2,
                   "rf"             = 0.25) # correct rf for the data
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1,
                  "rf"              = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5,
                  "rf"              = 1)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE,
                       'rf'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6,
                  rtol = 1e-8,
                  atol = 1e-8)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = squire:::deterministic_model()
  n_particles = 2

  # proposal kernel covriance
  drjl <- list(cluster = NULL, rungs = 2, pb_markdown = TRUE)


  # following checks to see that it is being correctly used to get better likelihoods
  # given contribution from the sero ll
  Sys.setenv("SQUIRE_PARALLEL_DEBUG"=TRUE)
  out <- drjacoby_mcmc(data = data,
               n_mcmc = 5,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 20,
               burnin = 5,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country,
               drjacoby_list = drjl)

  expect_true("drjacoby_out" %in% names(out$pmcmc_results))

})
