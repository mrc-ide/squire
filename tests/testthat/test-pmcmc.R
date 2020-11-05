context("pmcmc")

#------------------------------------------------
test_that("pmcmc fitting works", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  steps_per_day = 2
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  set.seed(93L)
  out <- pmcmc(data = data,
               n_mcmc = 5,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               Rt_args = list(date_Meff_change = NULL),
               country = country)

  expect_named(out, c("output", "parameters", "model", "replicate_parameters", "pmcmc_results", "interventions"))
  expect_warning(expect_s3_class(plot(out, what = "cases", particle_fit = TRUE), "gg"))
  expect_warning(expect_s3_class(plot(out, what = "deaths", particle_fit = TRUE), "gg"))
  expect_error(plot(out, what = "rubbish", particle_fit = TRUE),"must be one of")


  # DATE CHECKS DATE_R0
  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = list('start_date'      = as.Date("2020-05-20"),
                                 'R0'              = 5,
                                 'Meff'            = 1),
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country),
    "Maximum start date must be at least 2 days before the first date in data"
  )

  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = list('start_date'      = as.Date("2020-02-20"),
                                 'R0'              = 1,
                                 'Meff'            = 1),
                 pars_max = list('start_date'      = as.Date("2020-02-20"),
                                 'R0'              = 5,
                                 'Meff'            = 5),
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country),
    "start_date init must be greater than"
  )

  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = 0.5,
                 date_R0_change = "2022-05-10",
                 country = country),
    "Last date in date_R0_change is greater than the last date in data"
  )

  # DATE CHECKS DATE_CONTACT
  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country,
                 baseline_contact_matrix = contact_matrices[[1]],
                 contact_matrix_set = contact_matrices[1],
                 date_contact_matrix_set_change = "2020-05-10"),
    "Last date in date_contact_matrix_set_change is greater than"
  )

  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country,
                 contact_matrix_set = list(contact_matrices[[1]]),
                 date_contact_matrix_set_change = date_R0_change[1]),
    "baseline_contact_matrix can't be NULL if date_contact_matrix_set_change"
  )

  out <- pmcmc(data = data,
               n_mcmc = 5,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               Rt_args = list(date_Meff_change = NULL),
               country = country,
               baseline_contact_matrix = contact_matrices[[1]],
               contact_matrix_set = list(contact_matrices[[1]]),
               date_contact_matrix_set_change = date_R0_change[1])



  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country,
                 baseline_contact_matrix = contact_matrices[[1]],
                 contact_matrix_set = contact_matrices[1],
                 date_contact_matrix_set_change = "2020-02-01"),
    "First date in date_contact_matrix_set_change is earlier than maximum start date"
  )

  # DATE CHECKS date_hosp
  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 date_hosp_bed_capacity_change = "2022-05-10",
                 baseline_hosp_bed_capacity = 10,
                 hosp_bed_capacity = 100,
                 country = country),
    "Last date in date_hosp_bed_capacity_change is greater than the last date "
  )

  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 date_hosp_bed_capacity_change = "2020-02-02",
                 baseline_hosp_bed_capacity = 10,
                 hosp_bed_capacity = 100,
                 country = country),
    "First date in date_hosp_bed_capacity_change is earlier than maximum start"
  )

  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 hosp_bed_capacity = 10,
                 date_hosp_bed_capacity_change = date_R0_change[1],
                 country = country),
    "baseline_hosp_bed_capacity can't be NULL if date_hosp_bed_capacity_change"
  )

  out <- pmcmc(data = data,
               n_mcmc = 5,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               baseline_hosp_bed_capacity = 5,
               hosp_bed_capacity = 10,
               date_hosp_bed_capacity_change = date_R0_change[1],
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               Rt_args = list(date_Meff_change = NULL),
               country = country)


  # DATE CHECKS DATE_ICU
  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country,
                 baseline_ICU_bed_capacity = 10,
                 ICU_bed_capacity = 100,
                 date_ICU_bed_capacity_change = "2020-05-10")
  )

  expect_error(
    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country,
                 baseline_ICU_bed_capacity = 10,
                 ICU_bed_capacity = 100,
                 date_ICU_bed_capacity_change = "2020-02-01")
  )

  expect_error(

    out <- pmcmc(data = data,
                 n_mcmc = 5,
                 log_likelihood = NULL,
                 log_prior = NULL,
                 n_particles = 2,
                 steps_per_day = steps_per_day,
                 output_proposals = FALSE,
                 n_chains = 1,
                 replicates = 2,
                 burnin = 0,
                 squire_model = squire_model,
                 pars_init = pars_init,
                 pars_min = pars_min,
                 pars_max = pars_max,
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country,
                 ICU_bed_capacity = 100,
                 date_ICU_bed_capacity_change = date_R0_change[1])
  )

  out <- pmcmc(data = data,
               n_mcmc = 5,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country,
               baseline_ICU_bed_capacity = 5,
               ICU_bed_capacity = 10,
               date_ICU_bed_capacity_change = date_R0_change[1]
  )

})


#------------------------------------------------
test_that("pmcmc non future works", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)


  Sys.setenv("SQUIRE_PARALLEL_DEBUG"=TRUE)
  out <- pmcmc(data = data,
               n_mcmc = 5,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country)


})


#------------------------------------------------
test_that("pmcmc deterministic", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 4

  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  out <- pmcmc(data = data,
               n_mcmc = 1000,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = deterministic_model(),
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               Rt_args = list(date_Meff_change = NULL),
               country = country)


  expect_true(inherits(out$pmcmc_results$inputs$squire_model, "deterministic"))
  expect_error(get <- projections(out), "projections needs either time_period")

})


#-------------------------------------
test_that("pmcmc user pop and contact", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  pop <- get_population("Nigeria")
  mat <- get_mixing_matrix("Nigeria")*0.9

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  out <- pmcmc(data = data,
               n_mcmc = 5,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country,
               population = pop$n,
               baseline_contact_matrix = mat)

  expect_true(
    identical(out$pmcmc_results$inputs$model_params$contact_matrix_set[[1]], mat)
  )

})


#-------------------------------------
test_that("pmcmc future", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" =  FALSE)

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  expect_message(out <- pmcmc(data = data,
                              n_mcmc = 4,
                              log_likelihood = NULL,
                              log_prior = NULL,
                              n_particles = 2,
                              steps_per_day = steps_per_day,
                              output_proposals = FALSE,
                              n_chains = 2,
                              replicates = 2,
                              burnin = 0,
                              squire_model = squire_model,
                              pars_init = pars_init,
                              pars_min = pars_min,
                              pars_max = pars_max,
                              pars_discrete = pars_discrete,
                              pars_obs = pars_obs,
                              proposal_kernel = proposal_kernel,
                              R0_change = R0_change,
                              date_R0_change = date_R0_change,
                              country = country),
                 "rhat")


})


#-------------------------------------
test_that("pmcmc multiple chains and rhat", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  set.seed(93L)
  out <- pmcmc(data = data,
               n_mcmc = 200,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 3,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country)
  expect_named(out$pmcmc_results$rhat, c("psrf","mpsrf"))

  pl <- plot(out$pmcmc_results)
  expect_warning(expect_s3_class(plot(out, particle_fit = TRUE), "gg"))

  summa <- summary(out$pmcmc_results, burn_in = 10)
  expect_named(summa, c("summary", "corr_mat", "sd"))

})

#-------------------------------------
test_that("pmcmc single chain and rhat", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  out <- pmcmc(data = data,
               n_mcmc = 50,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country)
  expect_false("rhat" %in% names(out$pmcmc_results))

  pl <- plot(out$pmcmc_results)
  expect_warning(expect_s3_class(plot(out, particle_fit = TRUE), "gg"))

  summa <- summary(out$pmcmc_results, burn_in = 10)
  expect_named(summa, c("summary", "corr_mat", "sd"))

})


#-------------------------------------
test_that("pmcmc meff date", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 1,
                   'Meff_pl'        = 1)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 1,
                  'Meff_pl'         = 1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5,
                  'Meff_pl'         = 10)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE,
                       'Meff_pl'    = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  out <- pmcmc(data = data,
               n_mcmc = 50,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               Rt_args = list(date_Meff_change = "2020-03-20",
                              plateau_duration = 7),
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country)
  expect_false("rhat" %in% names(out$pmcmc_results))

  pl <- plot(out$pmcmc_results)
  expect_warning(expect_s3_class(plot(out, particle_fit = TRUE), "gg"))

  summa <- summary(out$pmcmc_results, burn_in = 10)
  expect_named(summa, c("summary", "corr_mat", "sd"))

})

#-------------------------------------
test_that("pmcmc 6p meff date", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 1,
                   'Meff_pl'        = 0.2,
                   "Rt_shift"       = 2,
                   "Rt_shift_scale" = 5)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 1,
                  'Meff_pl'         = 0,
                  "Rt_shift"       = 0,
                  "Rt_shift_scale" = 0)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5,
                  'Meff_pl'         = 1,
                  "Rt_shift"       = 5,
                  "Rt_shift_scale" = 10)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE,
                       'Meff_pl'    = FALSE,
                       "Rt_shift"       = FALSE,
                       "Rt_shift_scale" = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  out <- pmcmc(data = data,
               n_mcmc = 50,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               Rt_args = Rt_args_list(date_Meff_change = "2020-03-20",
                                      scale_Meff_pl = TRUE,
                                      Rt_shift_duration = 30,
                                      plateau_duration = 7),
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country)
  expect_false("rhat" %in% names(out$pmcmc_results))

  pl <- plot(out$pmcmc_results)
  expect_warning(expect_s3_class(plot(out, particle_fit = TRUE), "gg"))

  summa <- summary(out$pmcmc_results, burn_in = 10)
  expect_named(summa, c("summary", "corr_mat", "sd"))

})


#-------------------------------------
test_that("Start date and R0 only pmcmc", {
  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  set.seed(1)
  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  out <- pmcmc(data = data,
               n_mcmc = 50,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country)
  expect_false("rhat" %in% names(out$pmcmc_results))

  pl <- plot(out$pmcmc_results)
  expect_warning(expect_s3_class(plot(out, particle_fit = TRUE), "gg"))

  summa <- summary(out$pmcmc_results, burn_in = 10)
  expect_named(summa, c("summary", "corr_mat", "sd"))

  # check that evaluate Rt works
  from_eval <- evaluate_Rt_pmcmc(R0_change = out$interventions$R0_change,
                                 R0 = out$replicate_parameters$R0[1],
                                 date_R0_change = out$interventions$date_R0_change,
                                 pars = list(),
                                 Rt_args = out$pmcmc_results$inputs$Rt_args)

  expect_equal(out$replicate_parameters$R0[1]*out$interventions$R0_change,
               from_eval)

  # and check it by interventions_dates_for_odin
  tt <- intervention_dates_for_odin(dates = out$interventions$date_R0_change,
                                    change = out$interventions$R0_change,
                                    start_date = out$replicate_parameters$start_date[1],
                                    steps_per_day = 10)

  from_int_eval <- evaluate_Rt_pmcmc(R0_change = tt$change,
                                     R0 = out$replicate_parameters$R0[1],
                                     date_R0_change = tt$dates,
                                     pars = list(),
                                     Rt_args = out$pmcmc_results$inputs$Rt_args
  )

  expect_equal(c(rep(out$replicate_parameters$R0[1],
                     length(seq.Date(out$replicate_parameters$start_date[1], out$interventions$date_R0_change[1]-1, 1))),
                 out$replicate_parameters$R0[1]*out$interventions$R0_change),
               from_int_eval)

})


#-------------------------------------
test_that("offsetting", {


  expect_equal(start_date_to_offset(as.Date("2020-04-01"), as.Date("2020-03-01")), 31)
  expect_equal(start_date_to_offset(("2020-04-01"), ("2020-03-01")), 31)
  expect_equal(offset_to_start_date(("2020-04-01"), -31), as.Date("2020-03-01"))

})

#-------------------------------------
test_that("evaluate_Rt", {

  R0 <- 3
  R0_change <- c(seq(0.9,0.5,-0.1),0.7)
  date_R0_change <- c("2020-03-12","2020-03-18","2020-03-22",
                      "2020-03-25","2020-03-27","2020-03-29")
  date_Meff_change <- c("2020-03-26")

  Meff <- 2
  Meff_pl <- 6

  Rt_base <- evaluate_Rt_pmcmc(R0_change = R0_change,
                               R0 = R0,
                               date_R0_change = date_R0_change,
                               pars = list(Meff = Meff,
                                           Meff_pl = Meff_pl),
                               Rt_args = list(plateau_duration = 1,
                                              date_Meff_change = date_Meff_change))

  expect_lt(Rt_base[6], Rt_base[4])

  Rt <- evaluate_Rt_pmcmc(R0_change = NULL,
                          R0 = R0,
                          date_R0_change = date_R0_change,
                          pars = list(Meff = Meff,
                                      Meff_pl = Meff_pl),
                          Rt_args = list(plateau_duration=1,
                                         date_Meff_change = date_Meff_change))
  expect_equal(R0, Rt[1])

  Rt <- evaluate_Rt_pmcmc(R0_change = R0_change,
                          R0 = R0,
                          date_R0_change = date_R0_change,
                          pars = list(Meff = Meff,
                                      Meff_pl = Meff_pl),
                          Rt_args = list(plateau_duration=1,
                                         date_Meff_change = "2020-03-01"))
  expect_lt(mean(Rt_base), mean(Rt))

  # with scale
  Rt <- evaluate_Rt_pmcmc(R0_change = R0_change, R0 = R0,
                          pars = list(Meff = Meff,
                                      Meff_pl = Meff),
                          Rt_args = list(plateau_duration=1,
                                         date_Meff_change = "2020-03-22",
                                         scale_meff_pl = TRUE),
                          date_R0_change = date_R0_change)
  expect_equal(Rt[3], Rt[6])

  # with no shift
  Rt <- evaluate_Rt_pmcmc(R0_change = R0_change, R0 = R0,
                          pars = list(Meff = Meff,
                                      Meff_pl = Meff),
                          Rt_args = list(plateau_duration=1,
                                         date_Meff_change = "2020-03-22",
                                         scale_meff_pl = TRUE),
                          date_R0_change = date_R0_change)


  # with early shift
  expect_error(Rt <- evaluate_Rt_pmcmc(R0_change = R0_change, R0 = R0,
                                       pars = list(Meff = Meff,
                                                   Meff_pl = Meff,
                                                   Rt_shift = 0.1,
                                                   Rt_shift_scale = 1),
                                       Rt_args = list(plateau_duration=1,
                                                      date_Meff_change = "2020-03-01",
                                                      scale_meff_pl = TRUE),
                                       date_R0_change = date_R0_change),
               "Rt_shift provided but no Rt_shift_duration")

  Rt <- evaluate_Rt_pmcmc(R0_change = R0_change, R0 = R0,
                          pars = list(Meff = Meff,
                                      Meff_pl = Meff,
                                      Rt_shift = 0.1,
                                      Rt_shift_scale = 1),
                          Rt_args = list(plateau_duration=1,
                                         date_Meff_change = "2020-03-01",
                                         scale_meff_pl = TRUE,
                                         Rt_shift_duration = 3),
                          date_R0_change = date_R0_change)

  expect_lt(Rt[2], Rt[1])

  # with Rw params
  Rt <- evaluate_Rt_pmcmc(R0_change = R0_change, R0 = R0,
                          pars = list(Meff = Meff,
                                      Meff_pl = Meff,
                                      Rt_shift = 0.1,
                                      Rt_shift_scale = 1,
                                      Rt_rw_1 = 1,
                                      Rt_rw_2 = 2),
                          Rt_args = list(plateau_duration = 3,
                                         date_Meff_change = "2020-03-18",
                                         scale_meff_pl = TRUE,
                                         Rt_shift_duration = 5,
                                         Rt_rw_duration = 3),
                          date_R0_change = date_R0_change)

  expect_lt(Rt[3], Rt[2])
  expect_lt(Rt[4], Rt[3])
  expect_lt(Rt[5], Rt[4])
  expect_equal(Rt[5], Rt[6])
})


#------------------------------------------------
test_that("pmcmc deaths from treatment", {

  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)

  steps_per_day = 4

  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2


  out <- pmcmc(data = data,
               n_mcmc = 50,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = deterministic_model(),
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               baseline_hosp_bed_capacity = 1,
               baseline_ICU_bed_capacity = 1,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               Rt_args = list(date_Meff_change = NULL),
               treated_deaths_only = FALSE,
               country = country)

  out2 <- pmcmc(data = data,
                n_mcmc = 50,
                log_likelihood = NULL,
                log_prior = NULL,
                n_particles = 2,
                steps_per_day = steps_per_day,
                output_proposals = FALSE,
                n_chains = 1,
                replicates = 2,
                burnin = 0,
                squire_model = deterministic_model(),
                pars_init = pars_init,
                pars_min = pars_min,
                pars_max = pars_max,
                pars_discrete = pars_discrete,
                pars_obs = pars_obs,
                baseline_hosp_bed_capacity = 1,
                baseline_ICU_bed_capacity = 1,
                proposal_kernel = proposal_kernel,
                R0_change = R0_change,
                date_R0_change = date_R0_change,
                Rt_args = list(date_Meff_change = NULL),
                treated_deaths_only = TRUE,
                country = country)


  expect_lt(sum(format_output(out, "deaths")$y, na.rm = TRUE),
            sum(format_output(out2, "deaths")$y, na.rm = TRUE))

  expect_warning(expect_s3_class(plot(out, particle_fit = TRUE), "gg"))
  expect_warning(expect_s3_class(plot(out2, particle_fit = TRUE), "gg"))

  set.seed(91L)

  out <- pmcmc(data = data,
               n_mcmc = 50,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 2,
               burnin = 0,
               squire_model = explicit_model(),
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               baseline_hosp_bed_capacity = 1,
               baseline_ICU_bed_capacity = 1,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               Rt_args = list(date_Meff_change = NULL),
               treated_deaths_only = FALSE,
               country = country)

  out2 <- pmcmc(data = data,
                n_mcmc = 50,
                log_likelihood = NULL,
                log_prior = NULL,
                n_particles = 2,
                steps_per_day = steps_per_day,
                output_proposals = FALSE,
                n_chains = 1,
                replicates = 2,
                burnin = 0,
                squire_model = explicit_model(),
                pars_init = pars_init,
                pars_min = pars_min,
                pars_max = pars_max,
                pars_discrete = pars_discrete,
                pars_obs = pars_obs,
                baseline_hosp_bed_capacity = 1,
                baseline_ICU_bed_capacity = 1,
                proposal_kernel = proposal_kernel,
                R0_change = R0_change,
                date_R0_change = date_R0_change,
                Rt_args = list(date_Meff_change = NULL),
                treated_deaths_only = TRUE,
                country = country)

  expect_lt(sum(format_output(out, "deaths")$y, na.rm = TRUE),
            sum(format_output(out2, "deaths")$y, na.rm = TRUE))

  expect_warning(expect_s3_class(plot(out, particle_fit = TRUE), "gg"))
  expect_warning(expect_s3_class(plot(out2, particle_fit = TRUE), "gg"))



})
#

#------------------------------------------------
test_that("pmcmc with walker params", {

  Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")
  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  pars_init = list('start_date'     = as.Date("2020-02-07"),
                   'R0'             = 2.5,
                   'Meff'           = 2)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE)
  pars_obs = list(phi_cases = 0.1,
                  k_cases = 2,
                  phi_death = 1,
                  k_death = 2,
                  exp_noise = 1e6)

  steps_per_day = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2
  # proposal kernel covriance
  proposal_kernel <- matrix(0.5, ncol=length(pars_init), nrow = length(pars_init))
  diag(proposal_kernel) <- 1
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)


  Sys.setenv("SQUIRE_PARALLEL_DEBUG"=TRUE)
  out <- pmcmc(data = data,
               n_mcmc = 200,
               log_likelihood = NULL,
               log_prior = NULL,
               n_particles = 2,
               steps_per_day = steps_per_day,
               output_proposals = FALSE,
               n_chains = 1,
               replicates = 20,
               burnin = 0,
               squire_model = squire_model,
               pars_init = pars_init,
               pars_min = pars_min,
               pars_max = pars_max,
               pars_discrete = pars_discrete,
               pars_obs = pars_obs,
               proposal_kernel = proposal_kernel,
               R0_change = R0_change,
               date_R0_change = date_R0_change,
               country = country)


  out2 <- pmcmc(data = data,
                n_mcmc = 200,
                log_likelihood = NULL,
                log_prior = NULL,
                n_particles = 2,
                steps_per_day = steps_per_day,
                output_proposals = FALSE,
                n_chains = 1,
                replicates = 20,
                burnin = 0,
                squire_model = squire_model,
                pars_init = pars_init,
                pars_min = pars_min,
                pars_max = pars_max,
                pars_discrete = pars_discrete,
                pars_obs = pars_obs,
                proposal_kernel = proposal_kernel,
                R0_change = R0_change,
                date_R0_change = date_R0_change,
                walker_params = TRUE,
                country = country)


})
