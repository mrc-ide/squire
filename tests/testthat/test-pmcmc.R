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
                   'Meff'           = 2,
                   'Meff_pl'        = 1)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 0.1,
                  'Meff_pl'         = 1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5,
                  'Meff_pl'         = 1)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE,
                       'Meff_pl'    = FALSE)
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
  expect_warning(out <- pmcmc(data = data,
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
               date_Meff_change = NULL,
               country = country),
               "recommend starting to adapt scaling factor at least 100")

  expect_named(out, c("output", "parameters", "model", "inputs", "replicate_parameters", "pmcmc_results", "interventions"))
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
                                 'Meff'            = 1,
                                 'Meff_pl'         = 5),
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country)
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
                 pars_min = list('start_date'      = as.Date("2020-05-20"),
                                 'R0'              = 1,
                                 'Meff'            = 1,
                                 'Meff_pl'         = 1),
                 pars_max = list('start_date'      = as.Date("2020-05-20"),
                                 'R0'              = 5,
                                 'Meff'            = 5,
                                 'Meff_pl'         = 1),
                 pars_discrete = pars_discrete,
                 pars_obs = pars_obs,
                 proposal_kernel = proposal_kernel,
                 R0_change = R0_change,
                 date_R0_change = date_R0_change,
                 country = country)
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
                 country = country)
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
                 date_R0_change = "2020-02-01",
                 country = country)
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
                 date_contact_matrix_set_change = "2020-05-10")
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
                 date_contact_matrix_set_change = date_R0_change[1])
  )

  expect_warning(out <- pmcmc(data = data,
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
               date_Meff_change = NULL,
               country = country,
               baseline_contact_matrix = contact_matrices[[1]],
               contact_matrix_set = list(contact_matrices[[1]]),
               date_contact_matrix_set_change = date_R0_change[1]),
               "recommend starting to adapt scaling factor at least 100")



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
                 date_contact_matrix_set_change = "2020-02-01")
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
                 country = country)
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
                 country = country)
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
                 country = country)
  )

  expect_warning(out <- pmcmc(data = data,
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
               date_Meff_change = NULL,
               country = country),
               "recommend starting to adapt scaling factor at least 100")


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

  expect_warning(out <- pmcmc(data = data,
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
  ),
  "recommend starting to adapt scaling factor at least 100")

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
                   'Meff'           = 2,
                   'Meff_pl'         = 1)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 1,
                  'Meff_pl'         = 1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 3,
                  'Meff_pl'         = 1)
  pars_discrete = list('start_date' = TRUE,
                       'R0'         = FALSE,
                       'Meff'       = FALSE,
                       'Meff_pl'    = FALSE)
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
  expect_warning(out <- pmcmc(data = data,
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
               country = country),
               "recommend starting to adapt scaling factor at least 100")


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
                   'Meff'           = 2,
                   'Meff_pl'         = 1)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 0,
                  'Meff'            = 1,
                  'Meff_pl'         = 1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5,
                  'Meff_pl'         = 1)
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

  steps_per_day = 4

  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  n_particles = 2

  expect_warning(out <- pmcmc(data = data,
               n_mcmc = 5,
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
               date_Meff_change = NULL,
               country = country),
               "recommend starting to adapt scaling factor at least 100")


  expect_true(inherits(out$pmcmc_results$inputs$squire_model, "deterministic"))
  expect_error(get <- projections(out), "unlikely to work with deterministic")

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
                   'Meff'           = 1,
                   'Meff_pl'        = 1)
  pars_min = list('start_date'      = as.Date("2020-02-01"),
                  'R0'              = 1e-10,
                  'Meff'            = 1,
                  'Meff_pl'         = 1)
  pars_max = list('start_date'      = as.Date("2020-02-20"),
                  'R0'              = 5,
                  'Meff'            = 5,
                  'Meff_pl'         = 1)
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

  expect_warning(out <- pmcmc(data = data,
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
               baseline_contact_matrix = mat),
               "recommend starting to adapt scaling factor at least 100")

  expect_true(
    identical(out$pmcmc_results$inputs$model_params$contact_matrix_set[[1]], mat)
  )

})

