context("calibration")

#------------------------------------------------
test_that("calibrate particle works", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.6
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  set.seed(93L)
  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )
  expect_named(out, c("output","parameters","model","interventions","scan_results","replicate_parameters"))
  expect_s3_class(plot(out$scan_results, log = TRUE), "gg")
  expect_s3_class(plot(out$scan_results), "gg")
  expect_s3_class(plot(out$scan_results, what = "probability", log = FALSE), "gg")
  expect_s3_class(plot(out$scan_results, what = "probability", log = TRUE), "gg")
  expect_warning(expect_s3_class(plot(out, what = "cases", particle_fit = TRUE), "gg"))
  expect_warning(expect_s3_class(plot(out, what = "deaths", particle_fit = TRUE), "gg"))
  expect_error(plot(out, what = "rubbish", particle_fit = TRUE),"must be one of")


  # DATE CHECKS DATE_R0
  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = "2020-03-01",
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = last_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = 0.5,
    date_R0_change = "2020-05-10",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = 0.5,
    date_R0_change = "2020-02-01",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  # DATE CHECKS DATE_CONTACT
  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    baseline_contact_matrix = contact_matrices[[1]],
    reporting_fraction = reporting_fraction,
    contact_matrix_set = contact_matrices[1],
    date_contact_matrix_set_change = "2020-05-10",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    contact_matrix_set = list(contact_matrices[[1]]),
    date_contact_matrix_set_change = date_R0_change[1],
    replicates = replicates,
    country = country,
    forecast = 0
  ), "baseline_contact_matrix can")

  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_contact_matrix = contact_matrices[[1]],
    contact_matrix_set = list(contact_matrices[[1]]),
    date_contact_matrix_set_change = date_R0_change[1],
    replicates = replicates,
    country = country,
    forecast = 0
  )

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    baseline_contact_matrix = contact_matrices[[1]],
    reporting_fraction = reporting_fraction,
    contact_matrix_set = contact_matrices[1],
    date_contact_matrix_set_change = "2020-02-01",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  # DATE CHECKS date_hosp
  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_hosp_bed_capacity = 10,
    hosp_bed_capacity = 100,
    date_hosp_bed_capacity_change = "2020-05-10",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_hosp_bed_capacity = 10,
    hosp_bed_capacity = 100,
    date_hosp_bed_capacity_change = "2020-02-01",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    hosp_bed_capacity = 10,
    date_hosp_bed_capacity_change = date_R0_change[1],
    replicates = replicates,
    country = country,
    forecast = 0
  ), "baseline_hosp_bed_capacity can")

  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_hosp_bed_capacity = 5,
    hosp_bed_capacity = 10,
    date_hosp_bed_capacity_change = date_R0_change[1],
    replicates = replicates,
    country = country,
    forecast = 0
  )



  # DATE CHECKS DATE_ICU
  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_ICU_bed_capacity = 10,
    ICU_bed_capacity = 100,
    date_ICU_bed_capacity_change = "2020-05-10",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_ICU_bed_capacity = 10,
    ICU_bed_capacity = 100,
    date_ICU_bed_capacity_change = "2020-02-01",
    replicates = replicates,
    country = country,
    forecast = 0
  ))

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    ICU_bed_capacity = 10,
    date_ICU_bed_capacity_change = date_R0_change[1],
    replicates = replicates,
    country = country,
    forecast = 0
  ), "baseline_ICU_bed_capacity can")

  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_ICU_bed_capacity = 5,
    ICU_bed_capacity = 10,
    date_ICU_bed_capacity_change = date_R0_change[1],
    replicates = replicates,
    country = country,
    forecast = 0
  )

})


#------------------------------------------------
test_that("calibrate non future works", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 0.00001
  R0_max = 0.00001
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  Sys.setenv("SQUIRE_PARALLEL_DEBUG"=TRUE)
  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

})


#------------------------------------------------
test_that("calibrate user pop and contact", {


  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.6
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  pop <- get_population("Nigeria")
  mat <- get_mixing_matrix("Nigeria")*0.9

  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    baseline_contact_matrix = mat,
    replicates = replicates,
    population = pop$n,
    forecast = 0
  )

  expect_true(
    identical(out$scan_results$inputs$model_params$contact_matrix_set[[1]], mat)
  )

})

#------------------------------------------------
test_that("calibrate dt not playing with day stepping well", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.6
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    replicates = replicates,
    country = "Algeria",
    forecast = 0,
    dt = 0.61
  ), "must result in an integer")

})



