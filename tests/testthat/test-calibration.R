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
test_that("calibrate deterministic", {

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

out <- calibrate(
  data = data,
  R0_min = R0_min,
  R0_max = R0_max,
  R0_step = R0_step,
  first_start_date = first_start_date,
  last_start_date = last_start_date,
  day_step = day_step,
  squire_model = deterministic_model(),
  pars_obs = pars_obs,
  n_particles = 2,
  reporting_fraction = reporting_fraction,
  R0_change = R0_change,
  date_R0_change = date_R0_change,
  replicates = replicates,
  country = country,
  forecast = 0
)

expect_true(inherits(out$scan_results$inputs$squire_model, "deterministic"))
expect_error(get <- projections(out), "unlikely to work with deterministic")

})

test_that("calibrate user pop and contact", {

  pop <- get_population("Nigeria")
  mat <- get_mixing_matrix("Nigeria")*0.9

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

#------------------------------------------------
test_that("calibrate 3d particle works", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.7
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  Meff_min = 0.1
  Meff_max = 1.8
  Meff_step = 0.8
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  Sys.setenv("SQUIRE_PARALLEL_DEBUG"=FALSE)
  set.seed(93L)
  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    Meff_min = Meff_min,
    Meff_max = Meff_max,
    Meff_step = Meff_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    Rt_func = function(R0_change, R0, Meff) {R0_change*Meff*R0},
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

  expect_s3_class(plot(out$scan_results), "gg")
  expect_s3_class(plot(out$scan_results, what = "probability"), "gg")
  expect_s3_class(plot(out$scan_results, what = "probability", show = c(1,3)), "gg")
  expect_s3_class(plot(out$scan_results, what = "probability", show = c(2,3)), "gg")
  expect_s3_class(plot(out$scan_results, show = c(2,3)), "gg")
  expect_true(which.max(apply(out$scan_results$mat_log_ll,3,sum))==2)

})


#------------------------------------------------
test_that("calibrate 3d non future works", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.7
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  Meff_min = 0.5
  Meff_max = 1
  Meff_step = 0.5
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  set.seed(93L)
  Sys.setenv("SQUIRE_PARALLEL_DEBUG"=TRUE)
  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    Meff_min = Meff_min,
    Meff_max = Meff_max,
    Meff_step = Meff_step,
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
  expect_is(out,"squire_simulation")
  Sys.setenv("SQUIRE_PARALLEL_DEBUG"=FALSE)

})

#------------------------------------------------
test_that("R0 and Meff arge checking", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.7
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  Meff_min = 0.5
  Meff_max = 1
  Meff_step = 0.5
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  set.seed(93L)
  expect_error(out <- calibrate(
    data = data,
    R0_min = 2,
    R0_max = 1.5,
    R0_step = R0_step,
    Meff_min = Meff_min,
    Meff_max = Meff_max,
    Meff_step = Meff_step,
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
  ), "must be greater")

  expect_error(out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    Meff_min = 2,
    Meff_max = 1,
    Meff_step = Meff_step,
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
  ), "must be greater")


})


#------------------------------------------------
test_that("reporting fraction into pars_obs", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
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
  pars_obs <-  list(phi_cases = 1,
                    k_cases = 2,
                    phi_death = 1,
                    k_death = 2,
                    exp_noise = 1e6)
  n_particles = 5

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
    reporting_fraction = 0.1,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

  out2 <- calibrate(
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
    reporting_fraction = 1,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

  index <- odin_index(out$model)
  expect_true(sum(rowSums(out$output[,index$D,1]))  > sum(rowSums(out2$output[,index$D,1])))

})


#------------------------------------------------
test_that("date_changes before last start date", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  data <- data[which(data$deaths>0)[1]:nrow(data),]
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.6
  R0_step = 0.1
  first_start_date = "2020-03-04"
  last_start_date = "2020-03-05"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_R0_change[1] <- date_R0_change[1] - 8
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 5

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
    reporting_fraction = 0.1,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

  date_R0_change[2] <- "2020-03-05"
  out2 <- calibrate(
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
    reporting_fraction = 0.1,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

})


#------------------------------------------------
test_that("R0_prior", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  data <- data[which(data$deaths>0)[1]:nrow(data),]
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  country = "Algeria"
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  replicates = 2
  R0_min = 1.5
  R0_max = 4.5
  R0_step = 1
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 5

  set.seed(93L)
  out <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    R0_prior = list("func" = dnorm, "args"=list("mean"=2.5,"sd"=0.5,"log"=TRUE)),
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

  out2 <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    R0_prior = list("func" = dnorm, "args"=list("mean"=0.1,"sd"=0.01,"log"=TRUE)),
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

  out3 <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    Meff_min = 0.9,
    Meff_max = 1,
    Meff_step = 0.1,
    R0_prior = list("func" = dnorm, "args"=list("mean"=0.1,"sd"=0.01,"log"=TRUE)),
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 0
  )

  get <- lapply(list(out,out2, out3), format_output, "deaths")

  expect_true(max(get[[1]]$y,na.rm=TRUE) > max(get[[2]]$y,na.rm=TRUE))
  expect_true(max(get[[1]]$y,na.rm=TRUE) > max(get[[3]]$y,na.rm=TRUE))

})

#------------------------------------------------
test_that("Rt_func", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.7
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  Meff_min = 0.6
  Meff_max = 0.7
  Meff_step = 0.05
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
    Meff_min = 1.5,
    Meff_max = 2.5,
    Meff_step = 0.5,
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

  out2 <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    Meff_min = 1.5,
    Meff_max = 2.5,
    Meff_step = 0.5,
    Rt_func = function(R0_change, R0, Meff){R0_change*Meff*R0},
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

  out3 <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    Meff_min = Meff_min,
    Meff_max = Meff_max,
    Meff_step = Meff_step,
    Rt_func = function(R0_change, R0, Meff){R0_change*Meff*R0},
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

  expect_gt(mean(out$scan_results$mat_log_ll),mean(out2$scan_results$mat_log_ll))
  expect_gt(mean(out3$scan_results$mat_log_ll),mean(out2$scan_results$mat_log_ll))

})

