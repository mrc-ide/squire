context("calibration")

#------------------------------------------------
test_that("calibrate works", {
  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 2)
  expect_type(t1, "list")
  expect_s3_class(t1, "squire_simulation")
  expect_true(dim(t1$output)[3] == 2)
  expect_error(calibrate(country = "Angola", deaths = 5,
                         reporting_fraction = 0.5,
                         time_period = 120, seeding_age_groups = "wrong"),
               "inputted age groups not valid")
  expect_error(calibrate("Angola", -1, 0.5))
  expect_error(calibrate("Angola", 5, -0.1))
  expect_error(calibrate("Angola", 5, 1.1))
})


#------------------------------------------------
test_that("calibrate R0 works", {
  set.seed(123)
  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3,4))
  expect_true(all(c(3, 4) %in% t1$parameters$R0_scan))

  set.seed(123)
  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3))
  expect_true(all(t1$parameters$R0_scan == 3))

  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3))
  expect_true(all(t1$parameters$R0_scan == 3))

  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3,4,5),
                  R0=c(3,2,1), tt_R0=c(0,30,60))
  expect_true(all(t1$parameters$R0_scan %in% 3:5))



})


#------------------------------------------------
test_that("calibrate particle works", {

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
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 10

  out <- calibrate_particle(
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
