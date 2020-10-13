context("projections")

#------------------------------------------------
test_that("projection works", {


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
  n_particles = 10
  set.seed(123)

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
    forecast = 40
  )

  # check that it runs
  out <- projections(out, R0 = 2)
  p <- trigger_projections(out, trigger_metric = "deaths", trigger_value = 25)
  p2 <- trigger_projections(out, trigger_metric = "ICU_occupancy", trigger_value = 250)

  # check that it can be plotted (good test it plays nice with plotting)
  expect_warning(expect_s3_class(projection_plotting(list(out, p, p2),
                    scenarios = c("no trigger", "25 deaths", "250 ICU capacity"),
                    add_parms_to_scenarios = FALSE,
                    var_select =  "infections",
                    ci = FALSE), "gg"), "<10 replicates")

  # and check results make sense
  infs <- lapply(list(out, p, p2), format_output, "infections")
  expect_lt(sum(infs[[2]]$y, na.rm = TRUE), sum(infs[[1]]$y, na.rm = TRUE))
  expect_lt(sum(infs[[3]]$y, na.rm = TRUE), sum(infs[[2]]$y, na.rm = TRUE))

})
