context("particle")

test_that("particle works", {

  data <- read.csv(squire_file("extdata/example.csv"))

  model_start_date <- "2020-02-01"
  dt <- 1

  data <- data[order(data$date), ]
  data <- data[data$deaths>0 | data$cases>0,]

  model_params <- parameters_explicit_SEEIR(country = "Algeria",
                                            seeding_cases = 5,
                                            dt=dt)

  index <- odin_index(explicit_SEIR(user = model_params,
                                    unused_user_action = "ignore"))

  set.seed(123)
  out <- run_particle_filter(data = data,
                             model_params = model_params,
                             squire_model = explicit_model(),
                             model_start_date = model_start_date,
                             obs_params = list(phi_cases = 0.1,
                                               k_cases = 2,
                                               phi_death = 1,
                                               k_death = 2,
                                               exp_noise = 1e6),
                             n_particles = 5,
                             forecast_days = 0,
                             save_particles = FALSE,
                             return = "full")

  expect_true(names(out) == "log_likelihood")
  expect_lt(out$log_likelihood , 0)


  expect_error(out <- run_particle_filter(data = data,
                                          model_params = model_params,
                                          squire_model = explicit_model(),
                                          model_start_date = model_start_date,
                                          obs_params = list(phi_cases = 0.1,
                                                            k_cases = 2,
                                                            phi_death = 1,
                                                            k_death = 2,
                                                            exp_noise = 1e6),
                                          n_particles = 5,
                                          forecast_days = 14,
                                          save_particles = FALSE,
                                          return = "full"),
               "only possible if particles are saved")

  set.seed(123)
  out <- run_particle_filter(data = data,
                             model_params = model_params,
                             squire_model = explicit_model(),
                             model_start_date = model_start_date,
                             obs_params = list(phi_cases = 0.1,
                                               k_cases = 2,
                                               phi_death = 1,
                                               k_death = 2,
                                               exp_noise = 1e6),
                             n_particles = 5,
                             forecast_days = 0,
                             save_particles = TRUE,
                             return = "full")

  expect_true(all(names(out) == c("log_likelihood","states")))
  expect_lt(out$log_likelihood , 0)

  index <- c(index$D) - 1L
  particles <- apply(out$states[, index, ], c(1, 3), sum)
  pdf(file = NULL)
  plot_particles(particles, ylab = "D")
  points(as.Date(data$date), cumsum(data$deaths), pch = 19)
  dev.off()

})


test_that("particle_filter error cases", {
  set.seed(1)

  time_steps_per_day <- 4
  data <- read.csv(squire_file("extdata/example.csv"),
                   stringsAsFactors = FALSE)
  d <- particle_filter_data(data, "2020-02-02", time_steps_per_day)
  squire_model <- explicit_model()

  pars_model <- parameters_explicit_SEEIR("Angola")
  pars_obs <- list(phi_general = 0.95,
                   k_general = 2,
                   phi_ICU = 0.95,
                   k_ICU = 2,
                   phi_death = 926 / 1019,
                   k_death = 2,
                   exp_noise = 1e6)

  expect_warning(mod <- squire_model$odin_model(user = pars_model))
  compare <- squire_model$compare_model(mod, pars_obs, d)

  expect_error(
    particle_filter(NULL, mod, compare, 100),
    "Expected a data set derived from particle_filter_data")
  expect_error(
    particle_filter(data, mod, compare, 100),
    "Expected a data set derived from particle_filter_data")
  expect_error(
    particle_filter(d, NULL, compare, 100),
    "Expected 'model' to be an 'odin_model' object")
  expect_error(
    particle_filter(d, mod, compare, 1),
    "At least two particles required")
  expect_error(
    particle_filter(d, mod, compare, 100, forecast_days = 1),
    "forecasting only possible if particles are saved")
  expect_error(
    particle_filter(d, mod, compare, 100, forecast_days = -1),
    "forecast_days must be positive")
})
