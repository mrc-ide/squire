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
                             return = "ll")
  expect_is(out, "numeric")

  expect_message(out <- run_particle_filter(data = data,
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
                             return = "sample"))
  expect_is(out, "matrix")

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
                             return = "single")
  expect_named(out, c("log_likelihood", "sample_state"))

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
                                            forecast_days = 0,
                                            save_particles = TRUE,
                                            return = "single"))



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
  expect_error(
    run_particle_filter(data = data,
                        model_params = parameters_simple_SEEIR(
                          population = get_population("Algeria",simple_SEIR = TRUE)$n,
                          contact_matrix_set = contact_matrices[[1]]),
                        squire_model = simple_model(),
                        model_start_date = "2020-02-10",
                        obs_params = list(phi_cases = 0.1,
                                          k_cases = 2,
                                          phi_death = 1,
                                          k_death = 2,
                                          exp_noise = 1e6),
                        n_particles = 5,
                        forecast_days = 0,
                        save_particles = TRUE,
                        return = "full"),
    "compare function does not work with simple")

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
  expect_error(
    particle_filter(d, mod, compare, 100, forecast_days = 1, save_particles = TRUE, save_end_states = TRUE),
    "Can not have both save_particles")
})

test_that("run_particle_filter error cases", {

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
  expect_error(run_particle_filter(data = data,
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
                                   return = "jingle_bells"))

  expect_error(run_particle_filter(data = data,
                                   model_params = model_params,
                                   squire_model = explicit_model(),
                                   model_start_date = "2990-02-01",
                                   obs_params = list(phi_cases = 0.1,
                                                     k_cases = 2,
                                                     phi_death = 1,
                                                     k_death = 2,
                                                     exp_noise = 1e6),
                                   n_particles = 5,
                                   forecast_days = 0,
                                   save_particles = FALSE,
                                   return = "ll"))

  expect_error(run_(data.frame("datedsasd"="2020-04-10","ads"=0.05)))
  expect_error(particle_filter_data("go_away_covid"))
  expect_error(particle_filter_data(data.frame("date"=c("2020-03-10","2020-03-09"))))
  expect_error(particle_filter_data(data.frame("date"=c("2020-03-10","2020-03-11")),start_date = "2020-03-13"))

})


test_that("particle_filter_data error cases", {

  expect_error(particle_filter_data(data.frame("datedsasd"="2020-04-10","ads"=0.05)))
  expect_error(particle_filter_data("go_away_covid"))
  expect_error(particle_filter_data(data.frame("date"=c("2020-03-10","2020-03-09"))))
  expect_error(particle_filter_data(data.frame("date"=c("2020-03-10","2020-03-11")),start_date = "2020-03-13"))

})

test_that("interventions_unique error cases", {

  get <- interventions_unique(data.frame())
  expect_is(get,"list")
  expect_null(get$dates_change)

  expect_error(interventions_unique(data.frame("datedsasd"="2020-04-10","ads"=0.05)))
  expect_error(interventions_unique(data.frame("date"="2020-04-10","C"=0.05),x = "F"))

  expect_error(intervention_dates_for_odin("2020-02-10", "2020-02-21", 4))
  expect_error(intervention_dates_for_odin(c("2020-03-10","2020-03-09"), "2020-02-21", 4))
})

test_that("run_deterministic_comparison error cases", {

data <- read.csv(squire_file("extdata/example.csv"))
model_start_date <- "2020-02-01"
dt <- 1

data <- data[order(data$date), ]
data <- data[data$deaths>0 | data$cases>0,]

mod <- deterministic_model()

model_params <- mod$parameter_func(country = "Algeria",
                                          seeding_cases = 5,
                                          dt=dt)

index <- odin_index(explicit_SEIR_deterministic(user = model_params,
                                  unused_user_action = "ignore"))

set.seed(123)
expect_error(out <- run_particle_filter(data = data,
                           model_params = model_params,
                           squire_model = mod,
                           model_start_date = model_start_date,
                           obs_params = list(phi_cases = 0.1,
                                             k_cases = 2,
                                             phi_death = 1,
                                             k_death = 2,
                                             exp_noise = 1e6),
                           n_particles = 5,
                           forecast_days = 0,
                           save_particles = FALSE,
                           return = "twaddle"),
             "return argument must be full, ll, sample or single")

expect_error(out <- run_particle_filter(data = data,
                                        model_params = model_params,
                                        squire_model = mod,
                                        model_start_date = max(as.Date(as.character(data$date)))+1,
                                        obs_params = list(phi_cases = 0.1,
                                                          k_cases = 2,
                                                          phi_death = 1,
                                                          k_death = 2,
                                                          exp_noise = 1e6),
                                        n_particles = 5,
                                        forecast_days = 0,
                                        save_particles = FALSE,
                                        return = "ll"),
             "Model start date is later than data start date")

})
