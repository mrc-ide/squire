context("estimation")

test_that("scan_R0_date works", {

  set.seed(123)
  data <- read.csv(squire_file("extdata/example.csv"))
  model_start_date <- "2020-02-05"

  it <- read.csv(squire_file("extdata/example_intervention.csv"))
  it$date <- as.Date(it$date)
  it$tt_R0 <- abs(as.numeric(tail(it$date,1) - it$date) - as.numeric(tail(it$date,1) - as.Date(model_start_date)))

  date_R0_change <- head(it[cumsum(rle(it$C)$lengths)+1,]$date, -1)
  tt_R0 <- seq_along(date_R0_change) - 1L
  R0_change <- head(it[cumsum(rle(it$C)$lengths)+1,]$C, -1)


  # Parameters for run
  R0_min <- 3
  R0_max <- 4
  R0_step <- 1
  first_start_date <- "2020-02-01"
  last_start_date <- "2020-02-02"
  day_step <- 1

  scan_results <- scan_R0_date(R0_min = R0_min,
                               R0_max = R0_max,
                               R0_step = R0_step,
                               first_start_date = first_start_date,
                               last_start_date = last_start_date,
                               day_step = day_step,
                               data = data[1:10,],
                               model_params = parameters_explicit_SEEIR(country = "Algeria"),
                               R0_change = R0_change,
                               date_R0_change = date_R0_change,
                               squire_model = explicit_model(),
                               n_particles = 2)

  expect_is(scan_results, "squire_scan")
  expect_true("inputs" %in% names(scan_results))
  expect_setequal(names(scan_results$inputs),
                  c("model", "model_params", "interventions", "pars_obs", "data"))

  R0_grid <- seq(R0_min, R0_max, R0_step)
  date_grid <- seq(as.Date(first_start_date), as.Date(last_start_date), day_step)
  expect_equal(scan_results$x, R0_grid)
  expect_equal(scan_results$y, date_grid)
  expect_equal(dim(scan_results$renorm_mat_LL), dim(scan_results$mat_log_ll))
  expect_equal(dim(scan_results$renorm_mat_LL), c(length(R0_grid), length(date_grid)))
  expect_true(all(scan_results$renorm_mat_LL <= 1 & scan_results$renorm_mat_LL >= 0))

  # Plots run, but not checked
  pdf(file = NULL)
  plot(scan_results)
  dev.off()


})



test_that("Transmission is more likely", {

  set.seed(1)
  data <- read.csv(squire_file("extdata/example.csv"))
  model_start_date <- "2020-02-05"

  it <- read.csv(squire_file("extdata/example_intervention.csv"))
  it$date <- as.Date(it$date)
  it$tt_R0 <- abs(as.numeric(tail(it$date,1) - it$date) - as.numeric(tail(it$date,1) - as.Date(model_start_date)))

  date_R0_change <- head(it[cumsum(rle(it$C)$lengths)+1,]$date, -1)
  tt_R0 <- seq_along(date_R0_change) - 1L
  R0_change <- head(it[cumsum(rle(it$C)$lengths)+1,]$C, -1)


  # Parameters for run
  R0_min <- 0.0001
  R0_max <- 3
  R0_step <- 2.5
  first_start_date <- "2020-01-21"
  last_start_date <- "2020-01-21"
  day_step <- 1

  scan_results <- scan_R0_date(R0_min = R0_min,
                               R0_max = R0_max,
                               R0_step = R0_step,
                               first_start_date = first_start_date,
                               last_start_date = last_start_date,
                               day_step = day_step,
                               data = data[1:10,],
                               model_params = parameters_explicit_SEEIR(country = "Algeria"),
                               R0_change = R0_change,
                               date_R0_change = date_R0_change,
                               squire_model = explicit_model(),
                               n_particles = 2)

  # No transmission b = 0 much less likely than some transmission b = 0.1
  expect_lt(scan_results$mat_log_ll[[1]], scan_results$mat_log_ll[[2]])

})

test_that("Unreasonable start dates are less likely", {
  set.seed(1)

  data <- read.csv(squire_file("extdata/example.csv"))
  model_start_date <- "2020-02-05"

  it <- read.csv(squire_file("extdata/example_intervention.csv"))
  it$date <- as.Date(it$date)
  it$tt_R0 <- abs(as.numeric(tail(it$date,1) - it$date) - as.numeric(tail(it$date,1) - as.Date(model_start_date)))

  date_R0_change <- head(it[cumsum(rle(it$C)$lengths)+1,]$date, -1)
  tt_R0 <- seq_along(date_R0_change) - 1L
  R0_change <- head(it[cumsum(rle(it$C)$lengths)+1,]$C, -1)


  # Parameters for run
  R0_min <- 3
  R0_max <- 3
  R0_step <- 1
  first_start_date <- "2020-02-01"
  last_start_date <- "2020-02-29"
  day_step <- 20

  scan_results <- scan_R0_date(R0_min = R0_min,
                               R0_max = R0_max,
                               R0_step = R0_step,
                               first_start_date = first_start_date,
                               last_start_date = last_start_date,
                               day_step = day_step,
                               data = data[1:10,],
                               model_params = parameters_explicit_SEEIR(country = "Algeria"),
                               R0_change = R0_change,
                               date_R0_change = date_R0_change,
                               squire_model = explicit_model(),
                               n_particles = 2)

  # Eralay Feb start most likely
  expect_gt(scan_results$renorm_mat_LL[[1]], scan_results$renorm_mat_LL[[2]])


})

context("sample_grid_scan")

# Only tests that a grid search can be run
test_that("sample_grid_scan works", {


  set.seed(123)
  data <- read.csv(squire_file("extdata/example.csv"))
  model_start_date <- "2020-02-05"

  it <- read.csv(squire_file("extdata/example_intervention.csv"))
  it$date <- as.Date(it$date)
  it <- interventions_unique(it)

  date_R0_change <- it$dates_change
  R0_change <- it$change


  # Parameters for run
  R0_min <- 2
  R0_max <- 4
  R0_step <- 2
  first_start_date <- "2020-02-01"
  last_start_date <- "2020-02-04"
  day_step <- 3

  scan_results <- scan_R0_date(R0_min = R0_min,
                               R0_max = R0_max,
                               R0_step = R0_step,
                               first_start_date = first_start_date,
                               last_start_date = last_start_date,
                               day_step = day_step,
                               data = data[1:10,],
                               model_params = parameters_explicit_SEEIR(country = "Algeria"),
                               R0_change = R0_change,
                               date_R0_change = date_R0_change,
                               squire_model = explicit_model(),
                               n_particles = 2)

  n_sample_pairs <- 2
  res <- sample_grid_scan(scan_results = scan_results,
                          n_sample_pairs = n_sample_pairs,
                          n_particles = 2)

  model <- res$inputs$model$odin_model(user = res$inputs$model_params,
                                       unused_user_action = "ignore")
  # check length based on model and dates
  days_between <- length( min(as.Date(res$param_grid$start_date)) : as.Date(tail(rownames(res$trajectories[,,1]),1)))
  expect_equal(dim(res$trajectories), c(days_between, length(model$initial()), n_sample_pairs))


  res <- sample_grid_scan(scan_results = scan_results,
                          n_sample_pairs = n_sample_pairs,
                          n_particles = 2,
                          full_output = TRUE)
  days_between <- length( min(as.Date(res$param_grid$start_date)) : as.Date(tail(rownames(res$trajectories[,,1]),1)))
  expect_equal(dim(res$trajectories), c(days_between, length(model$.__enclos_env__$private$ynames), n_sample_pairs))


  res <- sample_grid_scan(scan_results = scan_results,
                          n_sample_pairs = n_sample_pairs,
                          n_particles = 2,
                          forecast_days = 5,
                          full_output = TRUE)
  days_between <- length( min(as.Date(res$param_grid$start_date)) : as.Date(tail(rownames(res$trajectories[,,1]),1)))
  expect_equal(dim(res$trajectories), c(days_between, length(model$.__enclos_env__$private$ynames), n_sample_pairs))

  res <- sample_grid_scan(scan_results = scan_results,
                          n_sample_pairs = n_sample_pairs,
                          n_particles = 2,forecast_days = 5,
                          full_output = FALSE)
  model <- res$inputs$model$odin_model(user = res$inputs$model_params,
                                       unused_user_action = "ignore")
  expect_is(model,"odin_model")
  expect_is(odin_index(model),"list")

})
