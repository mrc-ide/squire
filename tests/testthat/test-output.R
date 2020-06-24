test_that("deterministic output format works", {
  pop <- get_population("Afghanistan")
  m <- get_mixing_matrix("Afghanistan")
  model_output <- run_deterministic_SEIR_model(
      population = pop$n, contact_matrix_set = m,
      tt_R0 = c(0), R0 = c(3), time_period = 2,
      day_return = TRUE,
      hosp_bed_capacity = 100000,
      ICU_bed_capacity = 1000000)

  # reset output
  model_output$output[,2:dim(model_output$output)[[2]],1] <- 0

  # mock deaths
  model_output$output[1, c('D[2]'), 1] <- 2
  model_output$output[2, c('D[10]'), 1] <- 2
  model_output$output[2, c('D[16]'), 1] <- 5

  # mock infections
  model_output$output[1, c('IMild[17]'), 1] <- 4
  model_output$output[2, c('IMild[2]', 'ICase1[5]'), 1] <- 5

  # mock beds
  model_output$output[1, c('IOxGetLive1[10]'), 1] <- 2
  model_output$output[2, c('IOxGetLive2[7]', 'IOxNotGetLive2[3]'), 1] <- 3

  # mock icu
  model_output$output[1, c('IMVGetLive1[10]'), 1] <- 1
  model_output$output[2, c('IMVGetLive2[7]', 'IMVNotGetLive2[3]'), 1] <- 2

  actual <- format_deterministic_output(model_output)
  vars <- c('deaths','infections','hospital_demand','ICU_demand')
  expected <- data.frame(
    t = rep(c(1, 2), length(vars)),
    compartment = rep(vars, each = 2),
    value = c(2, 5, 4, 10, 2, 6, 1, 4),
    stringsAsFactors = FALSE
  )
  rownames(actual) <- seq_len(8)
  expect_mapequal(actual, expected)
})

test_that("output format works", {
  pop = get_population("Afghanistan", simple_SEIR = TRUE)
  set.seed(123)
  r1 <- run_simple_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 10,
                        replicates = 10,
                        contact_matrix_set=contact_matrices[[1]])
  o1 <- format_output_simple_model(r1)
  o2 <- format_output_simple_model(r1, reduce_age = FALSE)
  o4 <- format_output_simple_model(r1, var_select = "E")
  o5 <- format_output_simple_model(r1, var_select = "I")
  o6 <- format_output_simple_model(r1, var_select = "E", date_0 = Sys.Date())

  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o4, "list")
  expect_type(o5, "list")
  expect_type(o6, "list")

  pop <- get_population("Afghanistan", simple_SEIR = FALSE)
  m1 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n,
                                 dt = 1,
                                 time_period = 10,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])

  o1 <- format_output(m1)
  expect_true(all(table(o1$compartment) == 100))
  o2 <- format_output(m1, reduce_age = FALSE)
  o4 <- format_output(m1, reduce_age = FALSE, date_0 = Sys.Date())
  o5 <- format_output(m1, var_select = c("E", "ICase"))
  o6 <- format_output(m1, var_select = c("E", "IMild"))
  o7 <- format_output(m1, var_select = NULL)
  o8 <- format_output(m1, var_select = "E", combine_compartments = FALSE)
  o9 <- format_output(m1, var_select = "deaths")

  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o4, "list")
  expect_type(o5, "list")
  expect_type(o6, "list")
  expect_type(o7, "list")
  expect_type(o8, "list")
  expect_type(o9, "list")

  expect_named(o1, c("replicate", "compartment", "t", "y"))
  expect_named(o2, c("replicate", "age_group", "compartment", "t",  "y"))
  expect_named(o4, c("replicate", "age_group", "compartment", "t", "y", "date"))
  expect_named(o5, c("replicate", "compartment", "t", "y"))
  expect_named(o6, c("replicate", "compartment", "t", "y"))
  expect_named(o7, c("replicate", "compartment", "t", "y"))
  expect_named(o8, c("t", "replicate", "compartment", "y"))
  expect_named(o9, c("replicate", "compartment", "t", "y"))

  expect_error(format_output(m1, reduce_age = FALSE, date_0 = "wrong"))
  expect_error(format_output(m1, var_select = "moon"))
  expect_error(format_output_simple_model(m1, var_select = "moon"))

})

test_that("new helper functions to extract relevant outputs", {

  pop <- get_population("Afghanistan", simple_SEIR = FALSE)
  m1 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n,
                                 dt = 1,
                                 time_period = 10,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])
  deaths <- extract_deaths(m1)
  expect_named(deaths, c("replicate", "compartment", "t", "y"))
  infection_incidence <- extract_infection_incidence(m1)
  expect_named(infection_incidence, c("replicate", "compartment", "t", "y"))
  hospital_occ <- extract_hospital_occ(m1)
  expect_named(hospital_occ, c("t", "replicate", "y"))
  ICU_occ <- extract_ICU_occ(m1)
  expect_named(ICU_occ, c("t", "replicate", "y"))

})


test_that("squire object check and summary", {

  pop = get_population("Afghanistan", simple_SEIR = TRUE)
  set.seed(123)
  r1 <- run_simple_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = 2,
                               time_period = 400,
                               replicates = 10,
                               contact_matrix_set=contact_matrices[[1]])

  # check correctly identifies
  expect_silent(check_squire(r1))

  # check shows non squire
  naive <- data.frame("a" = 1, "b" = 3)
  expect_error(check_squire(naive), "Object must be a squire_simulation")

  # summary tests
  expect_output(summary(r1), regexp = "squire simulation")
  expect_output(print(r1), regexp = "1.1 years")



})


test_that("t correct in format_outputs",{

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

  r <- calibrate(
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

  get <- format_output(r, var_select = "E", reduce_age = FALSE, combine_compartments = TRUE,
                       date_0 = max(data$date))

  expect_equal(length(get$compartment), 17 * r$parameters$replicates * nrow(r$output))
  expect_named(get, c("replicate", "age_group", "compartment", "t", "y", "date"))

  get <- format_output(r, var_select = "E", reduce_age = TRUE, combine_compartments = TRUE,
                       date_0 = max(data$date))

  expect_equal(length(get$compartment),  r$parameters$replicates * nrow(r$output))
  expect_named(get, c("replicate", "compartment", "t", "y", "date"))

})


test_that("calibrate_output_parsing vs format_output",{

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

  m1 <- calibrate(
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

  o1 <- format_output(m1, c("R","deaths","infections","hospital_demand","ICU_demand"))

  index <- odin_index(m1$model)
  mv <- unlist(index[c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
                           "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2")])
  expect_true(identical(as.numeric(rowSums(m1$output[,mv,1])),
                        o1$y[o1$replicate == 1 & o1$compartment == "ICU_demand"]))

  expect_true(identical(as.numeric(rowSums(m1$output[,mv,2])),
                        o1$y[o1$replicate == 2 & o1$compartment == "ICU_demand"]))


})

test_that("output format works for hospitalisation and ICU incidence for stochastic and deterministic versions", {
  set.seed(123)
  pop <- get_population("Afghanistan", simple_SEIR = FALSE)
  m1 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n,
                                 dt = 1,
                                 time_period = 10,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])

  ICU_incidence <- format_output(m1, var_select = "ICU_incidence")
  hosp_incidence <- format_output(m1, var_select = "hospital_incidence")
  expect_type(ICU_incidence, "list")
  expect_type(hosp_incidence, "list")

  m <- get_mixing_matrix("Afghanistan")
  model_output <- run_deterministic_SEIR_model(
    population = pop$n, contact_matrix_set = m,
    tt_R0 = c(0), R0 = c(3), time_period = 2,
    day_return = TRUE,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000)

  actual <- format_deterministic_output(model_output)
  compartment_names <- unique(actual$compartment)
  expect_true("hospital_incidence" %in% compartment_names)
  expect_true("ICU_incidence" %in% compartment_names)

})


