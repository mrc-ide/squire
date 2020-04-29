test_that("deterministic output format works", {
  pop <- get_population("Afghanistan")
  m <- get_mixing_matrix("Afghanistan")
  model_output <- run_deterministic_SEIR_model(
    pop$n, m, c(0, 50), c(3, 3/2), 2, 100000, 1000000)

  # reset output
  model_output$output[,2:dim(model_output$output)[[2]]] <- 0

  # mock deaths
  model_output$output[1, c('D[2]')] <- 2
  model_output$output[2, c('D[10]')] <- 2
  model_output$output[2, c('D[16]')] <- 5

  # mock infections
  model_output$output[1, c('IMild[17]')] <- 4
  model_output$output[2, c('IMild[2]', 'ICase1[5]')] <- 5

  # mock beds
  model_output$output[1, c('IOxGetLive1[10]')] <- 2
  model_output$output[2, c('IOxGetLive2[7]', 'IOxNotGetLive2[3]')] <- 3

  # mock icu
  model_output$output[1, c('IMVGetLive1[10]')] <- 1
  model_output$output[2, c('IMVGetLive2[7]', 'IMVNotGetLive2[3]')] <- 2

  actual <- format_deterministic_output(model_output)
  vars <- c('deaths','infections','hospital_demand','ICU_demand')
  expected <- data.frame(
    t = rep(c(0, 1), length(vars)),
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
  o3 <- collapse_for_report(format_output_simple_model(r1, reduce_age = FALSE))
  o4 <- format_output_simple_model(r1, var_select = "E")
  o5 <- format_output_simple_model(r1, var_select = "E", date_0 = Sys.Date())

  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o3, "list")
  expect_type(o4, "list")
  expect_type(o5, "list")

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
  o3 <- collapse_for_report(format_output(m1, reduce_age = FALSE))
  o4 <- format_output(m1, reduce_age = FALSE, date_0 = Sys.Date())
  o5 <- format_output(m1, var_select = c("E", "ICase"))
  o6 <- format_output(m1, var_select = c("E", "IMild"))
  o7 <- format_output(m1, var_select = NULL)
  o8 <- format_output(m1, var_select = "E", combine_compartments = FALSE)
  o9 <- format_output(m1, var_select = "deaths")

  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o3, "list")
  expect_type(o4, "list")
  expect_type(o5, "list")
  expect_type(o6, "list")
  expect_type(o7, "list")
  expect_type(o8, "list")
  expect_type(o9, "list")

  expect_named(o1, c("replicate", "compartment", "t", "y"))
  expect_named(o2, c("replicate", "age_group", "compartment", "t",  "y"))
  expect_named(o3, c("compartment", "t", "replicate", "y"))
  expect_true(all(c("hospital","ICU","IMild","deaths") %in% unique(o3$compartment)))
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

  r <- calibrate(country = "Afghanistan", deaths = 6,
                 reporting_fraction = 1, dt=0.5, replicates = 3,
                 time_period = 365)
  get <- format_output(r, var_select = "E", reduce_age = FALSE, combine_compartments = TRUE,
                       date_0 = Sys.Date())

  expect_equal(length(get$compartment), 1/0.5 * 17 * r$parameters$replicates * r$parameters$time_period)
  expect_named(get, c("replicate", "age_group", "compartment", "t", "y", "date"))

  get <- format_output(r, var_select = "E", reduce_age = TRUE, combine_compartments = TRUE,
                       date_0 = Sys.Date())

  expect_equal(length(get$compartment), 1/0.5 * r$parameters$replicates * r$parameters$time_period)
  expect_named(get, c("replicate", "compartment", "t", "y", "date"))

})


test_that("calibrate_output_parsing vs format_output",{

  m1 <- calibrate(country = "Afghanistan", deaths = 6,
                       reporting_fraction = 1, dt=0.5, replicates = 3,
                       time_period = 365)

  o1 <- format_output(m1, c("R","deaths","infections","hospital_demand","ICU_demand"))
  g2 <- calibrate_output_parsing(m1)
  expect_identical(o1$y[o1$replicate == 1 & o1$compartment == "ICU_demand"],
                   g2$y[g2$replicate == 1 & g2$compartment == "ICU_demand"])


  index <- odin_index(m1$model)
  mv <- unlist(index[c("IMVGetLive1","IMVGetLive2","IMVGetDie1","IMVGetDie2",
                           "IMVNotGetLive1","IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2")])
  expect_true(identical(rowSums(m1$output[,mv,1]),
                        o1$y[o1$replicate == 1 & o1$compartment == "ICU_demand"]))

  expect_true(identical(rowSums(m1$output[,mv,2]),
                        o1$y[o1$replicate == 2 & o1$compartment == "ICU_demand"]))

  expect_true(identical(rowSums(m1$output[,mv,1]),
                        g2$y[g2$replicate == 1 & g2$compartment == "ICU_demand"]))

  expect_true(identical(rowSums(m1$output[,mv,2]),
                        g2$y[g2$replicate == 2 & g2$compartment == "ICU_demand"]))



})
