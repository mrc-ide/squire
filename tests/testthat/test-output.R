test_that("output format works", {
  pop = get_population("Afghanistan", simple_SEIR = TRUE)
  set.seed(123)
  r1 <- run_simple_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 10,
                        replicates = 10,
                        contact_matrix_set=contact_matrices[[1]])
  o1 <- format_output(r1)
  o2 <- format_output(r1, reduce_age = FALSE)
  o3 <- collapse_for_report(format_output(r1, reduce_age = FALSE))


  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o3, "list")
  expect_named(o1, c("compartment", "t",  "replicate", "y"))
  expect_named(o2, c("replicate", "age_group", "compartment", "t",  "y"))

  pop <- get_population("Afghanistan", simple_SEIR = FALSE)
  m1 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n,
                                 dt = 1,
                                 time_period = 10,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])

  o1 <- format_output(m1)
  o2 <- format_output(m1, reduce_age = FALSE)
  o3 <- collapse_for_report(format_output(m1, reduce_age = FALSE))
  o4 <- format_output(m1, reduce_age = FALSE, date_0 = Sys.Date())
  o5 <- format_output(m1, var_select = c("E", "ICase"))
  o6 <- format_output(m1, var_select = c("E", "IMild"))

  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o3, "list")
  expect_type(o4, "list")
  expect_type(o5, "list")
  expect_type(o6, "list")

  expect_named(o1, c("compartment", "t", "replicate", "y"))
  expect_named(o2, c("replicate", "age_group", "compartment","t", "y"))
  expect_named(o3, c("compartment", "t", "replicate", "y"))
  expect_true(all(c("hospital","ICU","IMild","deaths") %in% unique(o3$compartment)))
  expect_named(o4, c("replicate", "age_group", "compartment", "t", "y", "date"))
  expect_named(o5, c("compartment", "t", "replicate", "y"))
  expect_named(o6, c("compartment", "t", "replicate", "y"))

  expect_error(format_output(m1, reduce_age = FALSE, date_0 = "wrong"))
  expect_error(format_output(m1, var_select = "moon"))
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
  expect_named(deaths, c("compartment", "t", "replicate", "y"))
  infection_incidence <- extract_infection_incidence(m1)
  expect_named(infection_incidence, c("compartment", "t", "replicate", "y"))
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
                 reporting_fraction = 1, dt=1, replicates = 10,
                 time_period = 365)
  get <- format_output(r, reduce_age = FALSE, combine_compartments = FALSE,
                       date_0 = Sys.Date())

  expect_true(table(table(get[get$replicate==1 & get$compartment == "D",]$t)) == 365)
  expect_true(table(table(get[get$replicate==1 & get$compartment == "D",]$date)) == 365)

})
