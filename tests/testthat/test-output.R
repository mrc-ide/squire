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
  o3 <- format_output(r1, reduce_age = FALSE, reduce_compartment = FALSE)

  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o3, "list")
  expect_named(o1, c("compartment", "t", "replicate", "y"))
  expect_named(o2, c("compartment", "t", "replicate", "y"))
  expect_named(o3, c("replicate", "compartment", "age_group", "t", "y"))

  pop <- get_population("Afghanistan", simple_SEIR = FALSE)
  m1 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n,
                                 dt = 1,
                                 time_period = 10,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])

  o1 <- format_output(m1)
  o2 <- format_output(m1, reduce_age = FALSE)
  o3 <- format_output(m1, reduce_age = FALSE, reduce_compartment = FALSE)

  expect_type(o1, "list")
  expect_type(o2, "list")
  expect_type(o3, "list")
  expect_named(o1, c("compartment", "t", "replicate", "y"))
  expect_named(o2, c("compartment", "t", "replicate", "y"))
  expect_named(o3, c("replicate", "compartment", "age_group", "t", "y"))

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
