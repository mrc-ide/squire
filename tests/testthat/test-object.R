context("objects")

test_that("object methods ", {
  # Get a population
  pop <- get_population("Afghanistan", simple_SEIR = TRUE)
  # Run model
  m1 <- run_simple_SEEIR_model(population = pop$n,
                        dt = 1,
                        time_period = 100,
                        replicates = 30,
                        R0 = 4,
                        contact_matrix_set=contact_matrices[[1]])

  # Get a population
  pop <- get_population("Afghanistan", simple_SEIR = FALSE)
  m2 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n,
                                 dt = 1,
                                 time_period = 100,
                                 population_contact_matrix = contact_matrices[[1]],
                                 contact_matrix_set=contact_matrices[[1]])
  m3 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n, dt = 1,
                                 replicates = 5,
                                 time_period = 100,
                                 population_contact_matrix = contact_matrices[[1]],
                                 contact_matrix_set=contact_matrices[[1]])

  expect_type(m1, "list")
  expect_s3_class(m1, "squire_simulation")
  expect_s3_class(plot(m1), "gg")
  expect_s3_class(plot(m1, replicates = FALSE), "gg")
  expect_s3_class(plot(m1, summary_f = median), "gg")
  expect_s3_class(plot(m1, ci = FALSE), "gg")
  expect_s3_class(plot(m1, ci = TRUE), "gg")
  expect_s3_class(plot(m1, summarise = FALSE), "gg")
  expect_s3_class(plot(m1, q = c(0.25, 0.6)), "gg")
  expect_s3_class(plot(m1, var_select = "S"), "gg")
  expect_s3_class(plot(m2), "gg")
  expect_s3_class(plot(m2, replicates = TRUE), "gg")
  expect_s3_class(plot(m2, replicates = FALSE), "gg")
  expect_warning(plot(m3), "Summary statistic estimated from <10 replicates")
  expect_warning(plot(m3, ci = TRUE), "Confidence bounds estimated from <10 replicates")
  expect_null(check_squire(m1))
  expect_error(check_squire(1), "Object must be a squire_simulation")
  expect_error(plot(m1, var_select = c("S", "bad")), "Selected variable are not all present in output")
})
