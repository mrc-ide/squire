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
  m2 <- run_explicit_SEEIR_model(R0 = 2,
                                 population = pop$n, dt = 1,
                                 baseline_contact_matrix = contact_matrices[[1]],
                                 contact_matrix_set=contact_matrices[[1]])
  m3 <- run_explicit_SEEIR_model(R0 = 2,
                                 output_transform = FALSE,
                                 population = pop$n, dt = 1,
                                 baseline_contact_matrix = contact_matrices[[1]],
                                 contact_matrix_set=contact_matrices[[1]])
  expect_type(m1, "list")
  expect_s3_class(m1, "squire_simulation")
  expect_s3_class(plot(m1), "gg")
  expect_s3_class(plot(m1, replicates = FALSE), "gg")
  expect_s3_class(plot(m1, ci = FALSE), "gg")
  expect_s3_class(plot(m1, summary_f = median), "gg")
  expect_s3_class(plot(m1, var_select = "S"), "gg")
  expect_s3_class(plot(m2), "gg")
  expect_error(plot(m3), "Plotting does not work with untransformed output, please run
           the model with output_transform = TRUE")
  expect_null(check_squire(m1))
  expect_error(check_squire(1), "Object must be a squire_simulation")
})
