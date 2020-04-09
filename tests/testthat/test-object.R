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
  expect_type(m1, "list")
  expect_s3_class(m1, "squire_simulation")
  expect_s3_class(plot(m1), "gg")
  expect_null(check_squire(m1))
  expect_error(check_squire(1), "Object must be a squire_simulation")
})
