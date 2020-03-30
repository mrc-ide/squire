test_that("output df works", {
  pop = get_population("Afghanistan")

  set.seed(123)
  r1 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set=contact_matrices[[1]])

  o1 <- long_output(r1)
  expect_type(o1, "list")
  expect_equal(nrow(o1), 100 * 10 * length(pop$n) * 5)
  expect_named(o1, c("t", "age_group", "replicate", "compartment", "y"))
})
