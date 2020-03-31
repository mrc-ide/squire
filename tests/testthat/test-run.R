test_that("run works", {
  pop = get_population("Afghanistan")

  set.seed(123)
  r1 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set=contact_matrices[[1]])
  expect_type(r1$output, "list")
  n <- apply(r1$output$S[,,1], 1, sum) + apply(r1$output$E1[,,1], 1, sum) +
    apply(r1$output$E2[,,1], 1, sum) + apply(r1$output$I[,,1], 1, sum) +
    apply(r1$output$R[,,1], 1, sum)
  expect_true(all(n == sum(pop$n)))
  expect_equal(dim(r1$output$S), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$E1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$E2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$I), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$R), c(100 / 1, length(pop$n), 10))

  # Multiple R0
  set.seed(123)
  r2 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = c(2,2),
                        tt_R0 = c(0, 10),
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set=contact_matrices[[1]])
  expect_identical(r1$output, r2$output)
  set.seed(123)
  r3 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = c(2,5),
                        tt_R0 = c(0, 10),
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set=contact_matrices[[1]])
  expect_gt(sum(r3$output$I), sum(r2$output$I))

  # Multiple contact matrices
  set.seed(123)
  r4 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set = list(contact_matrices[[1]],
                                                  contact_matrices[[1]]),
                        tt_contact_matrix = c(0, 50))
  expect_identical(r1$output, r4$output)

  set.seed(123)
  r5 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set = list(contact_matrices[[1]],
                                                  contact_matrices[[2]]),
                        tt_contact_matrix = c(0, 50))
  expect_true(!identical(r1$output, r5$output))

})
