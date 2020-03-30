test_that("beta input check work", {
  mm <- matrix(runif(4), ncol = 2)
  expect_error(beta_est("A", mm, 1),
               "duration_infectiousness must be a positive numeric value")
  expect_error(beta_est(1:2, mm, 1),
               "duration_infectiousness must be of length = 1")
  expect_error(beta_est(-1, mm, 1),
               "duration_infectiousness must be a positive numeric value")

  expect_error(beta_est(1, mm, "A"),
               "R0 must be a positive numeric value")
  expect_error(beta_est(1, mm, -1),
               "R0 must be a positive numeric value")

  expect_error(beta_est(1, "A", 1),
               "mixing_matrix must be a matrix")
  expect_error(beta_est(1, data.frame(a = 1:2, b = 1:2), 1),
               "mixing_matrix must be a matrix")
})
