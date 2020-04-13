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


test_that("beta_explicit input check work", {
  mm <- matrix(runif(4), ncol = 2)
  mm_na <- mm
  mm_na[1] <- NA

  expect_error(beta_est_explicit(dur_IMild = 0, dur_ICase = 5,
                                 prob_hosp = c(0.2,0.1),
                                 mixing_matrix = mm, R0 = 2),
               "dur_IMild must be greater than zero")

  expect_error(beta_est_explicit(dur_IMild = 2, dur_ICase = 0,
                                 prob_hosp = c(0.2,0.1),
                                 mixing_matrix = mm, R0 = 2),
               "dur_ICase must be greater than zero")

  expect_error(beta_est_explicit(dur_IMild = c(2,2), dur_ICase = 5,
                                 prob_hosp = c(0.2,0.1),
                                 mixing_matrix = mm, R0 = 2),
               "dur_IMild must be of length 1")

  expect_error(beta_est_explicit(dur_IMild = 2, dur_ICase = 5,
                                 prob_hosp = c(0.2,NA),
                                 mixing_matrix = mm, R0 = 2),
               "prob_hosp must not contain NAs")

  expect_error(beta_est_explicit(dur_IMild = 2, dur_ICase = 5,
                                 prob_hosp = c(0.2,0.2),
                                 mixing_matrix = mm_na, R0 = 2),
               "mixing_matrix must not contain NAs")

})
