test_that("positive number checks", {
  expect_null(pos_num(1, "a"))
  expect_error(pos_num(-1, "a"), "a must be a positive number")
  expect_error(pos_num(-1, "b"), "b must be a positive number")
  expect_error(pos_num("a", "a"), "a must be a positive number")
  expect_error(pos_num(1:2, "a"), "a must have length = 1")
})

test_that("time point checks", {
  expect_null(check_time_change(1:10, 11))
  expect_error(check_time_change(1:11, 10), "Time change points must all be < time period
         and > 0")
  expect_error(check_time_change(-1:9, 10), "Time change points must all be < time period
         and > 0")
})

test_that("matrix checks", {
  p <- 1:10
  b <- matrix(1:100, ncol = 10)
  m <- list(b, b)
  expect_null(matrix_check(p, b, m))
  expect_error(matrix_check(1:11, b, m), "Length of population vector, dimensions of baseline_contact_matrix
         and dimensions of matrices in contact_matrix_set must all be equal")
  b2 = matrix(1:100, ncol = 5)
  expect_error(matrix_check(p, b2, m), "Length of population vector, dimensions of baseline_contact_matrix
         and dimensions of matrices in contact_matrix_set must all be equal")
  m2 <- list(b2, b2)
  expect_error(matrix_check(p, b, m2), "Length of population vector, dimensions of baseline_contact_matrix
         and dimensions of matrices in contact_matrix_set must all be equal")
})

test_that("init checks", {
  t1 <- init_check(NULL, 1:10)
  expect_type(t1, "list")
  expect_equal(nrow(t1), 10)
  expect_named(t1, c("S", "E", "E2", "I", "R"))
  expect_error(init_check(1:2, 1:10), "init should be a data.frame with columns:, S, E, E2, I, R
           and rows 1:age_groups")
  bad <- data.frame(x = 1:10)
  expect_error(init_check(bad, 1:10), "Names of init must be identical to S, E, E2, I, R if sepecified manually")
  bad2 <- t1
  bad2[1,1] <- 10
  expect_error(init_check(bad2, 1:10), "Row sums of init should be identical to population")
})
