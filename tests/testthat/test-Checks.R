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
  expect_null(matrix_check(p, m))
  expect_error(matrix_check(1:11, m),
               "Length of population vector and dimensions of matrices")
  b2 = matrix(1:100, ncol = 5)
  expect_error(matrix_check(p, list(m)),
               "Length of population vector and dimensions of matrices")
  m2 <- list(b2, b2)
  expect_error(matrix_check(p, m2),
               "Length of population vector and dimensions of matrices")
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

test_that("init explicit checks", {
  expect_error(init_check_explicit(NULL, 1:10, 20), "population must")
  expect_error(init_check_explicit(NULL, 20:36, 20.5))
  t1 <- init_check_explicit(NULL, 20:36, 20)
  expect_type(t1, "list")
  expect_equal(nrow(t1), 17)
  expect_named(t1, c("S","E1","E2","IMild","ICase1","ICase2","IOxGetLive1",
                     "IOxGetLive2","IOxGetDie1","IOxGetDie2","IOxNotGetLive1",
                     "IOxNotGetLive2","IOxNotGetDie1","IOxNotGetDie2","IMVGetLive1",
                     "IMVGetLive2","IMVGetDie1","IMVGetDie2","IMVNotGetLive1",
                     "IMVNotGetLive2","IMVNotGetDie1","IMVNotGetDie2","IRec1",
                     "IRec2","R","D"))
  expect_error(init_check_explicit(1:2, 1:17, 20), "init should be a data.frame with columns:")
  bad <- data.frame(x = 1:10)
  expect_error(init_check_explicit(bad, 1:17, 20), "S, E1, E2, ICase1, ICase2, IOxGetLive1, IOxGetLive2")
  bad2 <- t1
  bad2[1,1] <- 10
  expect_error(init_check_explicit(bad2, 0:16, 20), "Row sums of init should be identical to population")
})
