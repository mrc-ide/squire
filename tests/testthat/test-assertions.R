context("assertions")

#------------------------------------------------
test_that("nice_format working correctly", {
  expect_true(nice_format(NULL) == "")
  expect_true(nice_format(5) == "5")
  expect_true(nice_format(1:5) == "{1, 2, 3, 4, 5}")
  nice_format(c("foo", "bar") == "{foo, bar}")
})

#------------------------------------------------
test_that("assert_null working correctly", {
  expect_true(assert_null(NULL))

  expect_error(assert_null(5))
})

#------------------------------------------------
test_that("assert_non_null working correctly", {
  expect_true(assert_non_null(5))

  expect_error(assert_non_null(NULL))
})

#------------------------------------------------
test_that("assert_atomic correctly", {
  expect_true(assert_atomic(TRUE))
  expect_true(assert_atomic(1))
  expect_true(assert_atomic(seq(0,5,0.5)))
  expect_true(assert_atomic(0i))
  expect_true(assert_atomic("foo"))
  expect_true(assert_atomic(raw(8)))
  expect_true(assert_atomic(matrix(0,3,3)))

  expect_error(assert_atomic(list(1:5)))
  expect_error(assert_atomic(data.frame(1:5)))
})

#------------------------------------------------
test_that("assert_single correctly", {
  expect_true(assert_single(TRUE))
  expect_true(assert_single(1))
  expect_true(assert_single("foo"))

  expect_error(assert_single(NULL))
  expect_error(assert_single(1:5))
  expect_error(assert_single(list(0)))
  expect_error(assert_single(matrix(0,3,3)))
  expect_error(assert_single(data.frame(0)))
})

#------------------------------------------------
test_that("assert_string working correctly", {
  expect_true(assert_string("foo"))
  expect_true(assert_string(c("foo", "bar")))

  expect_error(assert_string(NULL))
  expect_error(assert_string(5))
  expect_error(assert_string(1:5))
})

#------------------------------------------------
test_that("assert_single_string working correctly", {
  expect_true(assert_single_string("foo"))

  expect_error(assert_single_string(NULL))
  expect_error(assert_single_string(c("foo", "bar")))
  expect_error(assert_single_string(5))
  expect_error(assert_single_string(1:5))
})

#------------------------------------------------
test_that("assert_logical working correctly", {
  expect_true(assert_logical(TRUE))
  expect_true(assert_logical(c(TRUE, FALSE)))

  expect_error(assert_logical(NULL))
  expect_error(assert_logical("TRUE"))
  expect_error(assert_logical(5))
  expect_error(assert_logical(1:5))
})

#------------------------------------------------
test_that("assert_single_logical working correctly", {
  expect_true(assert_single_logical(TRUE))

  expect_error(assert_single_logical(NULL))
  expect_error(assert_single_logical(c(TRUE, FALSE)))
  expect_error(assert_single_logical("TRUE"))
  expect_error(assert_single_logical(5))
  expect_error(assert_single_logical(1:5))
})

#------------------------------------------------
test_that("assert_numeric working correctly", {
  expect_true(assert_numeric(5))
  expect_true(assert_numeric(-5:5))

  expect_error(assert_numeric(NULL))
  expect_error(assert_numeric("foo"))
  expect_error(assert_numeric(c(1, "foo")))
})

#------------------------------------------------
test_that("assert_single_numeric working correctly", {
  expect_true(assert_single_numeric(5))

  expect_error(assert_single_numeric(-5:5))
  expect_error(assert_single_numeric(NULL))
  expect_error(assert_single_numeric("foo"))
  expect_error(assert_single_numeric(c(1, "foo")))
})

#------------------------------------------------
test_that("assert_int working correctly", {
  expect_true(assert_int(5))
  expect_true(assert_int(-5))
  expect_true(assert_int(-5:5))
  expect_true(assert_int(c(a = 5)))

  expect_error(assert_int(NULL))
  expect_error(assert_int(0.5))
  expect_error(assert_int("foo"))
  expect_error(assert_int(c(5,"foo")))
})

#------------------------------------------------
test_that("assert_single_int working correctly", {
  expect_true(assert_single_int(5))
  expect_true(assert_single_int(-5))

  expect_error(assert_single_int(-5:5))
  expect_error(assert_single_int(NULL))
  expect_error(assert_single_int(0.5))
  expect_error(assert_single_int("foo"))
  expect_error(assert_single_int(c(5,"foo")))
})

#------------------------------------------------
test_that("assert_pos working correctly", {
  expect_true(assert_pos(5))
  expect_true(assert_pos(seq(1, 5, 0.5)))
  expect_true(assert_pos(seq(0, 5, 0.5), zero_allowed = TRUE))

  expect_error(assert_pos(NULL))
  expect_error(assert_pos(-5))
  expect_error(assert_pos(seq(-1, -5, -0.5)))
  expect_error(assert_pos(seq(0, 5, 0.5), zero_allowed = FALSE))
  expect_error(assert_pos(seq(-5, 5, 0.5), zero_allowed = TRUE))
  expect_error(assert_pos("foo"))
})

#------------------------------------------------
test_that("assert_single_pos working correctly", {
  expect_true(assert_single_pos(5))

  expect_error(assert_single_pos(seq(1, 5, 0.5)))
  expect_error(assert_single_pos(seq(0, 5, 0.5), zero_allowed = TRUE))
  expect_error(assert_single_pos(NULL))
  expect_error(assert_single_pos(-5))
  expect_error(assert_single_pos(seq(-1, -5, -0.5)))
  expect_error(assert_single_pos(seq(0, 5, 0.5), zero_allowed = FALSE))
  expect_error(assert_single_pos(seq(-5, 5, 0.5), zero_allowed = TRUE))
  expect_error(assert_single_pos("foo"))
})

#------------------------------------------------
test_that("assert_pos_int working correctly", {
  expect_true(assert_pos_int(5))
  expect_true(assert_pos_int(0, zero_allowed = TRUE))
  expect_true(assert_pos_int(1:5))
  expect_true(assert_pos_int(0:5, zero_allowed = TRUE))

  expect_error(assert_pos_int(NULL))
  expect_error(assert_pos_int(-5))
  expect_error(assert_pos_int(-1:-5))
  expect_error(assert_pos_int(0:5, zero_allowed = FALSE))
  expect_error(assert_pos_int(-5:5, zero_allowed = TRUE))
  expect_error(assert_pos_int("foo"))
})

#------------------------------------------------
test_that("assert_single_pos_int working correctly", {
  expect_true(assert_single_pos_int(5))
  expect_true(assert_single_pos_int(0, zero_allowed = TRUE))

  expect_error(assert_single_pos_int(1:5))
  expect_error(assert_single_pos_int(0:5, zero_allowed = TRUE))
  expect_error(assert_single_pos_int(NULL))
  expect_error(assert_single_pos_int(-5))
  expect_error(assert_single_pos_int(-1:-5))
  expect_error(assert_single_pos_int(0:5, zero_allowed = FALSE))
  expect_error(assert_single_pos_int(-5:5, zero_allowed = TRUE))
  expect_error(assert_single_pos_int("foo"))
})

#------------------------------------------------
test_that("assert_single_bounded working correctly", {
  expect_true(assert_single_bounded(0.5))
  expect_true(assert_single_bounded(5, left = 0, right = 10))
  expect_true(assert_single_bounded(0, inclusive_left = TRUE))
  expect_true(assert_single_bounded(1, inclusive_right = TRUE))

  expect_error(assert_single_bounded(NULL))
  expect_error(assert_single_bounded(1:5))
  expect_error(assert_single_bounded(5))
  expect_error(assert_single_bounded(0, inclusive_left = FALSE))
  expect_error(assert_single_bounded(1, inclusive_right = FALSE))
  expect_error(assert_single_bounded("foo"))
})

#------------------------------------------------
test_that("assert_vector working correctly", {
  expect_true(assert_vector(1))
  expect_true(assert_vector(1:5))

  expect_error(assert_vector(NULL))
  expect_error(assert_vector(matrix(5,3,3)))
  expect_error(assert_vector(list(1:5, 1:10)))
  expect_error(assert_vector(data.frame(1:5, 2:6)))
})

#------------------------------------------------
test_that("assert_matrix working correctly", {
  expect_true(assert_matrix(matrix(NA,1,1)))
  expect_true(assert_matrix(matrix(5,3,3)))

  expect_error(assert_matrix(NULL))
  expect_error(assert_matrix(5))
  expect_error(assert_matrix(1:5))
  expect_error(assert_matrix(list(1:5, 1:10)))
  expect_error(assert_matrix(data.frame(1:5, 2:6)))
})

#------------------------------------------------
test_that("assert_list working correctly", {
  expect_true(assert_list(list(1:5)))
  expect_true(assert_list(data.frame(x = 1:5)))

  expect_error(assert_list(NULL))
  expect_error(assert_list(5))
  expect_error(assert_list(1:5))
  expect_error(assert_list(matrix(NA, 3, 3)))
})

#------------------------------------------------
test_that("assert_date working correctly", {

  expect_true(assert_date(Sys.Date()))
  expect_true(assert_date(as.character(Sys.Date())))

  expect_error(assert_date(NULL))
  expect_error(assert_date(5))
  expect_error(assert_date("jibebers"))

})


#------------------------------------------------
test_that("assert_dataframe working correctly", {
  expect_true(assert_dataframe(data.frame(x = 1:5)))

  expect_error(assert_dataframe(NULL))
  expect_error(assert_dataframe(5))
  expect_error(assert_dataframe(1:5))
  expect_error(assert_dataframe(list(1:5)))
  expect_error(assert_dataframe(matrix(NA, 3, 3)))
})

#------------------------------------------------
test_that("assert_custom_class working correctly", {
  expect_true(assert_custom_class(NULL, "NULL"))
  expect_true(assert_custom_class(data.frame(1:5), "data.frame"))

  expect_error(assert_custom_class(NULL, "foo"))
  expect_error(assert_custom_class(data.frame(1:5), "foo"))
})

#------------------------------------------------
test_that("assert_limit working correctly", {
  expect_true(assert_limit(c(0,1)))
  expect_true(assert_limit(c(-10,10)))

  expect_error(assert_limit(NULL))
  expect_error(assert_limit(1:5))
  expect_error(assert_limit(1))
  expect_error(assert_limit(c(2,1)))
  expect_error(assert_limit("foo"))
})

#------------------------------------------------
test_that("assert_eq working correctly", {
  expect_true(assert_eq(5,5))
  expect_true(assert_eq(1:5,1:5))
  expect_true(assert_eq("foo","foo"))
  expect_true(assert_eq(c(a = 5), c(b = 5)))

  expect_error(assert_eq(NULL,5))
  expect_error(assert_eq(5,NULL))
  expect_error(assert_eq(NULL,NULL))
  expect_error(assert_eq(4,5))
  expect_error(assert_eq(1:4,1:5))
  expect_error(assert_eq("foo","bar"))
})

#------------------------------------------------
test_that("assert_neq working correctly", {
  expect_true(assert_neq(4,5))
  expect_true(assert_neq(2:6,1:5))
  expect_true(assert_neq("foo","bar"))

  expect_error(assert_neq(NULL,5))
  expect_error(assert_neq(5,NULL))
  expect_error(assert_neq(NULL,NULL))
  expect_error(assert_neq(5,5))
  expect_error(assert_neq(1:5,1:5))
  expect_error(assert_neq("foo","foo"))
})

#------------------------------------------------
test_that("assert_gr working correctly", {
  expect_true(assert_gr(5,4))
  expect_true(assert_gr(1:5,0))
  expect_true(assert_gr(2:6,1:5))

  expect_error(assert_gr(NULL,5))
  expect_error(assert_gr(5,NULL))
  expect_error(assert_gr(NULL,NULL))
  expect_error(assert_gr(3,3))
  expect_error(assert_gr(3,4))
  expect_error(assert_gr(1:4,1:5))
})

#------------------------------------------------
test_that("assert_greq working correctly", {
  expect_true(assert_greq(5,4))
  expect_true(assert_greq(5,5))
  expect_true(assert_greq(1:5,0))
  expect_true(assert_greq(2:6,1:5))
  expect_true(assert_greq(1:5,1:5))

  expect_error(assert_greq(NULL,5))
  expect_error(assert_greq(5,NULL))
  expect_error(assert_greq(NULL,NULL))
  expect_error(assert_greq(3,4))
  expect_error(assert_greq(1:4,1:5))
})

#------------------------------------------------
test_that("assert_le working correctly", {
  expect_true(assert_le(4,5))
  expect_true(assert_le(1:5,9))
  expect_true(assert_le(2:6,3:7))

  expect_error(assert_le(NULL,5))
  expect_error(assert_le(5,NULL))
  expect_error(assert_le(NULL,NULL))
  expect_error(assert_le(3,3))
  expect_error(assert_le(4,3))
  expect_error(assert_le(1:4,1:5))
})

#------------------------------------------------
test_that("assert_leq working correctly", {
  expect_true(assert_leq(4,5))
  expect_true(assert_leq(4,4))
  expect_true(assert_leq(1:5,9))
  expect_true(assert_leq(2:6,2:6))

  expect_error(assert_leq(NULL,5))
  expect_error(assert_leq(5,NULL))
  expect_error(assert_leq(NULL,NULL))
  expect_error(assert_leq(4,3))
  expect_error(assert_leq(2:6,1:5))
})

#------------------------------------------------
test_that("assert_bounded working correctly", {
  expect_true(assert_bounded(seq(0, 1, 0.1)))
  expect_true(assert_bounded(seq(-5, 5, 0.1), left = -5, right = 5))

  expect_error(assert_bounded(NULL))
  expect_error(assert_bounded(seq(0, 1, 0.1), left = 0.1))
  expect_error(assert_bounded(seq(0, 1, 0.1), right = 0.9))
  expect_error(assert_bounded(0, left = 0, inclusive_left = FALSE))
  expect_error(assert_bounded(1, right = 1, inclusive_right = FALSE))
  expect_error(assert_bounded("foo"))
})

#------------------------------------------------
test_that("assert_in working correctly", {
  expect_true(assert_in(3, 3))
  expect_true(assert_in(3, 1:5))
  expect_true(assert_in("foo", c("foo", "bar")))

  expect_error(assert_in(NULL, 5))
  expect_error(assert_in(5, NULL))
  expect_error(assert_in(NULL, NULL))
  expect_error(assert_in(1:5, 1:4))
  expect_error(assert_in("foo", c("bar", "roo")))
})

#------------------------------------------------
test_that("assert_not_in working correctly", {
  expect_true(assert_not_in(3, 4))
  expect_true(assert_not_in(1:5, 6:10))
  expect_true(assert_not_in("foo", c("bar", "roo")))

  expect_error(assert_not_in(NULL, 5))
  expect_error(assert_not_in(5, NULL))
  expect_error(assert_not_in(NULL, NULL))
  expect_error(assert_not_in(1:5, 5:10))
  expect_error(assert_not_in("foo", c("foo", "bar")))
})

#------------------------------------------------
test_that("assert_length working correctly", {
  expect_true(assert_length(1:3, 3))
  expect_true(assert_length(3, 1))
  expect_true(assert_length(matrix(NA,3,4), 12))

  expect_error(assert_length(1:3, NULL))
  expect_error(assert_length(NULL, 1:3))
  expect_error(assert_length(NULL, NULL))
  expect_error(assert_length(3, 2))
  expect_error(assert_length(1:3, 2))
  expect_error(assert_length(matrix(NA,3,4), 9))
})

#------------------------------------------------
test_that("assert_same_length working correctly", {
  expect_true(assert_same_length(1:3, c("a", "b", "c")))

  expect_error(assert_same_length(NULL))
  expect_error(assert_same_length(1:3, c("a", "b")))
})

#------------------------------------------------
test_that("assert_same_length_multiple working correctly", {
  expect_true(assert_same_length_multiple(1:3, c("a", "b", "c"), list("foo", 1, 0.5)))
  expect_true(assert_same_length_multiple(NULL))

  expect_error(assert_same_length_multiple(1:3, c("a", "b")))
})

#------------------------------------------------
test_that("assert_2d working correctly", {
  expect_true(assert_2d(matrix(NA,3,3)))
  expect_true(assert_2d(matrix(NA,0,0)))
  expect_true(assert_2d(data.frame(1:3,1:3)))

  expect_error(assert_2d(NULL))
  expect_error(assert_2d(5))
  expect_error(assert_2d(array(NA, dim = c(2,3,4))))
})

#------------------------------------------------
test_that("assert_nrow working correctly", {
  expect_true(assert_nrow(matrix(NA,3,3), 3))
  expect_true(assert_nrow(data.frame(1:5,1:5), 5))

  expect_error(assert_nrow(NULL, 3))
  expect_error(assert_nrow(3, NULL))
  expect_error(assert_nrow(NULL, NULL))
  expect_error(assert_nrow(5, 1))
  expect_error(assert_nrow(data.frame(1:5,1:5), 4))
})

#------------------------------------------------
test_that("assert_ncol working correctly", {
  expect_true(assert_ncol(matrix(NA,3,3), 3))
  expect_true(assert_ncol(data.frame(1:5,1:5), 2))

  expect_error(assert_ncol(NULL, 3))
  expect_error(assert_ncol(3, NULL))
  expect_error(assert_ncol(NULL, NULL))
  expect_error(assert_ncol(5, 1))
  expect_error(assert_ncol(data.frame(1:5,1:5), 3))
})

#------------------------------------------------
test_that("assert_dim working correctly", {
  expect_true(assert_dim(matrix(NA,3,3), c(3,3)))
  expect_true(assert_dim(data.frame(1:5,1:5), c(5,2)))

  expect_error(assert_dim(NULL, 3))
  expect_error(assert_dim(3, NULL))
  expect_error(assert_dim(NULL, NULL))
  expect_error(assert_dim(5, 1))
  expect_error(assert_dim(data.frame(1:5,1:5), c(3,2)))
  expect_error(assert_dim(data.frame(1:5,1:5), c(5,3)))
})

#------------------------------------------------
test_that("assert_square_matrix working correctly", {
  expect_true(assert_square_matrix(matrix(0, 3, 3)))

  expect_error(assert_square_matrix(NULL))
  expect_error(assert_square_matrix(matrix(0, 2, 3)))
  expect_error(assert_square_matrix(1))
  expect_error(assert_square_matrix(1:5))
  expect_error(assert_square_matrix("foo"))
})

#------------------------------------------------
test_that("assert_symmetric_matrix working correctly", {
  m0 <- matrix(1:16, 4, 4)
  m1 <- m0 + t(m0)
  expect_true(assert_symmetric_matrix(m1))

  expect_error(assert_symmetric_matrix(NULL))
  expect_error(assert_symmetric_matrix(m0))
  expect_error(assert_symmetric_matrix(1))
  expect_error(assert_symmetric_matrix(1:5))
  expect_error(assert_symmetric_matrix("foo"))
})

#------------------------------------------------
test_that("assert_noduplicates working correctly", {
  expect_true(assert_noduplicates(5))
  expect_true(assert_noduplicates(1:5))
  expect_true(assert_noduplicates(c("foo", "bar")))
  expect_true(assert_noduplicates(NULL))

  expect_error(assert_noduplicates(c(1,1,2)))
  expect_error(assert_noduplicates(c("foo", "bar", "foo")))
})

#------------------------------------------------
test_that("assert_increasing working correctly", {
  expect_true(assert_increasing(1))
  expect_true(assert_increasing(rep(1,5)))
  expect_true(assert_increasing(-5:5))

  expect_error(assert_increasing(NULL))
  expect_error(assert_increasing(5:-5))
  expect_error(assert_increasing("foo"))
})

#------------------------------------------------
test_that("assert_decreasing working correctly", {
  expect_true(assert_decreasing(1))
  expect_true(assert_decreasing(rep(1,5)))
  expect_true(assert_decreasing(5:-5))

  expect_error(assert_decreasing(NULL))
  expect_error(assert_decreasing(-5:5))
  expect_error(assert_decreasing("foo"))
})


#------------------------------------------------
test_that("assert_file_exists working correctly", {
  tf <- tempfile()
  expect_error(assert_file_exists(tf), "file not found")
  writeLines("asdas", tf)
  expect_true(assert_file_exists(tf))
})
