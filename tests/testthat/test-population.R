test_that("population getter works", {
  expect_error(get_population("moon"))
  out <- get_population("Angola")
  expect_type(out, "list")
  expect_equal(nrow(out), 16)
  expect_equal(ncol(out), 5)
  expect_named(out, c("country", "age_group", "n", "matrix", "prop_80_plus"))
})


test_that("population getter works", {
  expect_error(get_mixing_matrix("moon"))
  out <- get_mixing_matrix("Angola")
  expect_is(out, "matrix")
  expect_equal(nrow(out), 16)
  expect_equal(ncol(out), 16)
})
