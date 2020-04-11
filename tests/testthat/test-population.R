test_that("population getter works", {
  expect_error(get_population("moon"))
  out <- get_population("Angola")
  expect_type(out, "list")
  expect_equal(nrow(out), 17)
  expect_equal(ncol(out), 4)
  expect_named(out, c("country", "age_group", "n", "matrix"))
})


test_that("mixing matrix getter works", {
  expect_error(get_mixing_matrix("moon"))
  out <- get_mixing_matrix("Angola")
  expect_is(out, "matrix")
  expect_equal(nrow(out), 16)
  expect_equal(ncol(out), 16)
})

test_that("healthcare capacity getter getter works", {
  expect_error(get_healthcare_capacity("moon"))
  out <- get_healthcare_capacity("Angola")
  expect_is(out, "list")
  expect_named(out, c("hosp_beds", "ICU_beds"))
  expect_is(out$hosp_beds, "numeric")
  expect_is(out$ICU_beds, "numeric")
})
