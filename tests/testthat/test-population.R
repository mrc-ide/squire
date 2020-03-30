test_that("population getter works", {
  expect_error(get_population("moon"))
  out <- get_population("Angola")
  expect_type(out, "list")
  expect_equal(nrow(out), 16)
  expect_equal(ncol(out), 3)
  expect_named(out, c("country", "age_group", "n"))
})
