test_that("population getter works", {
  expect_error(get_population("moon"))
  expect_error(get_population(3))
  expect_error(get_population(iso3c="moon"))
  expect_error(get_population(iso3c=4))
  out <- get_population("Angola")
  out2 <- get_population(iso3c="AGO")
  expect_type(out, "list")
  expect_equal(nrow(out), 17)
  expect_equal(ncol(out), 5)
  expect_named(out, c("country", "age_group", "n", "matrix", "iso3c"))
  expect_equal(out, out2)
})

test_that("lmic getter works", {
  expect_vector(get_lmic_countries())
  expect_true("Zambia" %in% get_lmic_countries())
  expect_false("France" %in% get_lmic_countries())
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

  out <- get_healthcare_capacity("Mali")
  expect_is(out, "list")
  expect_named(out, c("hosp_beds", "ICU_beds"))
  expect_is(out$hosp_beds, "numeric")
  expect_is(out$ICU_beds, "numeric")

  expect_error(get_healthcare_capacity("Reunion"))
})
