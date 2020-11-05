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

test_that("elderly population getter works", {
  expect_error(get_elderly_population("moon"))
  expect_error(get_elderly_population(3))
  expect_error(get_elderly_population(iso3c="moon"))
  expect_error(get_elderly_population(iso3c=4))
  out <- get_elderly_population("Angola")
  out2 <- get_elderly_population(iso3c="AGO")
  expect_type(out, "list")
  expect_equal(nrow(out), 3)
  expect_equal(ncol(out), 5)
  expect_named(out, c("country", "age_group", "n", "matrix", "iso3c"))
  expect_equal(out, out2)
})

test_that("parse_country_severity works", {
  expect_error(parse_country_severity("moon"))
  expect_error(parse_country_severity(3))
  out <- parse_country_severity("Angola")
  expect_type(out, "list")
  out2 <- parse_country_severity(walker_params = TRUE)
  expect_type(out2, "list")
  expect_error(parse_country_severity(walker_params = "bloop"))
  out3 <- parse_country_severity()
  expect_type(out3, "list")
})

test_that("parse_hospital_duration works", {
  expect_error(parse_hospital_duration("moon"))
  out <- parse_country_severity(walker = FALSE)
  expect_type(out, "list")
  out2 <- parse_country_severity(walker = TRUE)
  expect_type(out2, "list")
})

test_that("lmic getter works", {
  expect_vector(get_lmic_countries())
  expect_true("Zambia" %in% get_lmic_countries())
  expect_false("France" %in% get_lmic_countries())
})

test_that("mixing matrix getter works", {
  expect_error(get_mixing_matrix("moon"))
  out <- get_mixing_matrix("Angola")
  out <- get_mixing_matrix(iso3c = "AGO")
  expect_message(out <- get_mixing_matrix(country = "Angola", iso3c = "AGO"))
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

test_that("ICU and hosp bed direct work", {

  expect_error(get_ICU_bed_capacity("moon"))
  out <- get_ICU_bed_capacity("Angola")
  expect_equal(out, 604L)

  expect_error(get_hosp_bed_capacity("moon"))
  out <- get_hosp_bed_capacity("Angola")
  expect_equal(out, 30211L)

})
