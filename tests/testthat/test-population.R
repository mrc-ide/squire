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

test_that("parse_duration works", {
  out <- parse_durations(walker_params = FALSE)
  expect_type(out, "list")
  out2 <- parse_durations(walker_params = TRUE)
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


test_that("durations", {

  expect_true(is.list(squire:::default_durations()))

  expect_true(all(c("prob_severe", "prob_severe_death_treatment") %in%
                    names(squire:::default_probs())))

  expect_true(all(c("dur_rec", "dur_get_ox_survive") %in%
                    names(squire:::default_durations())))

})



test_that("population n are all integers", {

  int_log <- vapply(population$n, is.integer, logical(1))
  expect_true(sum(int_log) == length(population$n))

})

test_that("parse_duration mv_survive", {
  out <- parse_durations(walker_params = FALSE)
  out$dur_not_get_mv_survive
  pars <- parameters_explicit_SEEIR("Iran")

  expect_true(pars$gamma_not_get_mv_survive == 2 * 1/(out$dur_not_get_mv_survive))
  expect_true(pars$gamma_get_mv_survive == 2 * 1/(out$dur_get_mv_survive))
  expect_true(pars$gamma_not_get_ox_survive == 2 * 1/(out$dur_not_get_ox_survive))
  expect_true(pars$gamma_get_ox_survive == 2 * 1/(out$dur_not_get_ox_survive))
  expect_true(pars$gamma_not_get_mv_die == 2 * 1/(out$dur_not_get_mv_die))
  expect_true(pars$gamma_get_mv_die == 2 * 1/(out$dur_get_mv_die))
  expect_true(pars$gamma_not_get_ox_die == 2 * 1/(out$dur_not_get_ox_die))
  expect_true(pars$gamma_get_ox_die == 2 * 1/(out$dur_get_ox_die))
  expect_true(pars$gamma_E == 2 * 1/(out$dur_E))
  expect_true(pars$gamma_IMild == 1/(out$dur_IMild))
  expect_true(pars$gamma_ICase == 2 * 1/(out$dur_ICase))
  expect_true(pars$gamma_rec == 2 * 1/(out$dur_rec))
  expect_true(pars$gamma_R == 2 * 1/(out$dur_R))

})
