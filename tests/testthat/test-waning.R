test_that("waning runs", {

  set.seed(123)
  icu_cap <- 1000
  bed_cap <- 1e5
  r <- run_explicit_SEEIR_model(country = "United Kingdom",
                                R0 = 2.5,
                                time_period = 500,
                                dt = 1,
                                dur_R = 65,
                                hosp_bed_capacity = bed_cap,
                                ICU_bed_capacity = icu_cap,
                                replicates = 1)

  r1 <- run_deterministic_SEIR_model(country = "United Kingdom",
                                R0 = 2.5,
                                time_period = 500,
                                dt = 1,
                                dur_R = 65,
                                hosp_bed_capacity = bed_cap,
                                ICU_bed_capacity = icu_cap,
                                replicates = 1)


  expect_warning(expect_s3_class(plot(r, "infections"), "ggplot"), "<10")
  expect_warning(expect_s3_class(plot(r1, "infections"), "ggplot"), "<10")

  # check that postiive infections are caught
  f <- format_output(r, "infections")
  expect_true(all(f$y>=0))

  f1 <- format_output(r1, "infections")
  expect_true(all(f1$y>=0))


  # REPEAT NO WANING
  set.seed(123)
  icu_cap <- 1000
  bed_cap <- 1e5
  r_inf <- run_explicit_SEEIR_model(country = "United Kingdom",
                                R0 = 2.5,
                                time_period = 200,
                                dt = 1,
                                dur_R = Inf,
                                hosp_bed_capacity = bed_cap,
                                ICU_bed_capacity = icu_cap,
                                replicates = 1)

  r1_inf <- run_deterministic_SEIR_model(country = "United Kingdom",
                                     R0 = 2.5,
                                     time_period = 200,
                                     dt = 1,
                                     dur_R = Inf,
                                     hosp_bed_capacity = bed_cap,
                                     ICU_bed_capacity = icu_cap,
                                     replicates = 1)


  expect_warning(expect_s3_class(plot(r_inf, "infections"), "ggplot"), "<10")
  expect_warning(expect_s3_class(plot(r1_inf, "infections"), "ggplot"), "<10")

  # check that postiive infections are caught
  f_inf <- format_output(r_inf, "infections")
  expect_true(sum(f_inf$y) < sum(f$y))

  f1_inf <- format_output(r1_inf, "infections")
  expect_true(sum(f1_inf$y) < sum(f1$y))

})
