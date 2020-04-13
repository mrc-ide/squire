context("calibration")

#------------------------------------------------
test_that("calibrate works", {
  t1 <- calibrate("Angola", 5, 0.5, time_period = 120)
  expect_type(t1, "list")
  expect_s3_class(t1, "squire_simulation")
  expect_true(dim(t1$output)[3] == 100)
  expect_error(calibrate("Angola", 5, 0.5, time_period = 120, seeding_age_groups = "wrong"),
               "inputted age groups not valid")
  expect_error(calibrate("Angola", -1, 0.5))
  expect_error(calibrate("Angola", 5, -0.1))
  expect_error(calibrate("Angola", 5, 1.1))
})


#------------------------------------------------
test_that("calibrate works", {
  t1 <- calibrate("Angola", 2, 0.5, time_period = 120, max_seeding_cases = 5, min_seeding_cases = 1)
  o1 <- format_output(t1, reduce_age = TRUE, reduce_compartment = TRUE, date_0 = Sys.Date())


})
