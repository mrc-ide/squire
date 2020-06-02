context("align")

#------------------------------------------------
test_that("align works", {
  t1 <- align(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 2)
  expect_type(t1, "list")
  expect_s3_class(t1, "squire_simulation")
  expect_true(dim(t1$output)[3] == 2)
  expect_error(align(country = "Angola", deaths = 5,
                         reporting_fraction = 0.5,
                         time_period = 120, seeding_age_groups = "wrong"),
               "inputted age groups not valid")
  expect_error(align("Angola", -1, 0.5))
  expect_error(align("Angola", 5, -0.1))
  expect_error(align("Angola", 5, 1.1))
  expect_warning(expect_s3_class(plot(t1, "deaths", x_var = "date", date_0 = Sys.Date()), "gg"))
  expect_warning(expect_s3_class(plot(t1, "deaths"), "gg"))

})


#------------------------------------------------
test_that("align R0 works", {
  set.seed(123)
  t1 <- align(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3,4))
  expect_true(all(c(3, 4) %in% t1$parameters$R0_scan))

  set.seed(123)
  t1 <- align(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3))
  expect_true(all(t1$parameters$R0_scan == 3))

  t1 <- align(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3))
  expect_true(all(t1$parameters$R0_scan == 3))

  t1 <- align(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  time_period = 120, replicates = 10, R0_scan = c(3,4,5),
                  R0=c(3,2,1), tt_R0=c(0,30,60))
  expect_true(all(t1$parameters$R0_scan %in% 3:5))



})
