context("plotting")

#------------------------------------------------
test_that("cases and healthcare plotting works", {

  # correct format
  df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                          deaths = rev(c(1, 1, 2, 3, 4, 6)),
                          cases = c(740,700,400,300,200,110,58))

  t1 <- calibrate("Angola", 6, 1, time_period = 120, max_seeding_cases = 5,
                  min_seeding_cases = 1, replicates = 10)
  o1 <- calibrate_output_parsing(t1, date_0 = Sys.Date())
  o2 <- calibrate_output_parsing(t1)


  # calibrate
  set.seed(123)

  # do we get ggplots from both
  expect_s3_class(plot_calibration_healthcare(df = o1, data = df), "gg")
  expect_s3_class(plot_calibration_healthcare_barplot(df = o1, data = df), "gg")
  expect_s3_class(plot_calibration_cases(df = o1, data = df), "gg")
  expect_s3_class(plot_calibration_cases_barplot(df = o1, data = df), "gg")

  x <- death_data_format(date = NULL,
                         deaths = NULL,
                         cases = NULL)
  expect_type(x, "list")


})
