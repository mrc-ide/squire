context("plotting")

#------------------------------------------------
test_that("cases and healthcare plotting works", {

  # correct format
  df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                          deaths = rev(c(1, 1, 2, 3, 4, 6)))

  # calibrate
  set.seed(123)
  get2 <- calibrate(df, "India", parse_output = TRUE, replicates = 10)

  # do we get ggplots from both
  gg_cases <- plot(get2, what = "cases")
  expect_is(gg_cases, "ggplot")

  gg_healthcare <- plot(get2, what = "healthcare")
  expect_is(gg_healthcare, "ggplot")

  # and these are different
  expect_false(identical(gg_cases, gg_healthcare))

  # and does it fail if we put gibberish in
  expect_error(plot(get2, what = "trash"), "must be one of")

})
