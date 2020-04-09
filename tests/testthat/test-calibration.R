context("calibration")

#------------------------------------------------
test_that("death_data_format work", {

  # correct format
  df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                     deaths = rev(c(1, 1, 2, 3, 4, 6)))
  expect_is(df, "data.frame")
  expect_identical(names(df), c("date", "deaths", "cases"))

  # incorrect format
  expect_error(df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                     deaths = (c(1, 1, 2, 3, 4, 6))),
               regexp = "must be decreasing")

  # syntthetic
  set.seed(123)
  df <- death_data_format()
  expect_is(df, "data.frame")
  expect_identical(names(df), c("date", "deaths", "cases"))

})


#------------------------------------------------
test_that("calibrate works", {

  # # correct format
  # df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
  #                    deaths = rev(c(1, 1, 2, 3, 4, 6)))
  #
  # # check on correct data frame format
  # df_wrong <- df
  # names(df_wrong)[2] <- "DEATHS"
  # expect_error(out <- calibrate(df_wrong, "India"),
  #              "data does not contain a date and/or a deaths column")
  #
  # # run caliibrate
  # out <- calibrate(df, "India", replicates = 100)
  # out2 <- dplyr::group_by(out[out$name=="D",], t, replicate, date) %>%
  #   dplyr::summarise(value=sum(value))
  #
  # # are all deaths today greater than the deaths reported
  # expect_true(all(out2[out2$date == Sys.Date(),]$value >= df$deaths[1]))
  #
  # # are all deaths yesteday less than the deaths today
  # expect_true(all(out2[out2$date == Sys.Date()-1,]$value < df$deaths[1]))

})
