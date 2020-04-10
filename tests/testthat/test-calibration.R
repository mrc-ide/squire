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

  # correct format
  df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                     deaths = rev(c(1, 1, 2, 3, 4, 6)))

  # check on correct data frame format
  df_wrong <- df
  names(df_wrong)[2] <- "DEATHS"
  expect_error(out <- calibrate(df_wrong, "India"),
               "data does not contain a date and/or a deaths column")

  # missing deaths
  expect_error(df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                                       cases = c(6, 7, 0, 0, NA, 1)),
               "Deaths is NULL. If date is provided, deaths must be provided")


  # correct format
  expect_error(df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                          deaths = rev(c(1, 1, 2, 3, 4, 6)),
                          cases = c(6, 7, 0, 0, NA, 1)),
               "cases.* must be decreasing")

  # correct format
  expect_error(df <- death_data_format(date = c(Sys.Date(), Sys.Date() - (1:5)),
                                       deaths = rev(c(1, 6, 2, 3, 4, 6)),
                                       cases = c(7, 6, 0, 0, NA, 0)),
               "deaths.* must be decreasing")


  # run caliibrate
  replicates <- 10
  set.seed(123)
  out <- calibrate(df, "India", parse_output = FALSE, replicates = replicates)
  index <- odin_index(out$model)
  deaths <- vapply(seq_len(replicates), function(x) {
    rowSums(out$output[,index$D,x])
  }, FUN.VALUE = numeric(nrow(out$output)))

  # are all deaths today greater than the deaths reported
  expect_true(all(deaths[which(out$date == Sys.Date())] >= df$deaths[1]))

  # are all deaths yesteday less than the deaths today
  expect_true(all(deaths[which(out$date == (Sys.Date()-1))] < df$deaths[1]))


  # test the parsing
  date <- out$date
  out$date <- NULL
  expect_error(get <- calibrate_output_parsing(out), "r needs date element")

  out$date <- date
  get <- calibrate_output_parsing(out)
  expect_equal(names(get), c("df", "data", "parameters"))
  expect_equal(names(get$df), c("date", "replicate", "variable", "value"))
  expect_equal(as.character(unique(get$df$variable)),
               c("mild_cases","hospital_cases","deaths","icu","hospital_bed"))

  # check its the same if we parse directly
  set.seed(123)
  get2 <- calibrate(df, "India", parse_output = TRUE, replicates = replicates)
  identical(get, get2)

})
