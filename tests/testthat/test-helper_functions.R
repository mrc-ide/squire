context("helper-functions")

context("objects")

test_that("object methods ", {

  # default run
  r <- run_explicit_SEEIR_model("India", dt = 0.5)
  out <- extract_all_outputs(r)

  expect_is(out, "list")
  expect_equal(names(out), c("t", "S", "E", "IMild", "ICase", "IOx", "IMV",
                             "IRec", "R", "D"))

  r2 <- run_explicit_SEEIR_model("India", output_transform = FALSE, dt = 0.5)
  expect_error(out <- extract_all_outputs(r2),
               "Plotting does not work with untransformed output")

  # get some specifics
  out <- extract_specific_output(r, "infections")
  expect_equal(dim(out), c(730, 17, 10))
  out <- extract_specific_output(r, "deaths")
  expect_equal(dim(out), c(730, 17, 10))
  out <- extract_specific_output(r, "hospital")
  expect_equal(dim(out), c(730, 17, 10))
  out <- extract_specific_output(r, "ICU")
  expect_equal(dim(out), c(730, 17, 10))


  expect_error(out <- extract_specific_output(r, "trash"),
               "output_required must equal one of infections, deaths, hospital")


})
