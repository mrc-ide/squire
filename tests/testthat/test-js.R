context("js")

test_that("js and R versions agree for basic model", {
  pop <- get_population("Afghanistan")
  m <- get_mixing_matrix("Afghanistan")
  r_result <- run_deterministic_SEIR_model(
    pop$n,
    m,
    c(0, 50),
    c(3, 3/2),
    365,
    100000,
    1000000
  )

  path <- system.file(
    file.path('odin', 'explicit_SEIR_deterministic.R'),
    package = "squire",
    mustWork = TRUE
  )
  gen_js <- odin.js::odin_js(path)

  js_result <- run_deterministic_SEIR_model(
    pop$n,
    m,
    c(0, 50),
    c(3, 3/2),
    365,
    100000,
    1000000,
    mod_gen = gen_js
  )

  ## Awkward syntax here to drop the deSolve additional information
  ## off of the matrix (mod_r) so that it's easier to compare the
  ## numbers with the js version
  expect_equivalent(
    js_result$output,
    r_result$output[],
    tolerance = 1e-4)
})
