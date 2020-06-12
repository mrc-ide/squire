context("js")

test_that("js and R versions agree for basic model", {



  pop <- get_population("Afghanistan")
  m <- get_mixing_matrix("Afghanistan")

  init = data.frame(
    S = pop$n-1,
    E1 = 1,
    E2 = 0,
    IMild = 0,
    ICase1 = 0,
    ICase2 = 0,
    IOxGetLive1 = 0,
    IOxGetLive2 = 0,
    IOxGetDie1 = 0,
    IOxGetDie2 = 0,
    IOxNotGetLive1 = 0,
    IOxNotGetLive2 = 0,
    IOxNotGetDie1 = 0,
    IOxNotGetDie2 = 0,
    IMVGetLive1 = 0,
    IMVGetLive2 = 0,
    IMVGetDie1 = 0,
    IMVGetDie2 = 0,
    IMVNotGetLive1 = 0,
    IMVNotGetLive2 = 0,
    IMVNotGetDie1 = 0,
    IMVNotGetDie2 = 0,
    IRec1 = 0,
    IRec2 = 0,
    R = 0,
    D = 0
  )

  r_result <- run_deterministic_SEIR_model(
    population = pop$n,
    contact_matrix_set = m,
    tt_R0 = c(0, 50),
    R0 = c(3, 3/2),
    time_period = 365,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    init = init
  )

  path <- system.file(
    file.path('odin', 'explicit_SEIR_deterministic.R'),
    package = "squire",
    mustWork = TRUE
  )
  gen_js <- odin.js::odin_js(path)

  js_result <- run_deterministic_SEIR_model(
    population = pop$n,
    contact_matrix_set = m,
    tt_R0 = c(0, 50),
    R0 = c(3, 3/2),
    time_period = 365,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    mod_gen = gen_js,
    init = init
  )

  ## Awkward syntax here to drop the deSolve additional information
  ## off of the matrix (mod_r) so that it's easier to compare the
  ## numbers with the js version
  expect_equivalent(
    js_result$output,
    r_result$output[],
    tolerance = 1e-4)
})
