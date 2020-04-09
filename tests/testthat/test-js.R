context("js")

test_that("js and R versions agree for basic model", {
  S0 <- c(100000, 100000)
  mixing_matrix <- matrix(c(5, 2, 2, 5), nrow = 2, byrow = TRUE)
  m <- t(t(mixing_matrix) / S0)
  pars <- list(S0 = c(100000, 1000000),
               E0 = c(0, 0),
               I_mild0 = c(100, 100),
               I_hosp0 = c(100, 100),
               I_ICU0 = c(100, 100),
               R0 = c(0, 0),
               D0 = c(0, 0),
               gamma = 0.3,
               sigma = 0.3,
               mu = 0.01,
               p_mild = c(0.33, 0.33),
               p_hosp = c(0.33, 0.33),
               p_ICU = c(0.34, 0.34),
               beta_1 = 0.1,
               beta_2 = 0.1,
               m = m)

  path <- system.file("odin/less_basic_model_for_js.R",
                      package = "squire", mustWork = TRUE)
  gen_js <- odin.js::odin_js(path)
  mod_js <- gen_js(user = pars)
  t <- seq(from = 1, to = 200)

  mod_r <- squire:::less_basic_model_for_js(user = pars)

  ## Awkward syntax here to drop the deSolve additional information
  ## off of the matrix (mod_r) so that it's easier to compare the
  ## numbers with the js version
  expect_equivalent(
    mod_js$run(t),
    mod_r$run(t)[],
    tolerance = 1e-6)
})
