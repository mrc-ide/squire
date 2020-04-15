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

test_that("js and R versions agree for full model", {
  prob_hosp <- c(0.000744192, 0.000634166,0.001171109, 0.002394593, 0.005346437 ,
    0.010289885, 0.016234604, 0.023349169, 0.028944623, 0.038607042 ,
    0.057734879, 0.072422135, 0.101602458, 0.116979814, 0.146099064,
    0.176634654 ,0.180000000)
  prob_severe <- c(0.05022296,	0.05022296,	0.05022296,	0.05022296,	0.05022296,
    0.05022296,	0.05022296,	0.053214942, 0.05974426,	0.074602879,
    0.103612417, 0.149427991, 0.223777304,	0.306985918,
    0.385779555, 0.461217861, 0.709444444)
  prob_non_severe_death_treatment <- c(
    0.0125702,	0.0125702,	0.0125702,	0.0125702,
    0.0125702,	0.0125702,	0.0125702,	0.013361147,
    0.015104687,	0.019164124,	0.027477519,	0.041762108,
    0.068531658,	0.105302319,	0.149305732,	0.20349534,	0.5804312)
  prob_non_severe_death_no_treatment <- vapply(
    prob_non_severe_death_treatment * 2, min, numeric(1), 1)
  prob_severe_death_treatment <- rep(0.5, length(prob_hosp))
  prob_severe_death_no_treatment <- rep(0.95, length(prob_hosp))

  # Setting Up Model Parameters
  population <- squire::get_population("United Kingdom")
  population <- population$n
  m <- squire::get_mixing_matrix("United Kingdom")
  m <- squire:::process_contact_matrix_scaled_age(m, population)
  dur_R <- 2.09
  dur_hosp <- 5
  beta <- squire::beta_est_explicit(dur_R, dur_hosp, prob_hosp, m, 2)
  mm <- t(t(m) / population)
  mix_mat_set <- aperm(array(c(mm, mm, mm), dim = c(dim(mm), 3)), c(3, 1, 2))

  pars <- list(N_age = length(population),
               S_0 = population,
               E1_0 = rep(0, length(population)),
               E2_0 = rep(0, length(population)),
               IMild_0 = rep(1, length(population)),
               ICase1_0 = rep(1, length(population)),
               ICase2_0 = rep(1, length(population)),
               IOxGetLive1_0 = rep(0, length(population)),
               IOxGetLive2_0 = rep(0, length(population)),
               IOxGetDie1_0 = rep(0, length(population)),
               IOxGetDie2_0 = rep(0, length(population)),
               IOxNotGetLive1_0 = rep(0, length(population)),
               IOxNotGetLive2_0 = rep(0, length(population)),
               IOxNotGetDie1_0 = rep(0, length(population)),
               IOxNotGetDie2_0 = rep(0, length(population)),
               IMVGetLive1_0 = rep(0, length(population)),
               IMVGetLive2_0 = rep(0, length(population)),
               IMVGetDie1_0 = rep(0, length(population)),
               IMVGetDie2_0 = rep(0, length(population)),
               IMVNotGetLive1_0 = rep(0, length(population)),
               IMVNotGetLive2_0 = rep(0, length(population)),
               IMVNotGetDie1_0 = rep(0, length(population)),
               IMVNotGetDie2_0 = rep(0, length(population)),
               IRec1_0 = rep(0, length(population)),
               IRec2_0 = rep(0, length(population)),
               R_0 = rep(0, length(population)),
               D_0 = rep(0, length(population)),
               gamma_E = (2 * 1/4.58),
               gamma_R = (1/dur_R),
               gamma_hosp = (2 * 1/dur_hosp),
               gamma_get_ox_survive = (2 * 1/6),
               gamma_get_ox_die = (2 * 1/3.5),
               gamma_not_get_ox_survive = (2 * 1/9),
               gamma_not_get_ox_die = (0.5 * 2 * 1/9),
               gamma_get_mv_survive = (2 * 1/5.5),
               gamma_get_mv_die = (2 * 1/4),
               gamma_not_get_mv_survive = (2 * 1/12),
               gamma_not_get_mv_die = (2 * 1/1),
               gamma_rec = (2 * 1/6),
               prob_hosp = prob_hosp,
               prob_severe = prob_severe,
               prob_non_severe_death_treatment = prob_non_severe_death_treatment,
               prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
               prob_severe_death_treatment = prob_severe_death_treatment,
               prob_severe_death_no_treatment = prob_severe_death_no_treatment,
               p_dist = rep(1, length(population)),
               hosp_bed_capacity = 10000000000,
               ICU_bed_capacity = 10000000000,
               tt_matrix = c(0, 50, 100),
               mix_mat_set = mix_mat_set,
               tt_beta = c(0, 50, 200),
               beta_set = c(beta, beta/2, beta))

  path <- system.file("odin/explicit_SEIR_deterministic.R",
                      package = "squire", mustWork = TRUE)
  gen_js <- odin.js::odin_js(path)
  mod_js <- gen_js(user = pars)
  t <- seq(from = 1, to = 250)

  mod_r <- squire:::explicit_SEIR_deterministic(user = pars)

  ## Awkward syntax here to drop the deSolve additional information
  ## off of the matrix (mod_r) so that it's easier to compare the
  ## numbers with the js version
  expect_equivalent(
    mod_js$run(t),
    mod_r$run(t)[],
    tolerance = 1e-6)
})
