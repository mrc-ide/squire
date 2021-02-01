test_that("beta input check work", {
  mm <- matrix(runif(4), ncol = 2)
  expect_error(beta_est_simple("A", mm, 1),
               "duration_infectiousness must be a positive numeric value")
  expect_error(beta_est_simple(1:2, mm, 1),
               "duration_infectiousness must be of length = 1")
  expect_error(beta_est_simple(-1, mm, 1),
               "duration_infectiousness must be a positive numeric value")

  expect_error(beta_est_simple(1, mm, "A"),
               "R0 must be a positive numeric value")
  expect_error(beta_est_simple(1, mm, -1),
               "R0 must be a positive numeric value")

  expect_error(beta_est_simple(1, "A", 1),
               "mixing_matrix must be a matrix")
  expect_error(beta_est_simple(1, data.frame(a = 1:2, b = 1:2), 1),
               "mixing_matrix must be a matrix")
})


test_that("beta_explicit input check work", {
  mm <- matrix(runif(4), ncol = 2)
  mm_na <- mm
  mm_na[1] <- NA

  expect_error(beta_est_explicit(dur_IMild = 0, dur_ICase = 5,
                                 prob_hosp = c(0.2,0.1),
                                 mixing_matrix = mm, R0 = 2),
               "dur_IMild must be greater than zero")

  expect_error(beta_est_explicit(dur_IMild = 2, dur_ICase = 0,
                                 prob_hosp = c(0.2,0.1),
                                 mixing_matrix = mm, R0 = 2),
               "dur_ICase must be greater than zero")

  expect_error(beta_est_explicit(dur_IMild = c(2,2), dur_ICase = 5,
                                 prob_hosp = c(0.2,0.1),
                                 mixing_matrix = mm, R0 = 2),
               "dur_IMild must be of length 1")

  expect_error(beta_est_explicit(dur_IMild = 2, dur_ICase = 5,
                                 prob_hosp = c(0.2,NA),
                                 mixing_matrix = mm, R0 = 2),
               "prob_hosp must not contain NAs")

  expect_error(beta_est_explicit(dur_IMild = 2, dur_ICase = 5,
                                 prob_hosp = c(0.2,0.2),
                                 mixing_matrix = mm_na, R0 = 2),
               "mixing_matrix must not contain NAs")

})


test_that("best_est works for both models", {

  mod_simp <- simple_model()
  beta <- beta_est(squire_model = mod_simp,
                   model_params = mod_simp$parameter_func(
                     population = get_population("Angola", simple_SEIR = TRUE)$n,
                     contact_matrix_set = contact_matrices[1]),
                   R0 = 3)
  expect_true(beta - 0.1095709 < 0.001)

  mod_exp <- explicit_model()
  beta <- beta_est(squire_model = mod_exp,
                   model_params = mod_exp$parameter_func("Angola"),
                   R0 = 3)
  expect_true(beta - 0.1254887 < 0.001)

})


test_that("best_est works for apothecary", {

  mod_exp <- explicit_model()
  class(mod_exp) <- c("apothecary_model", "squire_model")
  model_params <- mod_exp$parameter_func("Angola")
  model_params$gamma_IAsymp <- 1
  model_params$gamma_IMild <- 2
  model_params$gamma_ICase <- 3
  model_params$rel_inf_asymp <- 4
  model_params$rel_inf_mild <- 5
  model_params$prob_asymp <- 6
  model_params$prob_hosp <- 7
  mat <- process_contact_matrix_scaled_age(model_params$contact_matrix_set[[1]],
                                           model_params$population)

  # mock out the function as we don't have it
  mod_exp$generate_beta_func <- mockery::mock()
  mockery::expect_args(mod_exp$generate_beta_func,
                       n = 1,
                       dur_IAsymp = 1/model_params$gamma_IAsymp,
                       dur_IMild = 1/model_params$gamma_IMild,
                       dur_ICase = 2/model_params$gamma_ICase,
                       rel_inf_asymp = model_params$rel_inf_asymp,
                       rel_inf_mild = model_params$rel_inf_mild,
                       prob_asymp = model_params$prob_asymp,
                       prob_hosp = model_params$prob_hosp,
                       mixing_matrix = mat,
                       R0 = 3)

})

test_that("best_est works for nimue", {

  mod_exp <- deterministic_model()
  class(mod_exp) <- c("nimue_model", "squire_model")
  model_params <- mod_exp$parameter_func("Angola")
  model_params$rel_infectiousness <- rep(1,17)
  mat <- process_contact_matrix_scaled_age(model_params$contact_matrix_set[[1]],
                                           model_params$population)

  # mock out the function as we don't have it
  mod_exp$generate_beta_func <- mockery::mock()

  beta <- beta_est(squire_model = mod_exp,
                   model_params = model_params,
                   R0 = 3)


  expect_true(mockery::expect_args(mod_exp$generate_beta_func,
                       n = 1,
                       dur_IMild = 1/model_params$gamma_IMild,
                       dur_ICase = 2/model_params$gamma_ICase,
                       prob_hosp = model_params$prob_hosp,
                       rel_infectiousness = model_params$rel_infectiousness,
                       mixing_matrix = mat,
                       R0 = 3))

})
