test_that("deterministic vaccine model", {

  pop <- get_population("Afghanistan")
  mm <- get_mixing_matrix("Afghanistan")

  # Non-vaccine model run
  m <- run_deterministic_SEIR_model(
    population = pop$n,contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    seed = 1
  )
  m$output <- m$output[,,1, drop = TRUE]
  o <- format_deterministic_output(m)

  # Vaccine model, no loss of natural immunity, no vaccination
    # This should match with the non-vaccine model
  m1 <- run_deterministic_SEIR_vaccine_model(
    population = pop$n,contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    vaccination_rate = rep(0, 17),
    seed = 1
  )
  o1 <- format_deterministic_vaccine_output(m1)

  comps <- unique(o$compartment)
  for(i in seq_along(comps)){
    t1 <- o[o$compartment == comps[i], "value"]
    t2 <- o1[o1$compartment == comps[i], "value"]
    expect_equal(t1, t2)
  }

  # Check total pop is constant and equal to expected
  i1 <- unlist(odin_index(m1$model)[2:32])
  expect_equal(rowSums(m1$output[,i1,1]), rep(sum(pop$n), 365))

  # Vaccine model, loss of natural immunity, no vaccination
  m2 <-run_deterministic_SEIR_vaccine_model(
    population = pop$n,contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = 20,
    vaccination_rate = rep(0, 17),
    seed = 1
  )
  o2 <- format_deterministic_vaccine_output(m2)
  r2 <- o2[o2$compartment == "R", "value"]
  # Check # recovered can fall
  expect_lt(min(diff(r2)), 0)

  # Check total pop is constant and equal to expected
  i2 <- unlist(odin_index(m2$model)[2:32])
  expect_equal(rowSums(m2$output[,i2,1]), rep(sum(pop$n), 365))

  # Vaccine model, no loss of natural immunity, vaccination
  m3 <- run_deterministic_SEIR_vaccine_model(
    population = pop$n,contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    vaccination_rate = rep(0.01, 17),
    seed = 1
  )
  o3 <- format_deterministic_vaccine_output(m3)
  v3 <- o3[o3$compartment == "V", "value"]
  # Check people are entering vaccinated compartment
  expect_gt(sum(v3), 0)

})
