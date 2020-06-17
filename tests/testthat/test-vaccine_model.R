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
    max_vaccine = 0,
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
  # Check no vaccines have been distributed
  vr1 <- sum(m1$output[,odin_index(m1$model)[["VRec"]],1])
  expect_equal(vr1, 0)

  # Vaccine model, loss of natural immunity, no vaccination
  m2 <-run_deterministic_SEIR_vaccine_model(
    population = pop$n,contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = 20,
    max_vaccine = 0,
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
    max_vaccine = 10,
    seed = 1
  )
  o3 <- format_deterministic_vaccine_output(m3)
  v3 <- o3[o3$compartment == "V", "value"]
  # Check people are entering vaccinated compartment
  expect_gt(sum(v3), 0)
  i3 <- unlist(odin_index(m3$model)[2:32])
  expect_equal(rowSums(m3$output[,i3,1]), rep(sum(pop$n), 365))
  # Check vaccine distribution >0 and  <= max
  vr3 <- diff(rowSums(m3$output[,odin_index(m3$model)[["VRec"]],1]))
  expect_gt(max(vr3), 0)
  expect_lte(max(vr3), 10)

  # Vaccine model, no loss of natural immunity, vaccination time-varying
  m4 <- run_deterministic_SEIR_vaccine_model(
    population = pop$n,contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    max_vaccine = c(0, 10),
    tt_vaccine = c(0, 100),
    seed = 1
  )
  # Check vaccine distribution >0 and  <= max
  vr4 <- diff(rowSums(m4$output[,odin_index(m4$model)[["VRec"]],1]))
  expect_equal(max(vr4[1:98]), 0)
  expect_lte(max(vr4), 10)
  expect_gt(max(vr4), 0)

  # Vaccine model, no loss of natural immunity, vaccination age-targeted
  m5 <- run_deterministic_SEIR_vaccine_model(
    population = pop$n,contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    max_vaccine = 10,
    vaccination_target = c(1, rep(0, 16)),
    seed = 1
  )
  vr5 <- colSums(m5$output[,odin_index(m5$model)[["VRec"]],1])
  expect_gt(vr5[1], 0)
  expect_equal(max(vr5[-1]), 0)

})
