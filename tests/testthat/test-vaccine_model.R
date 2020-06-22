test_that("deterministic vaccine model", {
  pop <- get_population("Angola")
  mm <- get_mixing_matrix("Angola")

  # Non-vaccine model run
  m <- run_deterministic_SEIR_model(
    population = pop$n,
    contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    seed = 1
  )
  oi <- odin_index(m$model)

  # Vaccine model, no loss of natural immunity, no vaccination
  # This should match with the non-vaccine model
  m1 <- run_deterministic_SEIR_vaccine_model(
    population = pop$n,
    contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    max_vaccine = 0,
    seed = 1
  )
  oi1 <- odin_index(m1$model)

  # Check all matrix outputs are equal to original model
  var_check <- names(oi)[names(oi) %in% names(oi1)]
  expect_equal(m$output[,unlist(oi[var_check]),1], m1$output[,unlist(oi1[var_check]),1])

  # Check total population is constant and = population input
  popout <- rowSums(m1$output[,oi1$N,1])
  expect_equal(popout, rep(sum(pop$n), length(popout)))

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
  o2 <- format_vaccine(m2, c("R", "N"))
  r2 <- o2[o2$compartment == "R", "value"]
  # Check # recovered can fall
  expect_lt(min(diff(r2)), 0)
  # Check total population is constant and = population input
  popout2 <- o2[o2$compartment == "N", "value"]
  expect_equal(popout2, rep(sum(pop$n), length(popout2)))

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
  o3 <- format_vaccine(m3, compartments = c("V", "N"))
  v3 <- o3[o3$compartment == "V", "value"]
  # Check people are entering vaccinated compartment
  expect_gt(sum(v3), 0)
  # Check vaccine distribution >0 and  <=max
  vr3 <- diff(o3[o3$compartment == "vaccinated", "value"])
  expect_gt(max(vr3), 0)
  expect_lte(max(diff(vr3)), 10)
  # Check total population is constant and = population input
  popout3 <- o3[o3$compartment == "N", "value"]
  expect_equal(popout3, rep(sum(pop$n), length(popout3)))

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
  o4 <- format_vaccine(m4, compartments = c("V", "N"))
  # Check vaccine distribution >0 and  <= max
  vr4 <- diff(o4[o4$compartment == "vaccinated", "value"])
  expect_equal(max(vr4[1:98]), 0)
  expect_lte(max(vr4), 10)
  expect_gt(max(vr4), 0)
  # Check total population is constant and = population input
  popout4 <- o4[o4$compartment == "N", "value"]
  expect_equal(popout4, rep(sum(pop$n), length(popout4)))

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
  o5 <- format_vaccine(m5, compartments = c("V", "N"), reduce_age = FALSE)
  vr5 <- diff(o5[o5$compartment == "vaccinated" & o5$age_index == 1, "value"])
  vr5b <- diff(o5[o5$compartment == "vaccinated" & o5$age_index != 1, "value"])
  expect_gt(sum(vr5), 0)
  expect_equal(max(vr5b), 0)


  ## Format check

  # Check age-aggregated summary = rowSums of original
  op1 <- format_vaccine(m1, compartments = c("S", "E1", "E2", "E1_vac", "E2_vac", "IMild",
                                             "ICase1", "ICase2", "IOxGetLive1", "IOxGetLive2",
                                             "IOxGetDie1", "IOxGetDie2",
                                             "IOxNotGetLive1", "IOxNotGetLive2",
                                             "IOxNotGetDie1", "IOxNotGetDie2",
                                             "IMVGetLive1", "IMVGetLive2",
                                             "IMVGetDie1", "IMVGetDie2",
                                             "IMVNotGetLive1", "IMVNotGetLive2",
                                             "IMVNotGetDie1", "IMVNotGetDie2"))
  uop1 <- unique(op1$compartment)
  for(i in seq_along(uop1)){
    expect_identical(op1[op1$compartment == uop1[i], "value"],
                     rowSums(m1$output[,oi1[[uop1[i]]],]))
  }

  # Check Compartment combinations are equal to sum of component compartments
  op2 <- format_vaccine(m1, compartments = c("E1", "E2", "E"), summaries = NULL) %>%
    tidyr::pivot_wider(id_cols = c(t, replicate), names_from = compartment, values_from = value) %>%
    dplyr::mutate(Echeck = E1 + E2)
  expect_equal(op2$E, op2$Echeck)

  op3 <- format_vaccine(m1, compartments = c("E1_vac", "E2_vac", "E_vac"), summaries = NULL) %>%
    tidyr::pivot_wider(id_cols = c(t, replicate), names_from = compartment, values_from = value) %>%
    dplyr::mutate(Echeck = E1_vac + E2_vac)
  expect_equal(op3$E_vac, op3$Echeck)

  op4 <- format_vaccine(m1, compartments = c("ICase1", "ICase2", "ICase"), summaries = NULL) %>%
    tidyr::pivot_wider(id_cols = c(t, replicate), names_from = compartment, values_from = value) %>%
    dplyr::mutate(ICasecheck = ICase1 + ICase2)
  expect_equal(op4$ICase, op4$ICasecheck)


})
