test_that("compare deterministic vaccine model to SEEIR model", {
  pop <- get_population("Angola")
  mm <- get_mixing_matrix("Angola")

  # Reference model
  m1 <- run_deterministic_SEIR_model(
    population = pop$n,
    contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    seed = 1,
    dt = 1,
    replicates = 1,
    seeding_cases = 20
  )
  oi1 <- odin_index(m1$model)

  # Vaccine model, no vaccine
  m2 <- run_vaccine(
    population = pop$n,
    contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    max_vaccine = 0,
    seed = 1,
    framework = "deterministic",
    dt = 1,
    replicates = 1,
    seeding_cases = 20
  )
  oi2 <- odin_index(m2$model)

  # Compare shared compartments
  compare_compartments <- names(oi1)[names(oi1) %in% names(oi2)]
  expect_equal(m1$output[,unlist(oi1[compare_compartments]),],
               m2$output[1:365,unlist(oi2[compare_compartments]),], tol = 0.00001)

  # Check all vaccine-related compartments are 0
  expect_equal(sum(m2$output[,unlist(oi2[c("SVac1", "SVac2", "SVac",
                                           "RVa1", "RVac2", "RVac",
                                           "V1", "V2", "V",
                                           "EVac1", "EVac2", "EVac",
                                           "vaccines")]),]), 0)

  # Check population size is constant at specified level
  expect_equal(format_vaccine(m2, "N", NULL)$value,
               rep(sum(pop$n), 366))

})


test_that("Vaccine on works", {
  pop <- get_population("Angola")
  mm <- get_mixing_matrix("Angola")

  # Vaccine model 100% efficacy against infection
  m1 <- run_vaccine(
    population = pop$n,
    contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    max_vaccine = 10000,
    seed = 1,
    framework = "deterministic",
    dt = 1,
    replicates = 1,
    seeding_cases = 20
  )

  # Check individuals reaching V
  expect_gt(sum(format_vaccine(m1, "V", NULL)$value), 0)

  # Check population size is constant at specified level
  expect_equal(format_vaccine(m1, "N", NULL)$value,
               rep(sum(pop$n), 366))
})


test_that("Age targeting works", {
  pop <- get_population("Angola")
  mm <- get_mixing_matrix("Angola")

  # Vaccine model 100% efficacy against infection yougest age group
  m1 <- run_vaccine(
    population = pop$n,
    contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    max_vaccine = 10000,
    vaccination_target = c(1, rep(0, 16)),
    seed = 1,
    framework = "deterministic",
    dt = 1,
    replicates = 1,
    seeding_cases = 20
  )

  # Check individuals in youngest age group reaching V
  age_v <- format_vaccine(m1, "V", NULL, reduce_age = FALSE)
  expect_gt(sum(dplyr::filter(age_v, age_index == 1)$value), 0)
  expect_equal(sum(dplyr::filter(age_v, age_index != 1)$value), 0)
})

test_that("Time-varying works", {
  pop <- get_population("Angola")
  mm <- get_mixing_matrix("Angola")

  # Vaccine model time varying vaccine
  m1 <- run_vaccine(
    population = pop$n,
    contact_matrix_set = mm,
    hosp_bed_capacity = 100000,
    ICU_bed_capacity = 1000000,
    day_return = TRUE,
    dur_R = Inf,
    max_vaccine = c(0, 1000, 0),
    tt_vaccine = c(0, 100, 200),
    seed = 1,
    framework = "deterministic",
    dt = 1,
    replicates = 1,
    seeding_cases = 20
  )

  # Check individuals in youngest age group reaching V
  t_v <- format_vaccine(m1, "vaccines", NULL)
  expect_equal(sum(dplyr::filter(t_v, t < 100)$value), 0)
  expect_gt(sum(dplyr::filter(t_v, t >= 100, t <200)$value), 0)
  expect_equal(sum(dplyr::filter(t_v, t >= 201)$value), 0)
})
