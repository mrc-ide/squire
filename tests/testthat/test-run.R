test_that("run works", {
  pop = get_population("Afghanistan", simple_SEIR = TRUE)

  set.seed(123)
  r1 <- run_simple_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = 2,
                               time_period = 100,
                               replicates = 10,
                               contact_matrix_set=contact_matrices[[1]])
  expect_type(r1$output, "double")

  o1 <- format_output(r1)
  expect_true(sum(dplyr::filter(o1, t == min(t), replicate == 1)$y) == sum(pop$n))

  expect_equal(sum(o1$compartment == "S"), 100 * 10)
  expect_equal(sum(o1$compartment == "E"), 100 * 10)
  expect_equal(sum(o1$compartment == "I"), 100 * 10)
  expect_equal(sum(o1$compartment == "R"), 100 * 10)

  # Multiple R0
  set.seed(123)
  r2 <- run_simple_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = c(2,2),
                               tt_R0 = c(0, 10),
                               time_period = 100,
                               replicates = 10,
                               contact_matrix_set=contact_matrices[[1]])
  expect_identical(r1$output, r2$output)
  set.seed(123)
  r3 <- run_simple_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = c(2,5),
                               tt_R0 = c(0, 10),
                               time_period = 100,
                               replicates = 10,
                               contact_matrix_set=contact_matrices[[1]])
  o2 <- format_output(r2)
  o3 <- format_output(r3)
  expect_gt(sum(dplyr::filter(o3, compartment == "I")$y),
            sum(dplyr::filter(o2, compartment == "I")$y))

  # Multiple contact matrices
  set.seed(123)
  r4 <- run_simple_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = 2,
                               time_period = 100,
                               replicates = 10,
                               contact_matrix_set = list(contact_matrices[[1]],
                                                         contact_matrices[[1]]),
                               tt_contact_matrix = c(0, 50))
  expect_identical(r1$output, r4$output)

  set.seed(123)
  r5 <- run_simple_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = 2,
                               time_period = 100,
                               replicates = 10,
                               contact_matrix_set = list(contact_matrices[[1]],
                                                         contact_matrices[[2]]),
                               tt_contact_matrix = c(0, 50))
  expect_true(!identical(r1$output, r5$output))

  set.seed(123)
  r6 <- run_simple_SEEIR_model(population = pop$n,
                               dt = 1,
                               R0 = 2,
                               time_period = 100,
                               replicates = 10,
                               contact_matrix_set = list(contact_matrices[[1]]),
                               tt_contact_matrix = c(0, 50))
  expect_true(!identical(r5$output, r6$output))

})

test_that("run explicit works", {
  pop = get_population("Afghanistan", simple_SEIR = FALSE)

  set.seed(123)
  r1 <- run_explicit_SEEIR_model(population = pop$n,
                                 dt = 1,
                                 R0 = 2,
                                 time_period = 100,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])
  expect_type(r1$output, "double")

  o1 <- format_output(r1)
  expect_true(sum(dplyr::filter(o1, t == min(t), replicate == 1)$y) == sum(pop$n))

  uc <- unique(o1$compartment)
  for(i in seq_along(uc)){
    expect_equal(sum(o1$compartment == uc[i]), 100 * 10)
  }
  # Multiple R0
  set.seed(123)
  r2 <- run_explicit_SEEIR_model(population = pop$n,
                                 dt = 1,
                                 R0 = c(2,2),
                                 tt_R0 = c(0, 10),
                                 time_period = 100,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])
  expect_identical(r1$output, r2$output)
  set.seed(123)
  r3 <- run_explicit_SEEIR_model(population = pop$n,
                                 dt = 1,
                                 R0 = c(2,5),
                                 tt_R0 = c(0, 10),
                                 time_period = 100,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]])
  o2 <- format_output(r2)
  o3 <- format_output(r3)
  expect_gt(sum(dplyr::filter(o3, compartment == "n_E2_I")$y),
            sum(dplyr::filter(o2, compartment == "n_E2_I")$y))

  # Multiple contact matrices
  set.seed(123)
  r4 <- run_explicit_SEEIR_model(population = pop$n,
                                 dt = 1,
                                 R0 = 2,
                                 time_period = 100,
                                 replicates = 10,
                                 contact_matrix_set = list(contact_matrices[[1]],
                                                           contact_matrices[[1]]),
                                 tt_contact_matrix = c(0, 50))
  expect_identical(r1$output, r4$output)

  set.seed(123)
  r5 <- run_explicit_SEEIR_model(population = pop$n,
                                 dt = 1,
                                 R0 = 2,
                                 time_period = 100,
                                 replicates = 10,
                                 contact_matrix_set = list(contact_matrices[[1]],
                                                           contact_matrices[[1]]*0.5,
                                                           contact_matrices[[1]]*0.2),
                                 tt_contact_matrix = c(0, 40, 70))
  expect_true(!identical(r1$output, r5$output))

  set.seed(123)
  r6 <- run_explicit_SEEIR_model(population = pop$n,
                                 dt = 1,
                                 R0 = 2,
                                 time_period = 100,
                                 replicates = 10,
                                 contact_matrix_set = list(contact_matrices[[1]]),
                                 tt_contact_matrix = c(0, 50))
  expect_true(!identical(r5$output, r6$output))
  expect_error(run_explicit_SEEIR_model(R0 = 2), "User must provide either the country being simulated or
         both the population size and contact_matrix_set")

  r7 <- run_explicit_SEEIR_model(R0 = 2, country = "Afghanistan")
  expect_type(r7$output, "double")
  expect_null(check_squire(r7))

  expect_error(run_explicit_SEEIR_model(country = "Afghanistan",
                                        hosp_bed_capacity = c(1000, 2000),
                                        tt_hosp_beds = 0))
  expect_error(run_explicit_SEEIR_model(country = "Afghanistan",
                                        ICU_bed_capacity = c(1000, 2000),
                                        tt_ICU_beds = 0))

  r8 <- run_explicit_SEEIR_model(country = "Afghanistan",
                                 ICU_bed_capacity = c(1000, 2000),
                                 tt_ICU_beds = c(0, 100))
  expect_type(r8, "list")


})



test_that("run explicit works when healthsystem capacity is swamped", {

  # Get the population
  pop <- get_population("United Kingdom")
  population <- pop$n

  # Get the mixing matrix
  contact_matrix <- get_mixing_matrix("United Kingdom")


  r <- run_explicit_SEEIR_model(population = population,
                                contact_matrix_set = contact_matrix,
                                R0 = 2.5,
                                time_period = 730,
                                dt = 1,
                                replicates = 1)

  o1 <- format_output(r)
  expect_equal(sum(is.na(o1$y)), 0)
  expect_equal(sum(o1$y < 0), 0)

  expect_error(r <- run_explicit_SEEIR_model(population = population,
                                             contact_matrix_set = contact_matrix,
                                             R0 = 2.5,
                                             time_period = 730,
                                             dt = 1,
                                             prob_non_severe_death_no_treatment = rep(1.1,17),
                                             replicates = 1),
               "prob_non_severe_death_no_treatment must be less than or equal to 1")

})



test_that("health system capacity", {

  set.seed(123)
  icu_cap <- 1000
  bed_cap <- 1e5
  r <- run_explicit_SEEIR_model(country = "United Kingdom",
                                R0 = 2.5,
                                time_period = 200,
                                dt = 1,
                                hosp_bed_capacity = bed_cap,
                                ICU_bed_capacity = icu_cap,
                                replicates = 1)

  icu_occ <- extract_ICU_occ(r)
  hosp_occ <- extract_hospital_occ(r)

  expect_equal(max(icu_occ$y), icu_cap)
  expect_equal(max(hosp_occ$y), bed_cap)
})




test_that("seeding", {

  set.seed(123)
  icu_cap <- 1000
  bed_cap <- 1e5
  r <- run_explicit_SEEIR_model(country = "United Kingdom",
                                R0 = 2.5,
                                time_period =200,
                                seeding_cases = 3,
                                dt = 1,
                                hosp_bed_capacity = bed_cap,
                                ICU_bed_capacity = icu_cap,
                                replicates = 1)
  o <- format_output(r)
  expect_equal(sum(dplyr::filter(o, compartment == "E", t == 1, replicate == 1)$y), 3)
})

test_that("run deterministic parameterises model correctly", {
  pop <- get_population("Afghanistan")
  m <- get_mixing_matrix("Afghanistan")
  output <- run_deterministic_SEIR_model(
    pop$n,
    m,
    c(0, 50),
    c(3, 3/2),
    365,
    100000,
    1000000
  )
  expect_equal(length(output$output[,1]), 365)
  expect_equal(output$parameters$hosp_bed_capacity, 100000)
  expect_equal(output$parameters$ICU_bed_capacity, 1000000)
})


test_that("run day_return", {

  tp <- 10
  r <- run_explicit_SEEIR_model("Angola",replicates = 1, day_return = TRUE, time_period = tp)
  expect_true(nrow(r$output) == tp+1)

})
