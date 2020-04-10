test_that("run works", {
  pop = get_population("Afghanistan", simple_SEIR = TRUE)

  set.seed(123)
  r1 <- run_simple_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        contact_matrix_set=contact_matrices[[1]])
  expect_type(r1$output, "list")
  n <- apply(r1$output$S[,,1], 1, sum) + apply(r1$output$E1[,,1], 1, sum) +
    apply(r1$output$E2[,,1], 1, sum) + apply(r1$output$I[,,1], 1, sum) +
    apply(r1$output$R[,,1], 1, sum)
  expect_true(all(n == sum(pop$n)))
  expect_equal(dim(r1$output$S), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$E1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$E2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$I), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$R), c(100 / 1, length(pop$n), 10))

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
  expect_gt(sum(r3$output$I), sum(r2$output$I))

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

  pop = get_population("Afghanistan")

  set.seed(123)
  expect_error(r1 <- run_explicit_SEEIR_model(dt = 1,
                                 R0 = 2,
                                 time_period = 100,
                                 replicates = 10,
                                 contact_matrix_set=contact_matrices[[1]]),
               "User must provide either the country being simulated ")

  r0 <- run_explicit_SEEIR_model(country = "Afghanistan",
                                              dt = 1,
                                              R0 = 2,
                                              time_period = 100,
                                              replicates = 10)
  expect_type(r0$output, "list")


  set.seed(123)
  r1 <- run_explicit_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        contact_matrix_set=contact_matrices[[1]])
  expect_type(r1$output, "list")

  vars <- names(r1$output)[grepl("^[[:upper:]]+$", substr(names(r1$output), 1, 1))]
  n <- rowSums(do.call(cbind, lapply(r1$output[vars], function(x) {apply(x[,,1],1,sum)})))

  # dim and pop size checks
  expect_true(all(n == sum(pop$n)))
  expect_equal(dim(r1$output$S), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$E1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$E2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMild), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$ICase1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$ICase2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxGetLive1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxGetLive2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxGetDie1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxGetDie2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxNotGetLive1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxNotGetLive2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxNotGetDie1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IOxNotGetDie2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVGetLive1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVGetLive2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVGetDie1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVGetDie2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVNotGetLive1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVNotGetLive2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVNotGetDie1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IMVNotGetDie2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IRec1), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$IRec2), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$R), c(100 / 1, length(pop$n), 10))
  expect_equal(dim(r1$output$D), c(100 / 1, length(pop$n), 10))

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
  expect_gt(sum(r3$output$IMild), sum(r2$output$Imild))

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
                                                  contact_matrices[[2]]),
                        tt_contact_matrix = c(0, 50))
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
                                output_transform = TRUE,
                                replicates = 1)

  vars <- names(r1$output)[grepl("^[[:upper:]]+$", substr(names(r1$output), 1, 1))]
  for(i in vars) {
  expect_equal(sum(is.na(colSums(r$output[[i]][,,1]))), 0)
  }

  for(i in vars) {
    expect_equal(sum(r$output[[i]][,,1] < 0), 0)
  }

  expect_error(r <- run_explicit_SEEIR_model(population = population,
                                contact_matrix_set = contact_matrix,
                                R0 = 2.5,
                                time_period = 730,
                                dt = 1,
                                output_transform = TRUE,
                                prob_non_severe_death_no_treatment = rep(1.1,17),
                                replicates = 1),
               "prob_non_severe_death_no_treatment must be less than or equal to 1")

})
