test_that("output df works", {
  pop = get_population("Afghanistan")

  set.seed(123)
  r1 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set=contact_matrices[[1]])

  o1 <- long_output(r1$output)
  expect_type(o1, "list")
  expect_equal(nrow(o1), 100 * 10 * length(pop$n) * 5)
  expect_named(o1, c("t", "age_group", "replicate", "compartment", "y"))
})

test_that("squire object check and summary", {

  pop = get_population("Afghanistan")
  set.seed(123)
  r1 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 400,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set=contact_matrices[[1]])

  # check correctly identifies
  expect_silent(check_squire(r1))

  # check shows non squire
  naive <- data.frame("a" = 1, "b" = 3)
  expect_error(check_squire(naive), "Object must be a squire_simulation")

  # summary tests
  expect_output(summary(r1), regexp = "squire simulation")
  expect_output(print(r1), regexp = "1.1 years")



})

test_that("squire object check and summary", {

  pop = get_population("Afghanistan")
  set.seed(123)
  r1 <- run_SEEIR_model(population = pop$n,
                        dt = 1,
                        R0 = 2,
                        time_period = 100,
                        replicates = 10,
                        baseline_contact_matrix = contact_matrices[[1]],
                        contact_matrix_set=contact_matrices[[1]])

  # check plotting actually happens
  pl <- plot(r1)
  expect_is(pl, "ggplot")

})
