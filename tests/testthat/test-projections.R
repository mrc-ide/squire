context("projections")

#------------------------------------------------
test_that("projection works", {


  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.6
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 10

  t1 <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 40
  )

  set.seed(123)
  index <- odin_index(t1$model)
  p1 <- projections(r = t1, R0 = 1.8, tt_R0 = 0)
  expect_lt(sum(rowSums(t1$output[,index$D,1])), sum(rowSums(p1$output[,index$D,1])))
  p2 <- projections(r = t1, R0_change = c(0.1, 0.05), tt_R0 = c(0, 20))
  expect_gt(sum(rowSums(p1$output[,index$D,1])), sum(rowSums(p2$output[,index$D,1])))

  out <- format_output(p2, "infections")
  expect_gt(mean(out$y[out$t==19]), mean(out$y[out$t==40]))

  # arg checks
  p3 <- projections(t1)
  expect_message(p3 <- projections(t1, R0 = c(3,2), R0_change = c(1,0.5), tt_R0 = c(0, 30)))
  expect_message(p3 <- projections(t1, contact_matrix_set = list(contact_matrices[[1]],contact_matrices[[2]]),
                                   contact_matrix_set_change = c(1,0.5), tt_contact_matrix = c(0, 30)))
  expect_message(p3 <- projections(t1, hosp_bed_capacity = c(3,2), hosp_bed_capacity_change = c(1,0.5),
                                   tt_hosp_beds = c(0, 30)))
  expect_message(p3 <- projections(t1, ICU_bed_capacity = c(3,2), ICU_bed_capacity_change = c(1,0.5),
                                   tt_ICU_beds = c(0, 30)))
  p3 <- projections(t1, R0 = 2)
  p3 <- projections(t1, R0_change = 0.5)
  p3 <- projections(t1, contact_matrix_set = contact_matrices[[1]])
  p3 <- projections(t1, contact_matrix_set_change = 0.5)
  p3 <- projections(t1, hosp_bed_capacity = 2)
  p3 <- projections(t1, hosp_bed_capacity_change = 0.5)
  p3 <- projections(t1, ICU_bed_capacity = 2)
  p3 <- projections(t1, ICU_bed_capacity_change = 0.5)


  # length checks
  expect_error(p3 <- projections(t1, R0 = c(2,1)))
  expect_error(p3 <- projections(t1, contact_matrix_set = list(contact_matrices[[1]],contact_matrices[[1]])))
  expect_error(p3 <- projections(t1, hosp_bed_capacity = c(2,1)))
  expect_error(p3 <- projections(t1, ICU_bed_capacity = c(2,1)))
  expect_error(p3 <- projections(t1, R0_change = c(2,1)))
  expect_error(p3 <- projections(t1, contact_matrix_set_change = c(2,1)))
  expect_error(p3 <- projections(t1, hosp_bed_capacity_change = c(2,1)))
  expect_error(p3 <- projections(t1, ICU_bed_capacity_change = c(2,1)))

  # bounds check
  expect_error(p3 <- projections(t1, tt_R0 = c(30,60)))
  expect_error(p3 <- projections(t1, tt_contact_matrix = c(30,60)))
  expect_error(p3 <- projections(t1, tt_hosp_beds = c(30,60)))
  expect_error(p3 <- projections(t1, tt_ICU_beds = c(30,60)))


  expect_error(p3 <- projections(t1, tt_R0 = c(30,60)))
  expect_error(p3 <- projections(t1, tt_contact_matrix = c(30,60)))
  expect_error(p3 <- projections(t1, tt_hosp_beds = c(30,60)))
  expect_error(p3 <- projections(t1, tt_ICU_beds = c(30,60)))


})



#------------------------------------------------
test_that("projection plotting", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.6
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 10

  t1 <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 40
  )

  set.seed(123)
  p2 <- projections(r = t1, R0_change = c(0.5, 0.2), tt_R0 = c(0, 20))
  p3 <- projections(r = t1,
                    contact_matrix_set_change = c(1, 0.5, 0.2),
                    tt_contact_matrix = c(0, 10, 20))


  expect_warning(g1 <- projection_plotting(r_list = list(t1,p2),
                            scenarios = c("Unmitigated","Mitigation"),
                            var_select = "infections", add_parms_to_scenarios = FALSE))
  expect_s3_class(g1, "gg")

  expect_warning(g2 <- projection_plotting(r_list = list(t1,p2),
                            scenarios = c("Unmitigated","Mitigation"),
                            var_select = c("ICU_occupancy", "ICU_demand"),
                            add_parms_to_scenarios = TRUE,ci = FALSE,summarise = TRUE))
  expect_s3_class(g2, "gg")

  expect_warning(g2 <- projection_plotting(r_list = list(t1,p2),
                            scenarios = c("Unmitigated","Mitigation"),
                            var_select = c("ICU_occupancy", "ICU_demand"),
                            add_parms_to_scenarios = TRUE,ci = TRUE,summarise = TRUE, replicates = TRUE))

  expect_error(projection_plotting(r_list = list(t1,matrix(0,1,1)),
                                   scenarios = c("Unmitigated","Mitigation"),
                                   var_select = c("ICU_occupancy", "ICU_demand"),
                                   add_parms_to_scenarios = TRUE,ci = FALSE,summarise = TRUE))


  # hack pinter invalidation
  t1$model$.__enclos_env__$private$ptr <- new("externalptr")
  set.seed(123)
  p2_copy <- projections(r = t1, R0_change = c(0.5, 0.2), tt_R0 = c(0, 20))
  expect_true(all(p2_copy$output[nrow(p2_copy$output),index$S,1] ==
                    p2$output[nrow(p2_copy$output),index$S,1]))
})

#------------------------------------------------
test_that("projection with normal run", {

  r <- run_explicit_SEEIR_model("Angola",replicates = 1, time_period = 100)
  r$output[,"time",1] <- r$output[,"time",1] - 50
  p <- projections(r, R0 = 10)
  index <- odin_index(p$model)
  expect_gt(sum(p$output[nrow(p$output),index$D,1]),
            sum(r$output[nrow(r$output),index$D,1]))

})


#------------------------------------------------
test_that("projection continuation past array size", {

  data <- read.csv(squire_file("extdata/example.csv"),stringsAsFactors = FALSE)
  interventions <- read.csv(squire_file("extdata/example_intervention.csv"))
  int_unique <- interventions_unique(interventions)
  reporting_fraction = 1
  country = "Algeria"
  replicates = 2
  R0_min = 2.6
  R0_max = 2.6
  R0_step = 0.1
  first_start_date = "2020-02-01"
  last_start_date = "2020-02-02"
  day_step = 1
  R0_change = int_unique$change
  date_R0_change = as.Date(int_unique$dates_change)
  date_contact_matrix_set_change = NULL
  squire_model = explicit_model()
  pars_obs = NULL
  n_particles = 2

  t1 <- calibrate(
    data = data,
    R0_min = R0_min,
    R0_max = R0_max,
    R0_step = R0_step,
    first_start_date = first_start_date,
    last_start_date = last_start_date,
    day_step = day_step,
    squire_model = squire_model,
    pars_obs = pars_obs,
    n_particles = n_particles,
    reporting_fraction = reporting_fraction,
    R0_change = R0_change,
    date_R0_change = date_R0_change,
    replicates = replicates,
    country = country,
    forecast = 10
  )

  p <- projections(t1, time_period = 20, R0_change = 2)
  expect_equal(as.Date(max(data$date)) + 20, as.Date(tail(rownames(p$output),1)))
  expect_true(all(diff(as.Date(rownames(p$output)))==1))

  r <- run_explicit_SEEIR_model("Angola",replicates = 1, time_period = 100)
  r$output[,"time",1] <- r$output[,"time",1] - 90
  p <- projections(r, R0_change = 2, time_period = 20)
  expect_true(all(round((diff(p$output[,"time",])),1) == r$parameters$dt))


})

