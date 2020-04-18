context("projections")

#------------------------------------------------
test_that("projection works", {

  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  dt = 1,
                  time_period = 100,
                  replicates = 2,
                  R0_scan = c(3,4))

  set.seed(123)
  index <- odin_index(t1$model)
  p1 <- projections(r = t1, R0 = 1.8, tt_R0 = 0)
  expect_gt(sum(rowSums(t1$output[,index$D,1])), sum(rowSums(p1$output[,index$D,1])))
  p2 <- projections(r = t1, R0_change = c(0.5, 0.2), tt_R0 = c(0, 20))
  expect_gt(sum(rowSums(p1$output[,index$D,1])), sum(rowSums(p2$output[,index$D,1])))

  out <- format_output(p2, "infections")
  expect_gt(mean(out$y[out$t==19]), mean(out$y[out$t==45]))


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

  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  dt = 1,
                  time_period = 100,
                  replicates = 2,
                  R0_scan = c(3,4))

  p2 <- projections(r = t1, R0_change = c(0.5, 0.2), tt_R0 = c(0, 20))

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

})
