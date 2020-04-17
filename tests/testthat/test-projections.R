context("projections")

#------------------------------------------------
test_that("projection works", {

  t1 <- calibrate(country = "Angola", deaths = 5,
                  reporting_fraction = 0.5,
                  dt = 1,
                  time_period = 100,
                  replicates = 2,
                  R0_scan = c(3,4))

  index <- odin_index(t1$model)
  p1 <- projections(r = t1, mitigation = 0.5, tt_mitigation = 0)
  expect_gt(sum(rowSums(t1$output[,index$D,1])), sum(rowSums(p1$output[,index$D,1])))
  p2 <- projections(r = t1, mitigation = c(0.5, 0.2), tt_mitigation = c(0, 60))
  expect_gt(sum(rowSums(p1$output[,index$D,1])), sum(rowSums(p2$output[,index$D,1])))

})
