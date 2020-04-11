context("data")

test_that("contact_matrices are matrices", {

  expect_true(all(unlist(lapply(squire::contact_matrices, is.matrix))))

  expect_true(all(unlist(lapply(
    squire::contact_matrices[unique(squire::population$matrix)], function(x){
         (nrow(x) == ncol(x)) & (nrow(x) == 16)
  }))))

})
