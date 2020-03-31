test_that("matrix manipulation", {
  im <- matrix(rep(1, 16 * 16), ncol = 16)
  m1 <- process_contact_matrix(im, rep(1, 16))
  expect_identical(m1, im)

  for(i in 1:10){
    i_rand <- matrix(rpois(16 * 16, 5), ncol = 16)
    diag(i_rand) <- 1
    p_rand <- rpois(16, 10)
    m1 <- process_contact_matrix(i_rand, p_rand)
    expect_true(all(diag(m1) == 1))
  }

})
