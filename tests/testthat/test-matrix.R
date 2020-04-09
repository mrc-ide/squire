test_that("matrix manipulation", {
  im <- matrix(rep(1, 16 * 16), ncol = 16)
  m1 <- process_contact_matrix(im, rep(1, 17))

  ## TODO: Rewrite test for process contact matrix as the conversion is not quite right
  #expect_identical(m1, im)

  for(i in 1:20){
    i_rand <- matrix(sample(1:100, 16 * 16, replace  = TRUE), ncol = 16)
    diag(i_rand) <- 1
    p_rand <- sample(1:100, 17, replace  = TRUE)
    m1 <- process_contact_matrix(i_rand, p_rand)
    # TODO: expect_true(all(diag(m1) == 1))
  }

})
