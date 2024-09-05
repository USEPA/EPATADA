test_that("TADA_PairForCriteriaCalc function does not grow dataset", {
  
  testdat1 <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 1,
    autoclean = TRUE
  )
  
  testdat2 <- TADA_PairForCriteriaCalc(testdat1)
  expect_true(dim(testdat1)[1] == dim(testdat2)[1])
})

