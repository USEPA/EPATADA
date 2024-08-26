test_that("PairForCriteriaCalc does not grow dataset", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  testdat1 <- TADA_PairForCriteriaCalc(testdat)
  expect_true(dim(testdat)[1] == dim(testdat1)[1])
})