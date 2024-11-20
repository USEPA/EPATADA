test_that("TADA_PairForCriteriaCalc function does not grow dataset", {
  # checks to see if testdat1 has any paired parameters needed to run TADA_PairForCriteriaCalc.
  # If it fails to run, an error will result and it will reiterate the testdat1 <- TADA_RandomTestingData() pull.
  testdat <- tryCatch(
    {
      testdat1 <- TADA_RandomTestingData(
        number_of_days = 1,
        choose_random_state = TRUE,
        autoclean = TRUE
      )
      testdat2 <- TADA_PairForCriteriaCalc(testdat1)
      testdat <- list(testdat1, testdat2)
    },
    error = function(err) {
      testdat1 <- data.frame()
      testdat2 <- data.frame()
      for (i in 1:10) {
        testdat1 <- TADA_RandomTestingData(
          number_of_days = 1,
          choose_random_state = TRUE,
          autoclean = TRUE
        )
        # Checks if testdat2 will run without error with the new testdat1 data pull.
        # If try() is able to run without error, then the loop will stop.
        if (class(try(testdat2 <- TADA_PairForCriteriaCalc(testdat1))) != "try-error") {
          message("Initial test failed. Retrying. Loop has ran through ", i, " time(s)")
          testdat2 <- TADA_PairForCriteriaCalc(testdat1)
          testdat <- list(testdat1, testdat2)
          return(testdat)
          break
        }
      }
      testdat <- list(testdat1, testdat2)
      return(testdat)
    }
  )
  testdat1 <- testdat[[1]]
  testdat2 <- testdat[[2]]

  expect_true(dim(testdat1)[1] == dim(testdat2)[1])
})
