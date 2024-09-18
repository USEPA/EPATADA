test_that("TADA_PairForCriteriaCalc function does not grow dataset", {
  testdat1 <- TADA_RandomTestingData(
    number_of_days = 1,
    choose_random_state = TRUE,
    autoclean = TRUE
  )

  # checks to see if testdat1 has any paired parameters needed to run TADA_PairForCriteriaCalc.
  # If it fails to run, an error will result and it will reiterate the testdat1 <- TADA_RandomTestingData() pull.
  tryCatch(
    {
      testdat2 <- TADA_PairForCriteriaCalc(testdat1)
    },
    error = function(testdat1) {
      for (i in 1:10) {
        testdat1 <- TADA_RandomTestingData(
          number_of_days = 1,
          choose_random_state = TRUE,
          autoclean = TRUE
        )
        # Checks if testdat2 will run without error with the new testdat1 data pull.
        # If try() is able to run without error, then the loop will stop.
        if (class(try(testdat2 <- TADA_PairForCriteriaCalc(testdat1))) != "try-error") {
          testdat2
          opt <- options(show.error.messages = FALSE)
          on.exit(options(opt))
          stop()
        }
      }
    }
  )

  expect_true(dim(testdat1)[1] == dim(testdat2)[1])
})
