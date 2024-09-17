
test_that("TADA_PairForCriteriaCalc function does not grow dataset", {
  
  testdat1 <- TADA_RandomTestingData(
    number_of_days = 1,
    choose_random_state = TRUE,
    autoclean = TRUE
    )
    
    tryCatch({
        testdat2 <- TADA_PairForCriteriaCalc (testdat1)
      }
      ,
      error = function (testdat1) {
        for(i in 1:10) {
          testdat1 <- TADA_RandomTestingData (
            number_of_days = 1,
            choose_random_state = TRUE,
            autoclean = TRUE
          )
        
        if( class( try( testdat2 <- TADA_PairForCriteriaCalc(testdat1))) != "try-error") {
          testdat2
          opt <- options(show.error.messages=FALSE)
          on.exit(options(opt))
          stop()
        }
      }
    })
    
  expect_true(dim(testdat1)[1] == dim(testdat2)[1])
})
