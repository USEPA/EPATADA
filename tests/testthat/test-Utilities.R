test_that("TADA_AutoClean function does not grow dataset", {
  testautoclean1 <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 5,
    autoclean = FALSE
  )
  testautoclean2 <- TADA_AutoClean(testautoclean1)
  expect_true(dim(testautoclean1)[1] == dim(testautoclean2)[1])
})

test_that("TADA_AutoClean function harmonizes pH units", {
  
  get_random_pH_data <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 5,
    autoclean = FALSE
  )
  
  pHtest <- dplyr::filter(get_random_pH_data, CharacteristicName %in% "pH")
  
  verify_random_data <- function() {
    df <- pHtest()
    while(nrow(df) < 200) {
      df <- get_random_pH_data()
    }
    return(df)
  }
  
  pHtest2 <- TADA_AutoClean(pHtest)
  
  print(unique(pHtest2$TADA.ResultMeasure.MeasureUnitCode))
  
  expect_true(unique(pHtest2$TADA.ResultMeasure.MeasureUnitCode) == "STD UNITS")
  
})
