test_that("TADA_AutoClean function does not grow dataset", {
  testautoclean1 <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 5,
    autoclean = FALSE
  )
  testautoclean2 <- TADA_AutoClean(testautoclean1)
  expect_true(dim(testautoclean1)[1] == dim(testautoclean2)[1])
})

test_that("TADA_AutoClean: pH harmonization works as expected", {
  
  # get random pH data
  random_data <- TADA_RandomTestingData(
    choose_random_state = FALSE,
    number_of_days = 1,
    autoclean = FALSE
  )
  random_pH_data <- dplyr::filter(random_data, CharacteristicName %in% "pH")
  
  while(nrow(random_pH_data) < 2) {
    random_data <- TADA_RandomTestingData(
      choose_random_state = FALSE,
      number_of_days = 1,
      autoclean = FALSE
    )
    random_pH_data <- dplyr::filter(random_data, CharacteristicName %in% "pH")
  }
  
  # TADA_AutoClean harmonizes pH, and other mod 1 required functions remove garbage data
  pHtest <- TADA_AutoClean(random_data)
  pHtest2 <- TADA_SimpleCensoredMethods(pHtest)
  pHtest3 <- TADA_AutoFilter(pHtest2)
  pHtest4 <- TADA_RunKeyFlagFunctions(pHtest3)
  pHtest5 <- TADA_HarmonizeSynonyms(pHtest4)
  
  # Is pH data harmonized after above mod 1 functions have run?
  pHtest6 <- dplyr::filter(pHtest5, CharacteristicName %in% "pH")
  print(unique(pHtest6$TADA.ResultMeasure.MeasureUnitCode))
  expect_true(unique(pHtest6$TADA.ResultMeasure.MeasureUnitCode) == "STD UNITS")
  
})
