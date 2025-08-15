test_that("TADA_AutoClean function does not grow dataset", {
  testautoclean1 <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 1,
    autoclean = FALSE
  )
  testautoclean2 <- TADA_AutoClean(testautoclean1)
  expect_true(dim(testautoclean1)[1] == dim(testautoclean2)[1])
})

test_that("TADA_AutoClean: pH harmonization works as expected", {
 # get random pH data
 random_data <- TADA_RandomTestingData(
   choose_random_state = TRUE,
   number_of_days = 1,
   autoclean = FALSE
 )
 random_pH_data <- dplyr::filter(random_data, CharacteristicName %in% "pH")

 while (nrow(random_pH_data) < 2) {
   random_data <- TADA_RandomTestingData(
     choose_random_state = TRUE,
     number_of_days = 1,
     autoclean = FALSE
   )
   random_pH_data <- dplyr::filter(random_data, CharacteristicName %in% "pH")
}
 # TADA_AutoClean harmonizes pH, and other mod 1 required functions remove nonsensical data
 pHtest <- TADA_AutoClean(random_data)
 pHtest2 <- TADA_SimpleCensoredMethods(pHtest)
 pHtest3 <- TADA_ConvertSpecialChars(pHtest2, col = "TADA.ResultMeasureValue", clean = TRUE)
 pHtest4 <- TADA_RunKeyFlagFunctions(pHtest3, clean = TRUE)
 pHtest5 <- TADA_HarmonizeSynonyms(pHtest4)

 # Is pH data harmonized after above mod 1 functions have run?
 pHtest6 <- dplyr::filter(pHtest5, CharacteristicName %in% "pH")
 print(unique(pHtest6$TADA.ResultMeasure.MeasureUnitCode))
 expect_true(unique(pHtest6$TADA.ResultMeasure.MeasureUnitCode) == "NONE")
})

test_that("Column names do not contain the pattern 'TADA.TADA.'", {
  test_TADA.TADA. <-
    TADA_ConvertSpecialChars(
      Data_Nutrients_UT,
      "TADA.DetectionQuantitationLimitMeasure.MeasureValue"
    )
  # Create a logical vector indicating which columns contain the pattern
  pattern_found <- grepl("TADA.TADA.", colnames(test_TADA.TADA.))
  
  # Test should pass if none of the columns contain the pattern
  expect_false(any(pattern_found), info = "Some column names contain the pattern 'TADA.TADA.'")
})

test_that("Column names do not contain the pattern 'TADA.TADA.'", {
  test_TADA.TADA. <-
    TADA_ConvertSpecialChars(
      Data_Nutrients_UT,
      "TADA.ResultMeasureValue"
    )
  # Create a logical vector indicating which columns contain the pattern
  pattern_found <- grepl("TADA.TADA.", colnames(test_TADA.TADA.))
  
  # Test should pass if none of the columns contain the pattern
  expect_false(any(pattern_found), info = "Some column names contain the pattern 'TADA.TADA.'")
})

test_that("Column names do not contain the pattern 'TADA.TADA.'", {
  test_TADA.TADA. <- TADA_AutoClean(Data_R5_TADAPackageDemo)
  # Create a logical vector indicating which columns contain the pattern
  pattern_found <- grepl("TADA.TADA.", colnames(test_TADA.TADA.))
  
  # Test should pass if none of the columns contain the pattern
  expect_false(any(pattern_found), info = "Some column names contain the pattern 'TADA.TADA.'")
})



test_that("Only numeric data remains after running TADA_ConvertSpecialChars clean = TRUE", {
  testdat <- TADA_RandomTestingData(number_of_days = 1,
                                    choose_random_state = TRUE,
                                    autoclean = TRUE)
  
  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", 
                      "Percentage", 
                      "Result Value/Unit Estimated from Detection Limit", 
                      "Less Than", 
                      "TP estimated from one or more subspecies.", 
                      "TN estimated from one or more subspecies.")))  
})

test_that("TADA_ConvertSpecialChars removes NAs when clean = TRUE", {
  testdat <- TADA_RandomTestingData(number_of_days = 1,
                                    choose_random_state = TRUE,
                                    autoclean = TRUE)

  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Create a list of values with NA in TADA.ResultMeasureValue or TADA.ResultMeasureValueDataTypes.Flag
  na_values <- testdat[is.na(testdat$TADA.ResultMeasureValue), ]
  na_flags <- testdat[is.na(testdat$TADA.ResultMeasureValueDataTypes.Flag), ]
  
  # Check if either na_values or na_flags has observations and fail if they do
  if (nrow(na_values) > 0 || nrow(na_flags) > 0) {
    stop("Failure: There are NA observations in TADA.ResultMeasureValue or TADA.ResultMeasureValueDataTypes.Flag.")
  }

  # Test to ensure the value column is entirely numeric
  expect_true(
    is.numeric(testdat$TADA.ResultMeasureValue),
    info = "The TADA.ResultMeasureValue column is not entirely numeric."
  )
  
  # # Test to ensure unit column does not contain any NA values
  # expect_true(
  #   !any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)),
  #   info = "The TADA.ResultMeasure.MeasureUnitCode column contains NA values."
  # )
})

test_that("TADA_ConvertSpecialChars removes all NAs in result cols", {
  testdat <- TADA_RandomTestingData(number_of_days = 1, 
                                    choose_random_state = TRUE)

  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))
  
  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))
  
  # does not cover units yet, add back in future
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))
})


test_that("Only numeric data remains after running TADA_ConvertSpecialChars clean = TRUE", {
  testdat = TADA_DataRetrieval(statecode = "CO", 
                               startDate = "2017-06-20", 
                               endDate = "2017-06-21", 
                               ask = FALSE)
  
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", "NA - Not Available", "Text", "Percentage")))
  
  testdat <- TADA_SimpleCensoredMethods(testdat,
                                        nd_method = "multiplier",
                                        nd_multiplier = 0.5,
                                        od_method = "as-is",
                                        od_multiplier = "null")
  
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", 
                      "NA - Not Available", 
                      "Text", 
                      "Result Value/Unit Estimated from Detection Limit",
                      "Result Value/Unit Cannot Be Estimated From Detection Limit",
                      "Percentage"
                      )))
  
  # subset_df <- testdat[testdat$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit", ]
  
  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))
  
  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))
  
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))
  
  # Test to make sure remaining result value data types are expected
  # "Result Value/Unit Copied from Detection Limit" should no longer be there
  # NA should not be there... 
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", 
                      "Percentage",
                      "Result Value/Unit Estimated from Detection Limit", 
                      "Less Than")))  
})


test_that("Only numeric data remains after running TADA_ConvertSpecialChars clean = TRUE", {
  testdat <- TADA_RandomTestingData(number_of_days = 1,
                                    choose_random_state = TRUE,
                                    autoclean = TRUE)
  
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", 
                      "Result Value/Unit Estimated from Detection Limit", 
                      "Less Than",
                      "Percentage",
                      "Approximate Value",
                      "Greater Than",
                      "Comma-Separated Numeric",
                      "Numeric Range - Averaged",
                      "Percentage Range - Averaged",
                      "Approximate Value",
                      "Result Value/Unit Copied from Detection Limit",
                      "NA - Not Available",
                      "Text",
                      "Non-ASCII Character(s)",
                      "Result Value/Unit Cannot Be Estimated From Detection Limit")))
  
  testdat <- TADA_SimpleCensoredMethods(testdat,
                                        nd_method = "multiplier",
                                        nd_multiplier = 0.5,
                                        od_method = "as-is",
                                        od_multiplier = "null")
  
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", 
                      "Result Value/Unit Estimated from Detection Limit", 
                      "Less Than",
                      "Percentage",
                      "Approximate Value",
                      "Greater Than",
                      "Comma-Separated Numeric",
                      "Numeric Range - Averaged",
                      "Percentage Range - Averaged",
                      "Approximate Value",
                      "Result Value/Unit Copied from Detection Limit",
                      "NA - Not Available",
                      "Text",
                      "Non-ASCII Character(s)",
                      "Result Value/Unit Cannot Be Estimated From Detection Limit")))
  
  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))
  
  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))
  
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))
  
  # Test to make sure remaining result value data types are expected
  # "Result Value/Unit Copied from Detection Limit" should no longer be there
  # NA should not be there... 
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", 
                      "Result Value/Unit Estimated from Detection Limit", 
                      "Less Than",
                      "Percentage",
                      "Approximate Value",
                      "Greater Than",
                      "Comma-Separated Numeric",
                      "Numeric Range - Averaged",
                      "Percentage Range - Averaged",
                      "Approximate Value",
                      "Result Value/Unit Copied from Detection Limit")))  
})
