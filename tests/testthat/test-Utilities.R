test_that("TADA_AutoClean function does not grow dataset", {
  testautoclean1 <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 1,
    autoclean = FALSE
  )
  testautoclean2 <- TADA_AutoClean(testautoclean1)
  expect_true(dim(testautoclean1)[1] == dim(testautoclean2)[1])
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

test_that("pH harmonization works as expected throughout workflow", {
  # Define a vector of state codes
  state_codes <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                   "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                   "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                   "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                   "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  
  # Select one random state code
  selected_state_code <- base::sample(state_codes, 1)
  
  # Select a random month and year in the last 10 years
  random_year_offset <- base::sample(0:9, 1)
  random_month_offset <- base::sample(0:11, 1)
  selected_year <- base::as.integer(base::format(base::Sys.Date(), "%Y")) - random_year_offset
  selected_month <- base::as.integer(base::format(base::Sys.Date(), "%m")) - random_month_offset
  
  # Adjust year and month if month is negative or zero
  if (selected_month <= 0) {
    selected_month <- selected_month + 12
    selected_year <- selected_year - 1
  }
  
  # Create start date
  start_date <- base::as.Date(base::paste(selected_year, base::sprintf("%02d", selected_month), "01", sep = "-"))
  
  # Function to add months using base R
  add_months <- function(date, n) {
    year <- base::as.integer(base::format(date, "%Y"))
    month <- base::as.integer(base::format(date, "%m"))
    day <- base::as.integer(base::format(date, "%d"))
    
    new_month <- month + n
    new_year <- year + (new_month - 1) %/% 12
    new_month <- (new_month - 1) %% 12 + 1
    
    new_date <- base::as.Date(base::paste(new_year, new_month, day, sep = "-"), "%Y-%m-%d")
    
    # Adjust the day if the new date is invalid
    if (base::is.na(new_date)) {
      new_date <- base::as.Date(base::paste(new_year, new_month, "01", sep = "-"), "%Y-%m-%d")
      new_date <- new_date + base::as.integer(base::format(new_date, "%d")) - 1
    }
    
    return(new_date)
  }
  
  # Calculate end date for a 6-month period
  end_date <- add_months(start_date, 5)
  
  # Measure the time taken to retrieve data
  ph_data <- TADA_DataRetrieval(
      statecode = selected_state_code,
      startDate = base::as.character(start_date),
      endDate = base::as.character(end_date),
      characteristicName = "pH",
      ask = FALSE
    )

  # Process data
  ph_data <- ph_data %>%
    TADA_SimpleCensoredMethods() %>%
    TADA_ConvertSpecialChars(col = "TADA.ResultMeasureValue", clean = TRUE) %>%
    TADA_RunKeyFlagFunctions(clean = TRUE) %>%
    TADA_HarmonizeSynonyms()
  
  # Assert that the data frame is not empty
  testthat::expect_gt(base::nrow(ph_data), 0, label = "Data frame should not be empty")
  
  # Check results for the state
  base::print(base::unique(ph_data$TADA.ResultMeasure.MeasureUnitCode))
  if (!base::all(base::unique(ph_data$TADA.ResultMeasure.MeasureUnitCode) == "NONE")) {
    base::message(base::paste("pH data unit codes for state", selected_state_code, "are not harmonized to 'NONE'"))
  }
})
