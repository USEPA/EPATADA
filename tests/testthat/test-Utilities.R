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
  testdat <- TADA_RandomTestingData(
    number_of_days = 1,
    choose_random_state = TRUE,
    autoclean = TRUE
  )

  # Check if the required data frame is empty or null
  if (is.null(testdat) || nrow(testdat) == 0) {
    skip("Skipping test because testdat is empty or null")
  }

  testdat <- TADA_ConvertSpecialChars(testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
    c(
      "Numeric",
      "Result Value/Unit Estimated from Detection Limit",
      "Less Than",
      "Percentage",
      "Approximate Value",
      "Greater Than",
      "Comma-Separated Numeric",
      "Numeric Range - Averaged",
      "Percentage Range - Averaged",
      "Approximate Value"
    )))
})

test_that("TADA_ConvertSpecialChars removes NAs when clean = TRUE", {
  testdat <- TADA_RandomTestingData(
    number_of_days = 1,
    choose_random_state = TRUE,
    autoclean = TRUE
  )

  # Check if the required data frame is empty or null
  if (is.null(testdat) || nrow(testdat) == 0) {
    skip("Skipping test because testdat is empty or null")
  }

  testdat <- TADA_ConvertSpecialChars(testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

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
})

test_that("TADA_ConvertSpecialChars removes all NAs in result cols", {
  testdat <- TADA_RandomTestingData(
    number_of_days = 1,
    choose_random_state = TRUE
  )

  # Check if the required data frame is empty or null
  if (is.null(testdat) || nrow(testdat) == 0) {
    skip("Skipping test because testdat is empty or null")
  }

  testdat <- TADA_ConvertSpecialChars(testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))
})


test_that("Only numeric data remains after running TADA_ConvertSpecialChars clean = TRUE", {
  testdat <- TADA_DataRetrieval(
    statecode = "CO",
    startDate = "2017-06-20",
    endDate = "2017-06-21",
    ask = FALSE
  )

  # Check if the required data frame is empty or null
  if (is.null(testdat) || nrow(testdat) == 0) {
    skip("Skipping test because testdat is empty or null")
  }

  testdat <- TADA_SimpleCensoredMethods(testdat,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )

  testdat <- TADA_ConvertSpecialChars(testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))

  # Test to make sure remaining result value data types are expected
  # "Result Value/Unit Copied from Detection Limit" should no longer be there
  # NA should not be there...
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
    c(
      "Numeric",
      "Result Value/Unit Estimated from Detection Limit",
      "Less Than",
      "Percentage",
      "Approximate Value",
      "Greater Than",
      "Comma-Separated Numeric",
      "Numeric Range - Averaged",
      "Percentage Range - Averaged",
      "Approximate Value"
    )))
})

test_that("pH harmonization works as expected throughout workflow", {
  # Set the start and end dates
  start_date <- as.Date("2020-01-01")
  end_date <- as.Date("2025-08-01")

  # Calculate the number of days between the start and end dates
  date_range <- as.numeric(end_date - start_date)

  # Generate a random number of days to add to the start date
  random_days <- sample(0:date_range, 1)

  # Calculate the random date
  random_date <- start_date + random_days

  # Calculate the date that is two days before the random date
  random_date_minus_2 <- random_date - 3

  # Store the dates as character strings
  random_date_str <- format(random_date, "%Y-%m-%d")
  random_date_minus_2_str <- format(random_date_minus_2, "%Y-%m-%d")

  # Retrieves pH data using the TADA_DataRetrieval function.
  ph_data <- TADA_DataRetrieval(
    startDate = random_date_minus_2_str,
    endDate = random_date_str,
    characteristicName = "pH",
    ask = FALSE
  )

  # Check if the required data frame is empty or null
  # - Skips the test if no data is retrieved.
  if (is.null(ph_data) || nrow(ph_data) == 0) {
    skip("Skipping test because ph_data is empty or null")
  }

  # Process data
  # - Applies several functions to clean and harmonize the data.
  ph_data <- ph_data %>%
    TADA_SimpleCensoredMethods() %>%
    TADA_ConvertSpecialChars(col = "TADA.ResultMeasureValue", clean = TRUE) %>%
    TADA_RunKeyFlagFunctions(clean = TRUE) %>%
    TADA_HarmonizeSynonyms()

  # Assert that the data frame is not empty
  # - Ensures that the processed data frame contains rows.
  testthat::expect_gt(base::nrow(ph_data), 0, label = "Data frame should not be empty")

  # Check results for the state
  # Prints and checks the unit codes to verify harmonization.
  print(unique(ph_data$TADA.ResultMeasure.MeasureUnitCode))
  if (!all(unique(ph_data$TADA.ResultMeasure.MeasureUnitCode) == "NONE")) {
    message(paste("pH data unit codes are not harmonized to 'NONE'"))
  }
})

test_that("Only numeric data remains after running TADA_ConvertSpecialChars clean = TRUE", {
  # Generate test data
  testdat <- TADA_RandomTestingData(
    number_of_days = 1,
    choose_random_state = TRUE,
    autoclean = TRUE
  )

  # Check if the required data frame is empty or null
  if (is.null(testdat) || nrow(testdat) == 0) {
    skip("Skipping test because testdat is empty or null")
  }

  # expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
  #                   c("Numeric",
  #                     "Result Value/Unit Estimated from Detection Limit",
  #                     "Less Than",
  #                     "Percentage",
  #                     "Approximate Value",
  #                     "Greater Than",
  #                     "Comma-Separated Numeric",
  #                     "Numeric Range - Averaged",
  #                     "Percentage Range - Averaged",
  #                     "Approximate Value",
  #                     "Result Value/Unit Copied from Detection Limit",
  #                     "NA - Not Available",
  #                     "Text",
  #                     "Non-ASCII Character(s)",
  #                     "Result Value/Unit Cannot Be Estimated From Detection Limit")))

  # Apply Convert Special Chars function
  testdat <- TADA_ConvertSpecialChars(testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))

  # Test to make sure remaining result value data types are expected
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
    c(
      "Numeric",
      "Result Value/Unit Estimated from Detection Limit",
      "Less Than",
      "Percentage",
      "Approximate Value",
      "Greater Than",
      "Comma-Separated Numeric",
      "Numeric Range - Averaged",
      "Percentage Range - Averaged",
      "Approximate Value"
    )))
})
