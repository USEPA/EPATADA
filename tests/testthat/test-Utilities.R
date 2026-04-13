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
  test_TADA.TADA. <- TADA_ConvertSpecialChars(
    Data_Nutrients_UT,
    "TADA.DetectionQuantitationLimitMeasure.MeasureValue"
  )
  # Create a logical vector indicating which columns contain the pattern
  pattern_found <- grepl("TADA.TADA.", colnames(test_TADA.TADA.))

  # Test should pass if none of the columns contain the pattern
  expect_false(
    any(pattern_found),
    info = "Some column names contain the pattern 'TADA.TADA.'"
  )
})

test_that("Column names do not contain the pattern 'TADA.TADA.'", {
  test_TADA.TADA. <- TADA_ConvertSpecialChars(
    Data_Nutrients_UT,
    "TADA.ResultMeasureValue"
  )
  # Create a logical vector indicating which columns contain the pattern
  pattern_found <- grepl("TADA.TADA.", colnames(test_TADA.TADA.))

  # Test should pass if none of the columns contain the pattern
  expect_false(
    any(pattern_found),
    info = "Some column names contain the pattern 'TADA.TADA.'"
  )
})

test_that("Column names do not contain the pattern 'TADA.TADA.'", {
  test_TADA.TADA. <- TADA_AutoClean(Data_R5_TADAPackageDemo)
  # Create a logical vector indicating which columns contain the pattern
  pattern_found <- grepl("TADA.TADA.", colnames(test_TADA.TADA.))

  # Test should pass if none of the columns contain the pattern
  expect_false(
    any(pattern_found),
    info = "Some column names contain the pattern 'TADA.TADA.'"
  )
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

  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  expect_true(all(
    unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
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
      )
  ))
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

  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Create a list of values with NA in TADA.ResultMeasureValue or TADA.ResultMeasureValueDataTypes.Flag
  na_values <- testdat[is.na(testdat$TADA.ResultMeasureValue), ]
  na_flags <- testdat[is.na(testdat$TADA.ResultMeasureValueDataTypes.Flag), ]

  # Check if either na_values or na_flags has observations and fail if they do
  if (nrow(na_values) > 0 || nrow(na_flags) > 0) {
    stop(
      "Failure: There are NA observations in TADA.ResultMeasureValue or TADA.ResultMeasureValueDataTypes.Flag."
    )
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

  testdat <- TADA_ConvertSpecialChars(
    testdat,
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

  testdat <- TADA_SimpleCensoredMethods(
    testdat,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )

  testdat <- TADA_ConvertSpecialChars(
    testdat,
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
  expect_true(all(
    unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
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
      )
  ))
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

  # Try to retrieve pH data using the TADA_DataRetrieval function.
  ph_data <- tryCatch(
    {
      TADA_DataRetrieval(
        startDate = random_date_minus_2_str,
        endDate = random_date_str,
        characteristicName = "pH",
        ask = FALSE
      )
    },
    httr2_http_500 = function(e) {
      # Skip the test if a 500 error occurs
      skip(
        "Skipping test due to 500 Internal Server Error during data retrieval"
      )
    },
    error = function(e) {
      # Re-throw the error if it's not a 500 error
      stop(e)
    }
  )

  # Check if the required data frame is empty or null
  # - Skips the test if no data is retrieved.
  if (is.null(ph_data) || nrow(ph_data) == 0) {
    skip("Skipping test because ph_data is empty or null")
  }

  # Process data
  # - Applies several functions to clean and harmonize the data.
  ph_data <- ph_data |>
    TADA_SimpleCensoredMethods() |>
    TADA_ConvertSpecialChars(col = "TADA.ResultMeasureValue", clean = TRUE) |>
    TADA_RunKeyFlagFunctions(clean = TRUE) |>
    TADA_HarmonizeSynonyms()

  # Assert that the data frame is not empty
  # - Ensures that the processed data frame contains rows.
  testthat::expect_gt(
    base::nrow(ph_data),
    0,
    label = "Data frame should not be empty"
  )

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
  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))

  # Test to make sure remaining result value data types are expected
  expect_true(all(
    unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
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
      )
  ))
})

# Mock characteristic reference table with a variety of cases
char_ref <- tibble::tribble(
  ~CharacteristicName                                                  , ~Char_Flag   , ~Comparable.Name                     , ~ExtraCol ,
  "Inorganic nitrogen (nitrate and nitrite)"                           , "Deprecated" , "Total Nitrogen (nitrate + nitrite)" , "X"       ,
  "Phosphate-phosphorus***retired***use Total Phosphorus, mixed forms" , "Deprecated" , "Total Phosphorus"                   , "Y"       ,
  # Duplicate key to test de-duplication and no row multiplication
  "Inorganic nitrogen (nitrate and nitrite)"                           , "Deprecated" , "Total Nitrogen (nitrate + nitrite)" , "Z"       ,
  # Deprecated but blank Comparable.Name
  "Old Thing"                                                          , "Deprecated" , ""                                   , "W"       ,
  # Active (non-deprecated)
  "Nitrate"                                                            , "Active"     , NA_character_                        , "A"
)

# Helper to run the function with mocked dependencies
run_with_mocks <- function(df, quiet = FALSE) {
  testthat::with_mocked_bindings(
    TADA_SubstituteDeprecatedChars(df, quiet = quiet),
    TADA_GetCharacteristicRef = function() char_ref,
    TADA_OrderCols = function(x) x, # no-op to preserve structure
    TADA_CheckColumns = function(.data, cols) {} # no-op, our test data includes the required columns
  )
}

test_that("preserves row count and order, and does not duplicate rows", {
  df <- tibble::tribble(
    ~CharacteristicName                        , ~OtherCol ,
    "Inorganic nitrogen (nitrate and nitrite)" ,         1 ,
    "Nitrate"                                  ,         2 ,
    "Phosphate-phosphorus"                     ,         3 ,
    "Old Thing"                                ,         4
  )

  result <- run_with_mocks(df, quiet = TRUE)

  expect_equal(nrow(result), nrow(df))
  expect_equal(result$OtherCol, df$OtherCol) # order preserved
})

test_that("uppercases TADA.CharacteristicName for all rows", {
  df <- tibble::tribble(
    ~CharacteristicName                        , ~OtherCol ,
    "Inorganic nitrogen (nitrate and nitrite)" ,         1 ,
    "Nitrate"                                  ,         2 ,
    "Phosphate-phosphorus"                     ,         3 ,
    "Old Thing"                                ,         4
  )

  result <- run_with_mocks(df, quiet = TRUE)
  expect_true(all(
    result$TADA.CharacteristicName == toupper(result$TADA.CharacteristicName)
  ))
})

test_that("substitutions occur where Comparable.Name is present; blanks/NA do not produce NA", {
  df <- tibble::tribble(
    ~CharacteristicName                        , ~OtherCol ,
    "Inorganic nitrogen (nitrate and nitrite)" ,         1 ,
    "Old Thing"                                ,         2
  )
  result <- run_with_mocks(df, quiet = TRUE)

  # Substitution for deprecated with valid comparable name
  expect_equal(
    result$TADA.CharacteristicName[1],
    "TOTAL NITROGEN (NITRATE + NITRITE)"
  )

  # For deprecated with blank Comparable.Name, keep original name uppercase
  expect_equal(result$TADA.CharacteristicName[2], "OLD THING")
})

test_that("NWIS retired trimming works for WQX names containing '***retired***'", {
  df <- tibble::tribble(
    ~CharacteristicName    , ~OtherCol ,
    "Phosphate-phosphorus" ,         1
  )
  result <- run_with_mocks(df, quiet = TRUE)

  expect_equal(result$TADA.CharacteristicName[1], "TOTAL PHOSPHORUS")
})

test_that("respects quiet = TRUE (no messages), and reports detailed mapping when quiet = FALSE", {
  df <- tibble::tribble(
    ~CharacteristicName                        , ~OtherCol ,
    "Inorganic nitrogen (nitrate and nitrite)" ,         1 ,
    "Phosphate-phosphorus"                     ,         2 ,
    "Old Thing"                                ,         3 ,
    "Nitrate"                                  ,         4
  )

  # No messages when quiet = TRUE
  expect_no_message(run_with_mocks(df, quiet = TRUE))

  # Expect detailed mapping when quiet = FALSE (only substituted ones)
  expect_message(
    run_with_mocks(df, quiet = FALSE),
    regexp = "Inorganic nitrogen \\(nitrate and nitrite\\) -> TOTAL NITROGEN \\(NITRATE \\+ NITRITE\\)"
  )
  expect_message(
    run_with_mocks(df, quiet = FALSE),
    regexp = "Phosphate-phosphorus -> TOTAL PHOSPHORUS"
  )
})

test_that("does not leak extra columns from ref and removes ref join columns", {
  df <- tibble::tribble(
    ~CharacteristicName                        , ~OtherCol ,
    "Inorganic nitrogen (nitrate and nitrite)" ,         1 ,
    "Phosphate-phosphorus"                     ,         2
  )
  result <- run_with_mocks(df, quiet = TRUE)

  # ExtraCol from ref should not appear
  expect_false("ExtraCol" %in% names(result))
  # Ref join columns should be removed
  expect_false("Comparable.Name" %in% names(result))
  expect_false("Char_Flag" %in% names(result))
})

test_that("when no deprecated names found, message indicates so", {
  df <- tibble::tribble(
    ~CharacteristicName , ~OtherCol ,
    "Nitrate"           ,         1
  )
  expect_message(
    run_with_mocks(df, quiet = FALSE),
    regexp = "No deprecated characteristic names found in dataset\\."
  )
})

test_that("existing TADA.CharacteristicName is preserved (and uppercased) when no substitution applies", {
  df <- tibble::tribble(
    ~CharacteristicName , ~OtherCol , ~TADA.CharacteristicName ,
    "Nitrate"           ,         1 , "bar"
  )
  result <- run_with_mocks(df, quiet = TRUE)

  # Should keep existing value and uppercase it
  expect_equal(result$TADA.CharacteristicName[1], "BAR")
})

test_that("handles empty input gracefully and respects quiet", {
  df <- tibble::tibble(CharacteristicName = character(), OtherCol = numeric())
  # With quiet = FALSE, message about empty DF
  expect_message(
    run_with_mocks(df, quiet = FALSE),
    regexp = "The entered data frame is empty\\. Skipping deprecated-name substitution\\."
  )
  # With quiet = TRUE, no message
  expect_no_message(run_with_mocks(df, quiet = TRUE))

  # Returned value is the same empty data frame
  result <- run_with_mocks(df, quiet = TRUE)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(df))
})

test_that("does not change non-deprecated names except for uppercasing", {
  df <- tibble::tribble(
    ~CharacteristicName , ~OtherCol ,
    "Nitrate"           ,         1
  )
  result <- run_with_mocks(df, quiet = TRUE)

  # Since TADA.CharacteristicName is initialized to uppercase of CharacteristicName,
  # and Nitrate is not deprecated, it should simply be 'NITRATE'
  expect_equal(result$TADA.CharacteristicName[1], "NITRATE")
})
