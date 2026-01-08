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

test_that("TADA_CorrectColType applies expected classes from reference", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  out <- TADA_CorrectColType(testdat)

  ref_path <- system.file("extdata", "TADAColTypeRef.csv", package = "EPATADA")
  expect_true(nzchar(ref_path) && file.exists(ref_path))
  ref <- utils::read.csv(ref_path, stringsAsFactors = FALSE, strip.white = TRUE)

  # Normalization
  ref$column_name <- trimws(ref$column_name)
  ref$column_type <- tolower(trimws(ref$column_type))

  present <- intersect(names(out), ref$column_name)

  # Helper to check classes by reference type
  check_col_class <- function(x, type) {
    type <- tolower(type)
    if (type == "character") {
      expect_true(is.character(x))
    } else if (type == "numeric") {
      # as.numeric produces double
      expect_identical(typeof(x), "double")
    } else if (type == "integer") {
      expect_identical(typeof(x), "integer")
    } else if (type == "logical") {
      expect_true(is.logical(x))
    } else if (type == "factor") {
      expect_true(is.factor(x))
    } else if (type == "date") {
      expect_true(inherits(x, "Date"))
    } else {
      # Unrecognized type — nothing to assert
      succeed()
    }
  }

  for (nm in present) {
    target_type <- ref$column_type[match(nm, ref$column_name)]
    check_col_class(out[[nm]], target_type)
  }
})

test_that("TADA_CorrectColType coerces from character to expected classes", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  ref_path <- system.file("extdata", "TADAColTypeRef.csv", package = "EPATADA")
  ref <- utils::read.csv(ref_path, stringsAsFactors = FALSE, strip.white = TRUE)
  ref$column_name <- trimws(ref$column_name)
  ref$column_type <- tolower(trimws(ref$column_type))

  present <- intersect(names(testdat), ref$column_name)
  # Take up to 5 columns to coerce from character regardless of their current class
  to_test <- head(present, 5)

  # Coerce selected columns to character deliberately
  for (nm in to_test) {
    testdat[[nm]] <- as.character(testdat[[nm]])
  }

  out <- TADA_CorrectColType(testdat)

  check_col_class <- function(x, type) {
    type <- tolower(type)
    if (type == "character") {
      expect_true(is.character(x))
    } else if (type == "numeric") {
      expect_identical(typeof(x), "double")
    } else if (type == "integer") {
      expect_identical(typeof(x), "integer")
    } else if (type == "logical") {
      expect_true(is.logical(x))
    } else if (type == "factor") {
      expect_true(is.factor(x))
    } else if (type == "date") {
      expect_true(inherits(x, "Date"))
    } else {
      succeed()
    }
  }

  for (nm in to_test) {
    target_type <- ref$column_type[match(nm, ref$column_name)]
    check_col_class(out[[nm]], target_type)
  }
})

test_that("TADA_CorrectColType leaves unknown columns unchanged", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  # Add an unknown column not present in the ref
  unknown_vals <- rep(1:3, length.out = nrow(testdat))
  testdat$Unknown_Column_For_Test <- unknown_vals

  out <- TADA_CorrectColType(testdat)

  expect_true("Unknown_Column_For_Test" %in% names(out))
  expect_identical(out$Unknown_Column_For_Test, unknown_vals)
})

test_that("TADA_CorrectColType warns when coercion introduces additional NAs", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  ref_path <- system.file("extdata", "TADAColTypeRef.csv", package = "EPATADA")
  ref <- utils::read.csv(ref_path, stringsAsFactors = FALSE, strip.white = TRUE)
  ref$column_name <- trimws(ref$column_name)
  ref$column_type <- tolower(trimws(ref$column_type))

  present <- intersect(names(testdat), ref$column_name)

  # Prefer a numeric column for NA-introducing test; otherwise try date, then logical
  pick_type <- function(type) {
    candidate <- present[
      ref$column_type[match(present, ref$column_name)] == type
    ]
    if (length(candidate)) candidate[1] else NULL
  }

  nm <- pick_type("numeric")
  if (is.null(nm)) {
    nm <- pick_type("integer")
  }
  if (is.null(nm)) {
    nm <- pick_type("date")
  }
  if (is.null(nm)) {
    nm <- pick_type("logical")
  }

  skip_if(
    is.null(nm),
    "No suitable column found to test NA-introducing warning."
  )

  # Create a copy and inject non-convertible values in rows that are currently non-NA
  bad <- testdat
  idx <- which(!is.na(bad[[nm]]))
  if (length(idx) == 0) {
    skip("No non-NA rows available in chosen column to test NA introduction.")
  }
  idx <- head(idx, min(3L, length(idx)))

  # Ensure column is character before coercion attempt
  bad[[nm]] <- as.character(bad[[nm]])
  ref_type <- ref$column_type[match(nm, ref$column_name)]

  if (ref_type %in% c("numeric", "integer")) {
    bad[[nm]][idx] <- "not_a_number"
  } else if (ref_type == "date") {
    bad[[nm]][idx] <- "not_a_date"
  } else if (ref_type == "logical") {
    bad[[nm]][idx] <- "maybe"
  } else {
    skip("Chosen column type won't reliably introduce NA on coercion.")
  }

  expect_warning(
    TADA_CorrectColType(bad),
    regexp = "introduced .* additional NA",
    info = paste0("Expected coercion to introduce NA in column '", nm, "'")
  )
})

# tests for TADA_WriteLayer

# Helper to create a minimal sf layer with fields that exercise renaming and sanitization
sample_layer <- function() {
  df <- data.frame(
    TOTALAREA_MI = c(1, 2),
    TOTALAREA_KM = c(3, 4),
    LongFieldName = c("A", "B"),
    LongFieldNum = c("C", "D"), # Will collide with LongFieldName after 10-char truncation
    `White space` = c("E", "F"),
    x = c(0, 1),
    y = c(0, 1),
    check.names = FALSE # preserve "White space" as-is
  )
  sf::st_as_sf(df, coords = c("x", "y"), crs = 4326)
}

test_that("TADA_WriteLayer sanitizes names, renames TOTALAREA_* fields, creates dir, and returns normalized path", {
  layer <- sample_layer()
  capture_env <- new.env(parent = emptyenv())
  capture_env$calls <- 0L

  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) layer,
    {
      with_mocked_bindings(
        .package = "sf",
        st_write = function(obj, dsn, ...) {
          capture_env$calls <- capture_env$calls + 1L
          capture_env$last_args <- list(obj = obj, dsn = dsn)
          TRUE
        },
        {
          out_path <- file.path(tempdir(), "nested1", "nested2", "ok.shp")
          ret <- TADA_WriteLayer(
            "http://fake/query",
            out_path,
            sanitize_names = TRUE
          )

          expect_equal(ret, normalizePath(out_path, mustWork = FALSE))
          expect_true(dir.exists(dirname(out_path)))
          expect_identical(capture_env$calls, 1L)

          layer_passed <- capture_env$last_args$obj
          expect_s3_class(layer_passed, "sf")
          expect_identical(attr(layer_passed, "sf_column"), "geometry")

          expect_identical(
            names(layer_passed),
            c(
              "tarea_mi",
              "tarea_km",
              "longfieldn",
              "longfield1", # ≤10 chars with numeric suffix applied
              "white_spac",
              "geometry"
            )
          )
        }
      )
    }
  )
})

test_that("TADA_WriteLayer can skip sanitization but still renames TOTALAREA_*", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) sample_layer(),
    {
      capture_env <- new.env(parent = emptyenv())
      with_mocked_bindings(
        .package = "sf",
        st_write = function(obj, dsn, ...) {
          capture_env$last <- obj
          TRUE
        },
        {
          out_path <- file.path(tempdir(), "nosanitize.shp")
          TADA_WriteLayer(
            "http://fake/query",
            out_path,
            sanitize_names = FALSE
          )

          layer_passed <- capture_env$last
          expect_identical(
            names(layer_passed),
            c(
              "TAREA_MI",
              "TAREA_KM",
              "LongFieldName",
              "LongFieldNum",
              "White space",
              "geometry"
            )
          )
        }
      )
    }
  )
})

test_that("TADA_WriteLayer warns when layerfilepath does not end with .shp", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) sample_layer(),
    {
      with_mocked_bindings(.package = "sf", st_write = function(...) TRUE, {
        out_path <- file.path(tempdir(), "layer.gpkg")
        expect_warning(
          TADA_WriteLayer("http://fake/query", out_path),
          "does not end with .shp"
        )
      })
    }
  )
})

test_that("TADA_WriteLayer reports getFeatureLayer errors clearly", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) stop("network fail"),
    {
      expect_error(
        TADA_WriteLayer(
          "http://fake/query",
          file.path(tempdir(), "a.shp")
        ),
        "getFeatureLayer\\(\\) failed for URL: .* — network fail"
      )
    }
  )
})

test_that("TADA_WriteLayer reports st_write errors clearly", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) sample_layer(),
    {
      with_mocked_bindings(
        .package = "sf",
        st_write = function(...) stop("GDAL write failure"),
        {
          expect_error(
            TADA_WriteLayer(
              "http://fake/query",
              file.path(tempdir(), "b.shp")
            ),
            "st_write\\(\\) failed for path: .* — GDAL write failure"
          )
        }
      )
    }
  )
})

test_that("TADA_WriteLayer validates inputs", {
  expect_error(TADA_WriteLayer(123, file.path(tempdir(), "x.shp")))
  expect_error(TADA_WriteLayer(character(), file.path(tempdir(), "x.shp")))
  expect_error(TADA_WriteLayer("", file.path(tempdir(), "x.shp")))

  expect_error(TADA_WriteLayer("http://fake/query", 1))
  expect_error(TADA_WriteLayer("http://fake/query", character()))
  expect_error(TADA_WriteLayer("http://fake/query", ""))
})
