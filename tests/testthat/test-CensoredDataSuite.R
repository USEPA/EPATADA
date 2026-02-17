test_that("TADA_IDCensoredData orphans", {
  cens.check <- TADA_RandomTestingData(choose_random_state = TRUE)
  expect_true(all(!is.na(cens.check$TADA.CensoredData.Flag)))
})

test_that("TADA_SimpleCensoredMethods doesn't drop data", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  cens.check <- TADA_SimpleCensoredMethods(testdat)
  expect_equal(dim(testdat)[1], dim(cens.check)[1])
})

test_that("TADA_IDCensoredData correctly handles specific text values such as ND", {
  # example data with this issue
  df <- TADA_DataRetrieval(
    startDate = "2022-12-19",
    endDate = "2022-12-20",
    ask = FALSE
  )

  df1 <- TADA_IDCensoredData(df)

  df2 <- subset(
    df1,
    subset = df1$ResultMeasureValue == "BPQL" |
      df1$ResultMeasureValue == "BDL" |
      df1$ResultMeasureValue == "ND"
  )

  unique(df2$ResultMeasureValue)

  # subset df: TADA.DetectionQuantitationLimitMeasure.MeasureValue = NA or None
  df3 <- subset(
    df2,
    subset = (!is.na(df2$TADA.DetectionQuantitationLimitMeasure.MeasureValue))
  )

  df3_subset <- dplyr::select(
    df3,
    ActivityTypeCode,
    ResultDetectionConditionText,
    CharacteristicName,
    TADA.CharacteristicName,
    ResultMeasureValue,
    TADA.ResultMeasureValue,
    ResultMeasure.MeasureUnitCode,
    TADA.ResultMeasureValueDataTypes.Flag,
    TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag,
    TADA.ResultMeasure.MeasureUnitCode,
    DetectionQuantitationLimitTypeName,
    DetectionQuantitationLimitMeasure.MeasureValue,
    DetectionQuantitationLimitMeasure.MeasureUnitCode,
    TADA.DetectionQuantitationLimitMeasure.MeasureValue,
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
    ProviderName
  )

  expect_true(all(
    df3_subset$TADA.ResultMeasureValueDataTypes.Flag ==
      "Result Value/Unit Copied from Detection Limit"
  ))

  expect_true(all(!is.na(df3_subset$TADA.ResultMeasureValue)))

  expect_true(all(!is.na(df3_subset$TADA.ResultMeasure.MeasureUnitCode)))
})

test_that("TADA_IDCensoredData does not introduce NAs in TADA.ResultMeasureValueDataTypes.Flag", {
  testdat <- TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 1,
    autoclean = TRUE
  )

  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Create a list of values with NA in TADA.ResultMeasureValueDataTypes.Flag
  na_flags <- testdat[is.na(testdat$TADA.ResultMeasureValueDataTypes.Flag), ]

  # Check if either na_values or na_flags has observations and fail if they do
  if (nrow(na_flags) > 0) {
    stop(
      "Failure: There are NA observations in TADA.ResultMeasureValueDataTypes.Flag."
    )
  }

  testdat2 <- TADA_IDCensoredData(testdat)

  # Create a list of values with NA in TADA.ResultMeasureValueDataTypes.Flag
  na_flags_2 <- testdat2[
    is.na(testdat2$TADA.ResultMeasureValueDataTypes.Flag),
  ]

  # Check if either na_values or na_flags has observations and fail if they do
  if (nrow(na_flags_2) > 0) {
    stop(
      "Failure: There are NA observations in TADA.ResultMeasureValueDataTypes.Flag."
    )
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

test_that("TADA_SimpleCensoredMethods does not introduce duplicates or NAs in result or unit cols that cannot be handled in TADA_ConvertSpecialChars", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))

  # TADA_ConvertSpecialChars does not handle this yet 8/11/25
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))

  testdat2 <- TADA_SimpleCensoredMethods(
    testdat,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )

  testdat3 <- TADA_ConvertSpecialChars(
    testdat2,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat3$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat3$TADA.ResultMeasureValue)))

  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat2$TADA.ResultMeasure.MeasureUnitCode)))
})

make_TADA_IDCensored_fixture <- function() {
  data.frame(
    ResultIdentifier = c("r1", "r2", "r3", "r4", "r5"),

    # Original result values (character, because ND/BDL/BPQL are strings)
    ResultMeasureValue = c("ND", "ND", NA_character_, "ND", NA_character_),

    # Original result unit (character)
    ResultMeasure.MeasureUnitCode = c(NA, NA, "mg/L", NA, NA),

    # TARGET columns that TADA_IDCensoredData expects to exist
    TADA.ResultMeasureValue = c(
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_
    ),
    TADA.ResultMeasure.MeasureUnitCode = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),

    # Detection condition text (Non-Detect when "Below Detection Limit")
    ResultDetectionConditionText = c(
      "Below Detection Limit", # r1
      "Below Detection Limit", # r2
      "Below Detection Limit", # r3
      "Below Detection Limit", # r4 -> will conflict with an Over-Detect limit type
      "Below Detection Limit" # r5
    ),

    # Detection limit type name (Non-Detect: "Reporting Limit"; Over-Detect: "Upper Quantitation Limit")
    DetectionQuantitationLimitTypeName = c(
      "Reporting Limit", # r1 Non-Detect
      "Reporting Limit", # r2 Non-Detect
      "Reporting Limit", # r3 Non-Detect
      "Upper Quantitation Limit", # r4 Over-Detect -> conflict with Non-Detect condition
      "Reporting Limit" # r5 Non-Detect
    ),

    # Detection limit value/unit (used for copying)
    TADA.DetectionQuantitationLimitMeasure.MeasureValue = c(
      0.5,
      NA,
      0.7,
      0.8,
      1.0
    ),
    TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode = c(
      "mg/L",
      NA,
      "mg/L",
      "mg/L",
      NA
    ),

    # Pre-existing TADA flag (controls path 2 copying when "NA - Not Available" or NA)
    TADA.ResultMeasureValueDataTypes.Flag = c(
      NA,
      NA,
      "NA - Not Available",
      NA,
      "NA - Not Available"
    ),

    # Include this to avoid running TADA_FlagMeasureQualifierCode for the test
    TADA.MeasureQualifierCode.Flag = NA_character_,

    stringsAsFactors = FALSE
  )
}

test_that("TADA_IDCensoredData copies detection limit values only when rules are met, and reverts on conflicts", {
  df <- make_TADA_IDCensored_fixture()
  out <- TADA_IDCensoredData(df)

  # r1: Path 1 copy — original result is ND and DL value+unit both present
  expect_equal(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "r1"],
    "Result Value/Unit Copied from Detection Limit"
  )
  expect_equal(out$TADA.ResultMeasureValue[out$ResultIdentifier == "r1"], 0.5)
  expect_equal(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "r1"],
    "mg/L"
  )

  # r2: Path 1 non-copy — original result ND but DL value+unit both NA
  expect_equal(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "r2"],
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )
  expect_true(is.na(out$TADA.ResultMeasureValue[out$ResultIdentifier == "r2"]))
  expect_true(is.na(out$TADA.ResultMeasure.MeasureUnitCode[
    out$ResultIdentifier == "r2"
  ]))

  # r3: Path 2 copy — TADA flag is "NA - Not Available" and DL value present (unit present -> copied)
  expect_equal(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "r3"],
    "Result Value/Unit Copied from Detection Limit"
  )
  expect_equal(out$TADA.ResultMeasureValue[out$ResultIdentifier == "r3"], 0.7)
  expect_equal(
    out$TADA.ResultMeasure.MeasureUnitCode[out$ResultIdentifier == "r3"],
    "mg/L"
  )

  # r4: Conflict case — initially copies, but condition/limit mismatch triggers conflict and reversion
  expect_equal(
    out$TADA.CensoredData.Flag[out$ResultIdentifier == "r4"],
    "Conflict between Condition and Limit"
  )
  expect_equal(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "r4"],
    "Result Value/Unit Cannot Be Estimated From Detection Limit"
  )
  expect_true(is.na(out$TADA.ResultMeasureValue[out$ResultIdentifier == "r4"]))
  expect_true(is.na(out$TADA.ResultMeasure.MeasureUnitCode[
    out$ResultIdentifier == "r4"
  ]))

  # r5: Path 2 copy with missing unit — DL value present and flag was NA - Not Available; unit is missing and should remain NA
  expect_equal(
    out$TADA.ResultMeasureValueDataTypes.Flag[out$ResultIdentifier == "r5"],
    "Result Value/Unit Copied from Detection Limit"
  )
  expect_equal(out$TADA.ResultMeasureValue[out$ResultIdentifier == "r5"], 1.0)
  expect_true(is.na(out$TADA.ResultMeasure.MeasureUnitCode[
    out$ResultIdentifier == "r5"
  ]))
})
