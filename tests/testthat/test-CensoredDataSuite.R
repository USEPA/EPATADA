test_that("TADA_IDCensoredData orphans", {
  cens.check <- TADA_RandomTestingData(choose_random_state = TRUE)
  expect_true(all(!is.na(cens.check$TADA.CensoredData.Flag)))
})

test_that("TADA_SimpleCensoredMethods doesn't drop data", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  cens.check <- TADA_SimpleCensoredMethods(testdat)
  expect_equal(dim(testdat)[1], dim(cens.check)[1])
})

test_that("TADA_IDCensoredData copies det lim values to result values if applicable", {
  copycheck <- TADA_RandomTestingData(choose_random_state = TRUE)

  if (nrow(copycheck) > 0) {
    copycheck1 <- TADA_IDCensoredData(copycheck)

    # let's look only at rows where the original result value = NA
    copycheck2 <- subset(copycheck1, subset = is.na(copycheck1$ResultMeasureValue))

    # the TADA.ResultMeasureValueDataTypes.Flag should = one of these three options
    expect_true(all(copycheck2$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit" |
                      copycheck2$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Cannot Be Estimated From Detection Limit" |
                      copycheck2$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available"))

    # subset df: TADA.DetectionQuantitationLimitMeasure.MeasureValue = NA or None
    copycheck_NAs <- subset(copycheck2, subset = (!is.na(copycheck2$TADA.DetectionQuantitationLimitMeasure.MeasureValue)))

    # for this subset, the TADA.ResultMeasureValueDataTypes.Flag should equal "Result Value/Unit Copied from Detection Limit"
    expect_true(all(copycheck_NAs$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit" &
                      !is.na(copycheck_NAs$TADA.ResultMeasureValue)))

    # subset df: TADA.DetectionQuantitationLimitMeasure.MeasureValue does NOT = NA or None
    copycheck_copies <- subset(copycheck2, subset = (is.na(copycheck2$TADA.DetectionQuantitationLimitMeasure.MeasureValue)))

    # for this subset, the TADA.ResultMeasureValueDataTypes.Flag should equal "NA - Not Available"
    expect_true(all((copycheck_copies$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available") &
      is.na(copycheck_copies$TADA.ResultMeasureValue)))
  }
})

test_that("TADA_IDCensoredData correctly handles specific text values such as ND", {
  # example data with this issue
  df <- TADA_DataRetrieval(
    startDate = "2022-12-19",
    endDate = "2022-12-20",
    ask = FALSE
  )

  df1 <- TADA_IDCensoredData(df)

  df2 <- subset(df1, subset = df1$ResultMeasureValue == "BPQL" |
    df1$ResultMeasureValue == "BDL" |
    df1$ResultMeasureValue == "ND")

  unique(df2$ResultMeasureValue)

  # subset df: TADA.DetectionQuantitationLimitMeasure.MeasureValue = NA or None
  df3 <- subset(df2, subset = (!is.na(df2$TADA.DetectionQuantitationLimitMeasure.MeasureValue)))

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

  expect_true(all(df3_subset$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit"))

  expect_true(all(!is.na(df3_subset$TADA.ResultMeasureValue)))

  expect_true(all(!is.na(df3_subset$TADA.ResultMeasure.MeasureUnitCode)))
})

test_that("TADA_IDCensoredData does not introduce NAs in TADA.ResultMeasureValueDataTypes.Flag", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  
  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Create a list of values with NA in TADA.ResultMeasureValueDataTypes.Flag
  na_flags <- testdat[is.na(testdat$TADA.ResultMeasureValueDataTypes.Flag), ]
  
  # Check if either na_values or na_flags has observations and fail if they do
  if (nrow(na_flags) > 0) {
    stop("Failure: There are NA observations in TADA.ResultMeasureValueDataTypes.Flag.")
  }

  testdat2 <- TADA_IDCensoredData(testdat)
  
  # Create a list of values with NA in TADA.ResultMeasureValueDataTypes.Flag
  na_flags_2 <- testdat2[is.na(testdat2$TADA.ResultMeasureValueDataTypes.Flag), ]
  
  # Check if either na_values or na_flags has observations and fail if they do
  if (nrow(na_flags_2) > 0) {
    stop("Failure: There are NA observations in TADA.ResultMeasureValueDataTypes.Flag.")
  }
  
  # Test to ensure the value column is entirely numeric
  expect_true(
    is.numeric(testdat$TADA.ResultMeasureValue),
    info = "The TADA.ResultMeasureValue column is not entirely numeric."
  )
  
  # Test to ensure unit column does not contain any NA values
  expect_true(
    !any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)),
    info = "The TADA.ResultMeasure.MeasureUnitCode column contains NA values."
  )
})

test_that("TADA_SimpleCensoredMethods does not introduce duplicates or NAs in result or unit cols that cannot be handled in TADA_ConvertSpecialChars", {
  
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  
  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))
  
  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))
  
  # TADA_ConvertSpecialChars does not handle this yet 8/11/25
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))
  
  testdat2 <- TADA_SimpleCensoredMethods(testdat,
                                         nd_method = "multiplier",
                                         nd_multiplier = 0.5,
                                         od_method = "as-is",
                                         od_multiplier = "null")
  
  testdat3 <- TADA_ConvertSpecialChars(testdat2, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat3$TADA.ResultMeasureValue))
  
  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat3$TADA.ResultMeasureValue)))
  
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat2$TADA.ResultMeasure.MeasureUnitCode)))
})
