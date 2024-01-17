test_that("TADA_IDCensoredData orphans", {
  cens.check <- TADA_DataRetrieval(statecode = "CO", startDate = "2021-01-01", endDate = "2022-01-01", characteristicName = c("Phosphorus", "Nitrate"))
  expect_true(all(!is.na(cens.check$TADA.CensoredData.Flag)))
})

test_that("TADA_SimpleCensoredMethods doesn't drop data", {
  testdat <- TADA_DataRetrieval(statecode = "KS", startDate = "2021-01-01", endDate = "2022-01-01", characteristicName = c("Phosphorus", "Nitrate"))
  cens.check <- TADA_SimpleCensoredMethods(testdat)
  expect_equal(dim(testdat)[1], dim(cens.check)[1])
})

test_that("TADA_IDCensoredData copies det lim values to result values if applicable", {
  copycheck <- TADA_RandomTestingData(choose_random_state = TRUE)

  copycheck1 <- TADA_IDCensoredData(copycheck)

  # let's look only at rows where the original result value = NA
  copycheck2 <- subset(copycheck1, subset = is.na(copycheck1$ResultMeasureValue))

  # the TADA.ResultMeasureValueDataTypes.Flag should = one of these two options
  expect_true(all(copycheck2$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit" |
    copycheck2$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available"))

  # subset df: TADA.DetectionQuantitationLimitMeasure.MeasureValue = NA or None
  copycheck_NAs <- subset(copycheck2, subset = (!is.na(copycheck2$TADA.DetectionQuantitationLimitMeasure.MeasureValue)))

  # for this subset, the TADA.ResultMeasureValueDataTypes.Flag should equal "Result Value/Unit Copied from Detection Limit"
  expect_true(all((copycheck_NAs$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit") &
    !is.na(copycheck_NAs$TADA.ResultMeasureValue)))

  # subset df: TADA.DetectionQuantitationLimitMeasure.MeasureValue does NOT = NA or None
  copycheck_copies <- subset(copycheck2, subset = (is.na(copycheck2$TADA.DetectionQuantitationLimitMeasure.MeasureValue)))

  # for this subset, the TADA.ResultMeasureValueDataTypes.Flag should equal "NA - Not Available"
  expect_true(all((copycheck_copies$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available") &
    is.na(copycheck_copies$TADA.ResultMeasureValue)))
})

test_that("TADA_IDCensoredData correctly handles specific text values such as ND", {
  df <- TADA_DataRetrieval(
    startDate = "2022-12-19",
    endDate = "2022-12-20"
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
    TADA.ActivityType.Flag,
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
