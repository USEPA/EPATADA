# Dataframe fixture
ResultIdentifier <- c(21, 22)
ActivityDepthHeightMeasure.MeasureValue <- c(2.0, 1)
ActivityDepthHeightMeasure.MeasureUnitCode <- c("m", "ft")
ActivityTopDepthHeightMeasure.MeasureValue <- c(NaN, NaN)
ActivityTopDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
ActivityBottomDepthHeightMeasure.MeasureValue <- c(NaN, NaN)
ActivityBottomDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
ResultDepthHeightMeasure.MeasureValue <- c(NaN, NaN)
ResultDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
ActivityEndTime.TimeZoneCode <- c(NaN, NaN)

TADAProfile <- data.frame(
  ResultIdentifier,
  ActivityDepthHeightMeasure.MeasureValue,
  ActivityDepthHeightMeasure.MeasureUnitCode,
  ActivityTopDepthHeightMeasure.MeasureValue,
  ActivityTopDepthHeightMeasure.MeasureUnitCode,
  ActivityBottomDepthHeightMeasure.MeasureValue,
  ActivityBottomDepthHeightMeasure.MeasureUnitCode,
  ResultDepthHeightMeasure.MeasureValue,
  ResultDepthHeightMeasure.MeasureUnitCode,
  ActivityEndTime.TimeZoneCode
)

# Test: Passing a string instead of a dataframe
test_that("TADA_CheckColumns catches non-dataframe input", {
  expect_error(
    TADA_CheckColumns("string", c("A", "B")),
    "Input object must be of class 'data.frame'"
  )
})

# Test: Passing a non-character vector for expected columns
test_that("TADA_CheckColumns catches non-character expected columns", {
  expect_error(
    TADA_CheckColumns(TADAProfile, list("A", "B")),
    "Expected columns must be a character vector."
  )
})

# Test: Dataframe is missing required columns
test_that("TADA_CheckColumns catches missing columns", {
  # Drop required column by name
  TADAProfile2 <- dplyr::select(
    TADAProfile,
    -ActivityDepthHeightMeasure.MeasureValue
  )
  # pass a regular expression to expect_error() since error message can change
  expect_error(
    TADA_CheckColumns(
      TADAProfile2,
      c("ActivityDepthHeightMeasure.MeasureValue")
    ),
    regexp = "The dataframe does not contain the required field\\(s\\): ActivityDepthHeightMeasure.MeasureValue"
  )
})

# Test: All required columns are present
test_that("TADA_CheckColumns succeeds with all required columns present", {
  expect_silent(TADA_CheckColumns(
    TADAProfile,
    c(
      "ResultIdentifier",
      "ActivityDepthHeightMeasure.MeasureValue",
      "ActivityDepthHeightMeasure.MeasureUnitCode"
    )
  ))
})

# Test: No expected columns specified
test_that("TADA_CheckColumns succeeds with no expected columns", {
  expect_silent(TADA_CheckColumns(TADAProfile, character(0)))
})


# Test: Correct conversion of units
test_that("TADA_ConvertDepthUnits correctly converts units", {
  converted <- TADA_ConvertDepthUnits(TADAProfile)
  expect_equal(
    converted$TADA.ActivityDepthHeightMeasure.MeasureValue[2],
    0.3048
  )
})

# When unit arg is not expected
test_that("TADA_ConvertDepthUnits catches bad unit arg", {
  err <- "Invalid 'unit' argument. 'unit' must be either 'm' (meter), 'ft' (feet), or 'in' (inch)."
  # Fixed = TRUE avoids dealing with regex
  expect_error(
    TADA_ConvertDepthUnits(TADAProfile, unit = "km"),
    err,
    fixed = TRUE
  )
})

# Conversion correct
test_that("TADA_ConvertDepthUnits convert ft to m", {
  x <- TADA_ConvertDepthUnits(TADAProfile)
  actual <- x$TADA.ActivityDepthHeightMeasure.MeasureValue[2]
  actual.unit <- x$TADA.ActivityDepthHeightMeasure.MeasureUnitCode[2]
  expect_equal(actual, 0.3048)
  expect_equal(actual.unit, "m")
})

# meters to m in depth columns
test_that("TADA_ConvertDepthUnits converts meters to m", {
  check_depth_meters <- TADA_DataRetrieval(
    statecode = "UT",
    organization = "USGS-UT",
    characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
    startDate = "2023-01-01",
    endDate = "2023-03-01",
    ask = FALSE
  )
  check_depth_meters <- TADA_ConvertDepthUnits(check_depth_meters)
  expect_false(
    "meters" %in%
      check_depth_meters$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
  )
})

# Check that TADA_CreateUnitRef contains a row for each TADA.CharacteristicName,
# and ResultMeasure.MeasureUnitCode
test_that("TADA_CreateUnitRef output contains a row for each TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, and ResultMeasure.MeasureUnitCode.", {
  testdat <- TADA_RandomTestingData(
    number_of_days = 2,
    choose_random_state = TRUE
  )

  # Skip the test if the data retrieval results in an empty data frame
  if (nrow(testdat) == 0) {
    skip("Data retrieval failed after multiple attempts, skipping the test.")
  }

  unit.ref <- TADA_CreateUnitRef(testdat)
  unit.ref <- unit.ref |>
    dplyr::select(TADA.CharacteristicName, ResultMeasure.MeasureUnitCode) |>
    dplyr::distinct()

  unit.combs <- TADA_UniqueCharUnitSpeciation(testdat)
  unit.combs <- unit.combs |>
    dplyr::select(TADA.CharacteristicName, ResultMeasure.MeasureUnitCode) |>
    dplyr::distinct()

  compare <- unit.ref |> dplyr::anti_join(unit.combs)
  expect_true(nrow(compare) == 0)
})
