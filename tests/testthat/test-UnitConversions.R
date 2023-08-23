# Unit tests for TADA_ConvertDepthUnits function
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

# When you pass a string instead of class(.data) should get an error
test_that("TADA_ConvertDepthUnits catches non-dataframe", {
  expect_error(
    TADA_ConvertDepthUnits("string"),
    "Input object must be of class 'data.frame'"
  )
})

# When dataframe is missing columns
test_that("TADA_ConvertDepthUnits catches non-dataframe", {
  # Drop by name
  TADAProfile2 <- dplyr::select(TADAProfile, -ActivityDepthHeightMeasure.MeasureValue)
  err <- "The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage."
  expect_error(TADA_ConvertDepthUnits(TADAProfile2), err)
})

# When unit arg is not expected
test_that("TADA_ConvertDepthUnits catches bad unit arg", {
  err <- "Invalid 'unit' argument. 'unit' must be either 'm' (meter), 'ft' (feet), or 'in' (inch)."
  # Fixed = TRUE avoids dealing with regex
  expect_error(TADA_ConvertDepthUnits(TADAProfile, unit = "km"), err, fixed = TRUE)
})

# When fields arg is not valid
test_that("TADA_ConvertDepthUnits check fields argument for valid inputs", {
  expect_error(
    TADA_ConvertDepthUnits(TADAProfile, fields = "ActivityDepthHeightMeasure.MeasureValue"),
    "Invalid 'fields' argument. 'fields' must include one or many of the
    following: 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
    'ActivityBottomDepthHeightMeasure,' and/or 'ResultDepthHeightMeasure.'"
  )
})

# When all unit columns are null
test_that("TADA_ConvertDepthUnits all value columns NaN", {
  # Replace units col w/ NaN
  TADAProfile$ActivityDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
  printy <- "No action taken: the dataframe does not have any depth data in ActivityTop/BottomDepthHeight or ResultDepthHeight columns."
  expect_output(TADA_ConvertDepthUnits(TADAProfile), printy)
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
    startDate = "2023-01-01"
  )
  check_depth_meters <- TADA_ConvertDepthUnits(check_depth_meters)
  expect_false("meters" %in% check_depth_meters$TADA.ActivityDepthHeightMeasure.MeasureUnitCode)
})
