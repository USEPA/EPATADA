test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Unit tests for DepthProfileData function
# Dataframe fixture
ActivityDepthHeightMeasure.MeasureValue <- c(2.0, 1)
ActivityDepthHeightMeasure.MeasureUnitCode <- c("m", "ft")
ActivityTopDepthHeightMeasure.MeasureValue <- c(NaN, NaN)
ActivityTopDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
ActivityBottomDepthHeightMeasure.MeasureValue <- c(NaN, NaN)
ActivityBottomDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
ResultDepthHeightMeasure.MeasureValue <- c(NaN, NaN)
ResultDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
ActivityEndTime.TimeZoneCode <- c(NaN, NaN)
TADAProfile <- data.frame(ActivityDepthHeightMeasure.MeasureValue,
                          ActivityDepthHeightMeasure.MeasureUnitCode,
                          ActivityTopDepthHeightMeasure.MeasureValue,
                          ActivityTopDepthHeightMeasure.MeasureUnitCode,
                          ActivityBottomDepthHeightMeasure.MeasureValue,
                          ActivityBottomDepthHeightMeasure.MeasureUnitCode,
                          ResultDepthHeightMeasure.MeasureValue,
                          ResultDepthHeightMeasure.MeasureUnitCode,
                          ActivityEndTime.TimeZoneCode)

# When you pass a string instead of class(.data) should get an error
test_that("DepthProfileData catches non-dataframe", {
  expect_error(DepthProfileData("string"),
               "Input object must be of class 'data.frame'")
})

# When dataframe is missing columns
test_that("DepthProfileData catches non-dataframe", {
  # Drop by name
  TADAProfile2 <- dplyr::select(TADAProfile, -ActivityDepthHeightMeasure.MeasureValue)
  err <- "The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage."
  expect_error(DepthProfileData(TADAProfile2), err)
})

# When unit arg is not expected
test_that("DepthProfileData catches bad unit arg", {
  # Must escape regex special characters
  err <- "Invalid \'unit\' argument. \'unit\' must be either \'m\' \\(meter\\), \'ft\' \\(feet\\), or \'in\' \\(inch\\)\\."
  expect_error(DepthProfileData(TADAProfile, unit = "km"), err)
})

# When fields arg is not valid
test_that("DepthProfileData check fields argument for valid inputs", {
  expect_error(DepthProfileData(TADAProfile, fields = 'ActivityDepthHeightMeasure.MeasureValue'),
               "Invalid 'fields' argument. 'fields' must include one or many of the
    following: 'ActivityDepthHeightMeasure,' 'ActivityTopDepthHeightMeasure,'
    'ActivityBottomDepthHeightMeasure,' and/or 'ResultDepthHeightMeasure.'"
  )
})

# When all unit columns are null
test_that("DepthProfileData all value columns NaN", {
  # Replace units col w/ NaN
  TADAProfile$ActivityDepthHeightMeasure.MeasureUnitCode <- c(NaN, NaN)
  err <- "The dataset does not have any depth data."
  expect_error(DepthProfileData(TADAProfile), err)
})

# Conversion correct
test_that("DepthProfileData convert ft to m", {
  x = DepthProfileData(TADAProfile)
  actual = x$ActivityDepthHeightMeasure.MeasureValue[1]
  actual.unit <- x$ActivityDepthHeightMeasure.MeasureUnitCode[1]
  expect_equal(actual, 0.3048)
  expect_equal(actual.unit, 'm')
})