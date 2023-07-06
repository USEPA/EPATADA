test_that("InvalidCoordinates works", {
  # use example dataset
  data(Nutrients_Utah)

  # flagonly
  InvalidCoord_flags <- TADA_FlagCoordinates(Nutrients_Utah)
  unique(InvalidCoord_flags$TADA.InvalidCoordinates)
  reviewselectcolumns <- InvalidCoord_flags %>% dplyr::select(TADA.InvalidCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  reviewflagsonly <- dplyr::filter(
    reviewselectcolumns,
    is.na(TADA.InvalidCoordinates.Flag) != TRUE
  )
  unique(reviewflagsonly$TADA.InvalidCoordinates.Flag)

  # removeimprecise
  ImpreciseCoord_removed <- TADA_FlagCoordinates(Nutrients_Utah, clean_imprecise = TRUE)
  unique(ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag)

  expect_true(any(ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag != "Imprecise_Latincludes999" |
    ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag != "Imprecise_Longincludes999" |
    ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag != "Imprecise_lessthan3decimaldigits"))

  # Remove data with coordinates outside the USA, but keep flagged data with imprecise coordinates:
  OutsideUSACoord_removed <- TADA_FlagCoordinates(Nutrients_Utah, clean_outsideUSA = "remove")
  unique(OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag)

  expect_true(any(OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag != "LONG_OutsideUSA" |
    OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag != "LAT_OutsideUSA"))

  ## Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
  Invalid_removed <- TADA_FlagCoordinates(Nutrients_Utah, clean_outsideUSA = "remove", clean_imprecise = TRUE)
  unique(Invalid_removed$TADA.InvalidCoordinates.Flag)
})


test_that("Imprecise_lessthan3decimaldigits works", {
  # use example dataset
  data(Nutrients_Utah)

  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Nutrients_Utah)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.InvalidCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.InvalidCoordinates.Flag == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 3) %>% dplyr::distinct()

  expect_true(all(sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 4))
})

test_that("Imprecise_lessthan3decimaldigits works again", {
  # use example dataset
  data(Nutrients_Utah)

  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Nutrients_Utah)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.InvalidCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.InvalidCoordinates.Flag == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 3) %>% dplyr::distinct()

  expect_true(all(sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 4))
})

test_that("No NA's in independent flag columns", {
  today <- Sys.Date()
  twoago <- as.character(today - 2 * 365)
  testdat <- TADA_DataRetrieval(statecode = "UT", startDate = twoago, characteristicName = c("Nitrate", "Copper"), sampleMedia = "Water")
  testdat <- TADA_ConvertResultUnits(testdat, transform = TRUE)

  testdat <- suppressWarnings(TADA_FlagMethod(testdat, clean = FALSE, errorsonly = FALSE))
  expect_false(any(is.na(testdat$TADA.AnalyticalMethod.Flag)))

  testdat <- TADA_FindContinuousData(testdat, clean = FALSE, errorsonly = FALSE)
  expect_false(any(is.na(testdat$TADA.AggregatedContinuousData.Flag)))

  testdat <- TADA_FlagAboveThreshold(testdat, clean = FALSE, errorsonly = FALSE)
  expect_false(any(is.na(testdat$TADA.ResultValueAboveUpperThreshold.Flag)))

  testdat <- TADA_FlagBelowThreshold(testdat, clean = FALSE, errorsonly = FALSE)
  expect_false(any(is.na(testdat$TADA.ResultValueBelowLowerThreshold.Flag)))

  testdat <- TADA_FindQAPPDoc(testdat, clean = FALSE)
  expect_false(any(is.na(testdat$TADA_FindQAPPDoc)))
})
