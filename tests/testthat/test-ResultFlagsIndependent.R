test_that("InvalidCoordinates works", {
  # use example dataset
  data(Data_Nutrients_UT)

  # flagonly
  InvalidCoord_flags <- TADA_FlagCoordinates(Data_Nutrients_UT)
  unique(InvalidCoord_flags$TADA.InvalidCoordinates)
  reviewselectcolumns <- InvalidCoord_flags %>% dplyr::select(TADA.InvalidCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  reviewflagsonly <- dplyr::filter(
    reviewselectcolumns,
    is.na(TADA.InvalidCoordinates.Flag) != TRUE
  )
  unique(reviewflagsonly$TADA.InvalidCoordinates.Flag)

  # removeimprecise
  ImpreciseCoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_imprecise = TRUE)
  unique(ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag)

  expect_true(any(ImpreciseCoord_removed$TADA.InvalidCoordinates.Flag != "Imprecise_lessthan3decimaldigits"))

  # Remove data with coordinates outside the USA, but keep flagged data with imprecise coordinates:
  OutsideUSACoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove")
  unique(OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag)

  expect_true(any(OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag != "LONG_OutsideUSA" |
    OutsideUSACoord_removed$TADA.InvalidCoordinates.Flag != "LAT_OutsideUSA"))

  ## Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
  Invalid_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove", clean_imprecise = TRUE)
  unique(Invalid_removed$TADA.InvalidCoordinates.Flag)
})


test_that("Imprecise_lessthan3decimaldigits works", {
  # use example dataset
  data(Data_Nutrients_UT)

  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Data_Nutrients_UT)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.InvalidCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.InvalidCoordinates.Flag == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 3) %>% dplyr::distinct()

  expect_true(all(sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 4))
})

test_that("Imprecise_lessthan3decimaldigits works again", {
  # use example dataset
  data(Data_Nutrients_UT)

  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Data_Nutrients_UT)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.InvalidCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.InvalidCoordinates.Flag == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 3) %>% dplyr::distinct()

  expect_true(all(sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 4))
})

test_that("No NA's in independent flag columns", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  testdat <- TADA_ConvertResultUnits(testdat, transform = TRUE)

  testdat <- suppressWarnings(TADA_FlagMethod(testdat, clean = FALSE, flaggedonly = FALSE))
  expect_false(any(is.na(testdat$TADA.AnalyticalMethod.Flag)))

  testdat <- TADA_FindContinuousData(testdat, clean = FALSE, flaggedonly = FALSE)
  expect_false(any(is.na(testdat$TADA.AggregatedContinuousData.Flag)))

  testdat <- TADA_FlagAboveThreshold(testdat, clean = FALSE, flaggedonly = FALSE)
  expect_false(any(is.na(testdat$TADA.ResultValueAboveUpperThreshold.Flag)))

  testdat <- TADA_FlagBelowThreshold(testdat, clean = FALSE, flaggedonly = FALSE)
  expect_false(any(is.na(testdat$TADA.ResultValueBelowLowerThreshold.Flag)))

  testdat <- TADA_FindQAPPDoc(testdat, clean = FALSE)
  expect_false(any(is.na(testdat$TADA_FindQAPPDoc)))
})

test_that("TADA_FindPotentialDuplicates functions do not grow dataset", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  testdat1 <- TADA_FindPotentialDuplicatesSingleOrg(testdat)
  testdat2 <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)
  expect_true(dim(testdat)[1] == dim(testdat1)[1])
  expect_true(dim(testdat)[1] == dim(testdat2)[1])
})

test_that("TADA_FindPotentialDuplicatsMultipleOrgs labels nearby site and multiple org groupings incrementally if duplicates are found", {
  testdat <- TADA_RandomTestingData()
  testdat <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  testdat1 <- testdat %>%
    dplyr::select(TADA.NearbySiteGroups) %>%
    dplyr::filter(TADA.NearbySiteGroups != "No nearby sites") %>%
    tidyr::separate_rows(TADA.NearbySiteGroups, sep = ", ") %>%
    dplyr::pull() %>%
    stringr::str_remove_all("Group_") %>%
    unique() %>%
    as.numeric() %>%
    sort()

  testdat2 <- testdat %>%
    dplyr::select(TADA.MultipleOrgDupGroupID) %>%
    dplyr::filter(TADA.MultipleOrgDupGroupID != "Not a duplicate") %>%
    unique() %>%
    dplyr::pull() %>%
    as.numeric() %>%
    sort()

  expect_true(unique(diff(testdat1)) == 1 | NA)

  expect_true(unique(diff(testdat2)) == 1 | NA)
})

test_that("TADA_FindPotentialDuplicatsMultipleOrgs has non-NA values for each row in columns added in function", {
  testdat <- TADA_RandomTestingData()
  testdat <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  expect_false(any(is.na(testdat$TADA.MultipleOrgDupGroupID)))
  expect_false(any(is.na(testdat$TADA.MultipleOrgDuplicate)))
  expect_false(any(is.na(testdat$TADA.NearbySiteGroups)))
  expect_false(any(is.na(testdat$TADA.ResultSelectedMultipleOrgs)))
})