test_that("SuspectCoordinates works", {
  # flagonly
  SuspectCoord_flags <- TADA_FlagCoordinates(Data_Nutrients_UT)
  unique(SuspectCoord_flags$TADA.SuspectCoordinates)
  reviewselectcolumns <- SuspectCoord_flags %>% dplyr::select(TADA.SuspectCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  reviewflagsonly <- dplyr::filter(
    reviewselectcolumns,
    is.na(TADA.SuspectCoordinates.Flag) != TRUE
  )
  unique(reviewflagsonly$TADA.SuspectCoordinates.Flag)

  # removeimprecise
  ImpreciseCoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_imprecise = TRUE)
  unique(ImpreciseCoord_removed$TADA.SuspectCoordinates.Flag)

  expect_true(any(ImpreciseCoord_removed$TADA.SuspectCoordinates.Flag != "Imprecise_lessthan3decimaldigits"))

  # Remove data with coordinates outside the USA, but keep flagged data with imprecise coordinates:
  OutsideUSACoord_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove")
  unique(OutsideUSACoord_removed$TADA.SuspectCoordinates.Flag)

  expect_true(any(OutsideUSACoord_removed$TADA.SuspectCoordinates.Flag != "LONG_OutsideUSA" |
    OutsideUSACoord_removed$TADA.SuspectCoordinates.Flag != "LAT_OutsideUSA"))

  ## Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
  Suspect_removed <- TADA_FlagCoordinates(Data_Nutrients_UT, clean_outsideUSA = "remove", clean_imprecise = TRUE)
  unique(Suspect_removed$TADA.SuspectCoordinates.Flag)
})


test_that("Imprecise_lessthan3decimaldigits works", {
  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Data_Nutrients_UT)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.SuspectCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.SuspectCoordinates.Flag == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 3) %>% dplyr::distinct()

  expect_true(all(sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 4))
})

test_that("Imprecise_lessthan3decimaldigits works again", {
  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Data_Nutrients_UT)
  FLAGSONLY <- FLAGSONLY %>% dplyr::select(TADA.SuspectCoordinates.Flag, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
  FLAGSONLY <- dplyr::filter(FLAGSONLY, FLAGSONLY$TADA.SuspectCoordinates.Flag == "Imprecise_lessthan3decimaldigits")
  FLAGSONLY <- dplyr::filter(FLAGSONLY, sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 3) %>% dplyr::distinct()

  expect_true(all(sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 4))
})

test_that("No NA's in independent flag columns", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  testdat <- TADA_ConvertResultUnits(testdat, transform = TRUE)

  testdat <- suppressWarnings(TADA_FlagMethod(testdat, clean = FALSE, flaggedonly = FALSE))
  expect_false(any(is.na(testdat$TADA.AnalyticalMethod.Flag)))

  testdat <- TADA_FlagContinuousData(testdat, clean = FALSE, flaggedonly = FALSE)
  expect_false(any(is.na(testdat$TADA.ContinuousData.Flag)))

  testdat <- TADA_FlagAboveThreshold(testdat, clean = FALSE, flaggedonly = FALSE)
  expect_false(any(is.na(testdat$TADA.ResultValueAboveUpperThreshold.Flag)))

  testdat <- TADA_FlagBelowThreshold(testdat, clean = FALSE, flaggedonly = FALSE)
  expect_false(any(is.na(testdat$TADA.ResultValueBelowLowerThreshold.Flag)))

  testdat <- TADA_FindQAPPDoc(testdat, clean = FALSE)
  expect_false(any(is.na(testdat$TADA_FindQAPPDoc)))
})

# takes too long
# test_that("TADA_FindPotentialDuplicates functions do not grow dataset", {
#   testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
#   testdat1 <- TADA_FindPotentialDuplicatesSingleOrg(testdat)
#   testdat2 <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)
#   expect_true(dim(testdat)[1] == dim(testdat1)[1])
#   expect_true(dim(testdat)[1] == dim(testdat2)[1])
# })

# takes too long
# test_that("TADA_FindPotentialDuplicatsMultipleOrgs labels nearby site and multiple org groupings incrementally if duplicates are found", {
#   testdat <- TADA_RandomTestingData()
#   testdat <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)
#
#   testdat1 <- testdat %>%
#     dplyr::select(TADA.NearbySiteGroup) %>%
#     dplyr::distinct() %>%
#     dplyr::pull() %>%
#     as.numeric() %>%
#     sort()
#
#   testdat2 <- testdat %>%
#     dplyr::select(TADA.MultipleOrgDupGroupID) %>%
#     dplyr::filter(TADA.MultipleOrgDupGroupID != "Not a duplicate") %>%
#     unique() %>%
#     dplyr::pull() %>%
#     as.numeric() %>%
#     sort()
#
#   expect_true(length(unique(diff(testdat1))) < 2 | length(testdat1 == 0))
#
#   expect_true(length(unique(diff(testdat2))) < 2 | length(testdat2 == 0))
# })

# takes too long
# test_that("TADA_FindPotentialDuplicatsMultipleOrgs has non-NA values for each row in columns added in function", {
#   testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
#   testdat <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)
#
#   expect_false(any(is.na(testdat$TADA.MultipleOrgDupGroupID)))
#   expect_false(any(is.na(testdat$TADA.MultipleOrgDuplicate)))
#   expect_false(any(is.na(testdat$TADA.MonitoringLocationIdentifier)))
#   expect_false(any(is.na(testdat$TADA.ResultSelectedMultipleOrgs)))
# })

test_that("range flag functions work", {
  # use random data
  upper <- TADA_RandomTestingData(choose_random_state = TRUE)

  expect_no_error(TADA_FlagAboveThreshold(upper))
  expect_no_warning(TADA_FlagAboveThreshold(upper))
  expect_no_message(TADA_FlagAboveThreshold(upper))
  expect_no_condition(TADA_FlagAboveThreshold(upper))

  expect_no_error(TADA_FlagBelowThreshold(upper))
  expect_no_warning(TADA_FlagBelowThreshold(upper))
  expect_no_message(TADA_FlagBelowThreshold(upper))
  expect_no_condition(TADA_FlagBelowThreshold(upper))
})


test_that("QC results are not flagged as Continuous", {
  cont_QC <- TADA_RandomTestingData(choose_random_state = TRUE) %>%
    TADA_FlagContinuousData() 
  
  cont_QC_filt <- cont_QC %>%
    dplyr::filter(TADA.ContinuousData.Flag == "Continuous")
  
  cont_QC_disc <- cont_QC %>%
    dplyr::filter(TADA.ContinuousData.Flag == "Discrete")

  if(nrow(cont_QC_filt) > 0) {
  
  expect_true(!(unique(cont_QC_filt$TADA.ActivityType.Flag)) %in% c(
    "QC_duplicate", "QC_calibration",
    "QC_replicate", "QC_blank",
    "QC_other"
  ))
  }
  
  if(nrow(cont_QC_filt) == 0) {
    expect_true(nrow(cont_QC_disc) > 0)
  }
})
