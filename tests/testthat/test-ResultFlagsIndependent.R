test_that("SuspectCoordinates works", {
  # flagonly
  SuspectCoord_flags <- TADA_FlagCoordinates(Data_Nutrients_UT)
  unique(SuspectCoord_flags$TADA.SuspectCoordinates)
  reviewselectcolumns <- SuspectCoord_flags |>
    dplyr::select(
      TADA.SuspectCoordinates.Flag,
      TADA.LatitudeMeasure,
      TADA.LongitudeMeasure
    )
  reviewflagsonly <- dplyr::filter(
    reviewselectcolumns,
    is.na(TADA.SuspectCoordinates.Flag) != TRUE
  )
  unique(reviewflagsonly$TADA.SuspectCoordinates.Flag)

  # removeimprecise
  ImpreciseCoord_removed <- TADA_FlagCoordinates(
    Data_Nutrients_UT,
    clean_imprecise = TRUE
  )
  unique(ImpreciseCoord_removed$TADA.SuspectCoordinates.Flag)

  expect_true(any(
    ImpreciseCoord_removed$TADA.SuspectCoordinates.Flag !=
      "Imprecise_lessthan3decimaldigits"
  ))

  # Remove data with coordinates outside the USA, but keep flagged data with imprecise coordinates:
  OutsideUSACoord_removed <- TADA_FlagCoordinates(
    Data_Nutrients_UT,
    clean_outsideUSA = "remove"
  )
  unique(OutsideUSACoord_removed$TADA.SuspectCoordinates.Flag)

  expect_false(any(OutsideUSACoord_removed$TADA.SuspectCoordinates.Flag %in% c(
    "LONG_OutsideUSA",
    "LAT_OutsideUSA"
  )))
  
  expect_false(any(ImpreciseCoord_removed$TADA.SuspectCoordinates.Flag == "Imprecise_lessthan3decimaldigits"))

  ## Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
  Suspect_removed <- TADA_FlagCoordinates(
    Data_Nutrients_UT,
    clean_outsideUSA = "remove",
    clean_imprecise = TRUE
  )
  unique(Suspect_removed$TADA.SuspectCoordinates.Flag)
})


test_that("Imprecise_lessthan3decimaldigits works", {
  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Data_Nutrients_UT)
  FLAGSONLY <- FLAGSONLY |>
    dplyr::select(
      TADA.SuspectCoordinates.Flag,
      TADA.LatitudeMeasure,
      TADA.LongitudeMeasure
    )
  FLAGSONLY <- dplyr::filter(
    FLAGSONLY,
    FLAGSONLY$TADA.SuspectCoordinates.Flag == "Imprecise_lessthan3decimaldigits"
  )
  FLAGSONLY <- dplyr::filter(
    FLAGSONLY,
    sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 3
  ) |>
    dplyr::distinct()

  expect_true(all(
    sapply(FLAGSONLY$TADA.LongitudeMeasure, TADA_DecimalPlaces) < 4
  ))
})

test_that("Imprecise_lessthan3decimaldigits works again", {
  # flagonly
  FLAGSONLY <- TADA_FlagCoordinates(Data_Nutrients_UT)
  FLAGSONLY <- FLAGSONLY |>
    dplyr::select(
      TADA.SuspectCoordinates.Flag,
      TADA.LatitudeMeasure,
      TADA.LongitudeMeasure
    )
  FLAGSONLY <- dplyr::filter(
    FLAGSONLY,
    FLAGSONLY$TADA.SuspectCoordinates.Flag == "Imprecise_lessthan3decimaldigits"
  )
  FLAGSONLY <- dplyr::filter(
    FLAGSONLY,
    sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 3
  ) |>
    dplyr::distinct()

  expect_true(all(
    sapply(FLAGSONLY$TADA.LatitudeMeasure, TADA_DecimalPlaces) < 4
  ))
})

test_that("No NAs in independent flag columns", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  testdat <- TADA_ConvertResultUnits(testdat, transform = TRUE)

  testdat <- suppressWarnings(TADA_FlagMethod(
    testdat,
    clean = FALSE,
    flaggedonly = FALSE
  ))
  expect_false(any(is.na(testdat$TADA.AnalyticalMethod.Flag)))

  testdat <- TADA_FlagContinuousData(
    testdat,
    clean = FALSE,
    flaggedonly = FALSE
  )
  expect_false(any(is.na(testdat$TADA.ContinuousData.Flag)))

  testdat <- TADA_FlagAboveThreshold(
    testdat,
    clean = FALSE,
    flaggedonly = FALSE
  )
  expect_false(any(is.na(testdat$TADA.ResultValueAboveUpperThreshold.Flag)))

  testdat <- TADA_FlagBelowThreshold(
    testdat,
    clean = FALSE,
    flaggedonly = FALSE
  )
  expect_false(any(is.na(testdat$TADA.ResultValueBelowLowerThreshold.Flag)))

  testdat <- TADA_FindQAPPDoc(testdat, clean = FALSE)
  expect_false(any(is.na(testdat$TADA.QAPPDocAvailable)))
})

test_that("TADA_FindPotentialDuplicates functions do not grow dataset", {
  testdat <- Data_R5_TADAPackageDemo |> dplyr::filter(StateCode == "17")

  # Skip the test if the test dataframe is empty
  if (dim(testdat)[1] == 0) {
    skip("Test dataframe is empty, skipping test.")
  }

  testdat1 <- TADA_FindPotentialDuplicatesSingleOrg(testdat)
  testdat2 <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  expect_equal(nrow(testdat), nrow(testdat1))
  expect_equal(nrow(testdat), nrow(testdat2))
})

test_that("TADA_FindPotentialDuplicatesMultipleOrgs labels nearby site and multiple org groupings incrementally if duplicates are found", {
  testdat <- Data_R5_TADAPackageDemo
  testdat <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat) |>
    dplyr::filter(StateCode == "17")

  testdat1 <- testdat |>
    dplyr::select(TADA.NearbySiteGroup) |>
    dplyr::distinct() |>
    dplyr::pull() |>
    as.numeric() |>
    sort()

  testdat2 <- testdat |>
    dplyr::select(TADA.MultipleOrgDupGroupID) |>
    dplyr::filter(TADA.MultipleOrgDupGroupID != "Not a Duplicate") |>
    unique() |>
    dplyr::pull() |>
    as.numeric() |>
    sort()

  expect_true(length(unique(diff(testdat1))) < 2 | length(testdat1 == 0))

  expect_true(length(unique(diff(testdat2))) < 2 | length(testdat2 == 0))
})

test_that("TADA_FindPotentialDuplicatesMultipleOrgs has non-NA values for added columns", {
  testdat <- Data_R5_TADAPackageDemo |> dplyr::filter(StateCode == "17")
  
  testthat::skip_if(
    is.null(testdat) || NROW(testdat) == 0,
    "Empty test data; skipping test."
  )
  
  testdat <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)
  
  expect_false(any(is.na(testdat$TADA.MultipleOrgDupGroupID)))
  expect_false(any(is.na(testdat$TADA.MultipleOrgDup.Flag)))
})

test_that("WQXcharValRef.rda contains only one row for each unique characteristic/source/unit combination for threshold functions", {
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  rm(file_path)

  unit.ref <- dplyr::filter(
    WQXcharValRef,
    Type == "CharacteristicUnit",
    Status == "Accepted"
  )

  find.dups <- unit.ref |>
    dplyr::filter(Type == "CharacteristicUnit") |>
    dplyr::group_by(Characteristic, Source, Value.Unit) |>
    dplyr::mutate(
      Min_n = length(unique(Minimum)),
      Max_n = length(unique(Maximum))
    ) |>
    dplyr::filter(Min_n > 1 | Max_n > 1)

  expect_true(nrow(find.dups) == 0)
})


test_that("range flag functions work", {
  # use random data
  upper <- TADA_RandomTestingData(choose_random_state = TRUE)

  expect_no_error(TADA_FlagAboveThreshold(upper))
  expect_no_warning(TADA_FlagAboveThreshold(upper))

  expect_no_error(TADA_FlagBelowThreshold(upper))
  expect_no_warning(TADA_FlagBelowThreshold(upper))
})


test_that("QC results are not flagged as Continuous", {
  cont_QC <- TADA_RandomTestingData(choose_random_state = TRUE) |>
    TADA_FlagContinuousData()

  cont_QC_filt <- cont_QC |>
    dplyr::filter(TADA.ContinuousData.Flag == "Continuous")

  cont_QC_disc <- cont_QC |>
    dplyr::filter(TADA.ContinuousData.Flag == "Discrete")

  if (nrow(cont_QC_filt) > 0) {
    expect_true(
      !(unique(cont_QC_filt$TADA.ActivityType.Flag)) %in%
        c(
          "QC_duplicate",
          "QC_calibration",
          "QC_replicate",
          "QC_blank",
          "QC_other"
        )
    )
  }

  if (nrow(cont_QC_filt) == 0) {
    expect_true(nrow(cont_QC_disc) > 0)
  }
})

test_that("TADA_FindPotentialDuplicatesSingleOrg has non-NA values for each row in columns added in function", {
  testdat <- Data_R5_TADAPackageDemo |> dplyr::filter(StateCode == "17")
  
  testthat::skip_if(
    is.null(testdat) || NROW(testdat) == 0,
    "Empty test data; skipping test."
  )
  
  testdat <- TADA_FindPotentialDuplicatesSingleOrg(testdat)
  
  expect_true("TADA.SingleOrgDupGroupID" %in% names(testdat))
  expect_true("TADA.SingleOrgDup.Flag" %in% names(testdat))
  
  expect_false(any(is.na(testdat$TADA.SingleOrgDupGroupID)))
  expect_false(any(is.na(testdat$TADA.SingleOrgDup.Flag)))
})

test_that("TADA_FindQAPPApproval filters Y, N, and NA correctly", {
  dat <- data.frame(
    QAPPApprovedIndicator = c("Y", "N", NA)
  )
  
  res1 <- TADA_FindQAPPApproval(dat, clean = FALSE, cleanNA = FALSE, flaggedonly = FALSE)
  expect_equal(nrow(res1), 3)
  
  res2 <- TADA_FindQAPPApproval(dat, clean = TRUE, cleanNA = FALSE, flaggedonly = FALSE)
  expect_equal(res2$QAPPApprovedIndicator, c("Y", NA))
  
  res3 <- TADA_FindQAPPApproval(dat, clean = TRUE, cleanNA = TRUE, flaggedonly = FALSE)
  expect_equal(res3$QAPPApprovedIndicator, "Y")
  
  res4 <- TADA_FindQAPPApproval(dat, clean = FALSE, cleanNA = TRUE, flaggedonly = TRUE)
  expect_equal(res4$QAPPApprovedIndicator, "N")
})

test_that("TADA_FlagAboveThreshold treats threshold equality as Pass", {
  file_path <- system.file("extdata", "WQXcharValRef.rda", package = "EPATADA")
  load(file_path)
  
  ref_row <- dplyr::filter(
    WQXcharValRef,
    Type == "CharacteristicUnit",
    TADA.WQXVal.Flag == "Pass",
    !is.na(Maximum)
  ) |>
    dplyr::slice(1)
  
  dat <- data.frame(
    TADA.CharacteristicName = ref_row$Characteristic,
    TADA.ActivityMediaName = ref_row$Source,
    TADA.ResultMeasureValue = ref_row$Maximum,
    TADA.ResultMeasure.MeasureUnitCode = ref_row$Value.Unit
  )
  
  res <- TADA_FlagAboveThreshold(dat)
  expect_equal(res$TADA.ResultValueAboveUpperThreshold.Flag, "Pass")
})
