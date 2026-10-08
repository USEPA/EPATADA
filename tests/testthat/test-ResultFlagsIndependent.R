test_that("SuspectCoordinates works", {
  # Flag suspect coordinates
  SuspectCoord_flags <- TADA_FlagCoordinates(Data_Nutrients_UT)

  expect_true("TADA.SuspectCoordinates.Flag" %in% names(SuspectCoord_flags))

  expect_false(any(is.na(SuspectCoord_flags$TADA.SuspectCoordinates.Flag)))

  # Remove imprecise coordinates
  ImpreciseCoord_removed <- TADA_FlagCoordinates(
    Data_Nutrients_UT,
    clean_imprecise = TRUE
  )

  expect_false(any(stringr::str_detect(
    ImpreciseCoord_removed$TADA.SuspectCoordinates.Flag,
    stringr::fixed("Imprecise_lessthan3decimaldigits")
  )))

  # Remove data with coordinates outside the USA, but keep flagged data with imprecise coordinates:
  OutsideUSACoord_removed <- TADA_FlagCoordinates(
    Data_Nutrients_UT,
    clean_outsideUSA = "remove"
  )

  expect_false(any(stringr::str_detect(
    OutsideUSACoord_removed$TADA.SuspectCoordinates.Flag,
    "LAT_OutsideUSA|LONG_OutsideUSA"
  )))

  # Remove data with imprecise coordinates or coordinates outside the USA from the dataframe:
  Suspect_removed <- TADA_FlagCoordinates(
    Data_Nutrients_UT,
    clean_outsideUSA = "remove",
    clean_imprecise = TRUE
  )

  expect_false(any(stringr::str_detect(
    Suspect_removed$TADA.SuspectCoordinates.Flag,
    paste(
      "Imprecise_lessthan3decimaldigits",
      "LAT_OutsideUSA",
      "LONG_OutsideUSA",
      sep = "|"
    )
  )))
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
  expect_false(any(is.na(testdat$TADA_FindQAPPDoc)))
})

testthat::test_that("TADA_FindPotentialDuplicatesMultipleOrgs does not grow dataset - no duplicates", {
  testdat <- Data_R5_TADAPackageDemo |>
    dplyr::filter(StateCode == "17")

  if (nrow(testdat) == 0) {
    testthat::skip("Test dataframe is empty, skipping test.")
  }

  # Use a small subset for a stable unit test
  testdat <- testdat |>
    dplyr::slice_head(n = 10)

  # Mock nearby-sites detection so no rows are grouped as duplicates
  testthat::local_mocked_bindings(
    TADA_FindNearbySites = function(.data, dist_buffer = 100, org_hierarchy = "none") {
      .data |>
        dplyr::mutate(
          TADA.NearbySites.Flag = "Nearby",
          TADA.NearbySiteGroup = dplyr::row_number(),
          TADA.MonitoringLocationIdentifier = MonitoringLocationIdentifier
        )
    },
    .package = "EPATADA"
  )

  result <- EPATADA:::TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  testthat::expect_equal(nrow(result), nrow(testdat))
  testthat::expect_true(all(c(
    "TADA.MultipleOrgDup.Flag",
    "TADA.MultipleOrgDupGroupID"
  ) %in% names(result)))
  testthat::expect_true(all(result$TADA.MultipleOrgDup.Flag == "Not a Duplicate"))
})

testthat::test_that("TADA_FindPotentialDuplicatesMultipleOrgs does not grow dataset - forced duplicate group", {
  testdat <- Data_R5_TADAPackageDemo |>
    dplyr::filter(StateCode == "17")

  if (nrow(testdat) < 6) {
    testthat::skip("Not enough rows in test dataframe, skipping test.")
  }

  # Use a small subset for a stable unit test
  testdat <- testdat |>
    dplyr::slice_head(n = 6)

  # Force the rows into one nearby-site group and make 3 orgs share the same
  # comparable result values so the duplicate logic is exercised.
  testdat$TADA.NearbySites.Flag <- "Nearby"
  testdat$TADA.NearbySiteGroup <- c(1, 1, 1, 2, 3, 4)
  testdat$TADA.MonitoringLocationIdentifier <- testdat$MonitoringLocationIdentifier

  # Ensure first three rows satisfy the duplicate grouping conditions
  testdat$ActivityStartDate[1:3] <- testdat$ActivityStartDate[1]
  testdat$ActivityStartTime.Time[1:3] <- testdat$ActivityStartTime.Time[1]
  testdat$TADA.ComparableDataIdentifier[1:3] <- testdat$TADA.ComparableDataIdentifier[1]
  testdat$ActivityTypeCode[1:3] <- testdat$ActivityTypeCode[1]
  testdat$TADA.ResultMeasureValue[1:3] <- testdat$TADA.ResultMeasureValue[1]
  testdat$OrganizationIdentifier[1:3] <- c("ORG_A", "ORG_B", "ORG_C")
  testdat$ResultIdentifier[1:3] <- c("R1", "R2", "R3")

  # Since the nearby columns already exist, TADA_FindNearbySites won't be called
  result <- EPATADA:::TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  testthat::expect_equal(nrow(result), nrow(testdat))
  testthat::expect_true(all(c(
    "TADA.MultipleOrgDup.Flag",
    "TADA.MultipleOrgDupGroupID"
  ) %in% names(result)))
  testthat::expect_true(any(result$TADA.MultipleOrgDup.Flag == "Duplicate Selected"))
  testthat::expect_true(any(result$TADA.MultipleOrgDup.Flag == "Duplicate Not Selected"))
})

testthat::test_that("TADA_FindPotentialDuplicatesMultipleOrgs clean=TRUE removes not selected rows", {
  testdat <- Data_R5_TADAPackageDemo |>
    dplyr::filter(StateCode == "17")

  if (nrow(testdat) < 6) {
    testthat::skip("Not enough rows in test dataframe, skipping test.")
  }

  testdat <- testdat |>
    dplyr::slice_head(n = 6)

  testdat$TADA.NearbySites.Flag <- "Nearby"
  testdat$TADA.NearbySiteGroup <- c(1, 1, 1, 2, 3, 4)
  testdat$TADA.MonitoringLocationIdentifier <- testdat$MonitoringLocationIdentifier

  testdat$ActivityStartDate[1:3] <- testdat$ActivityStartDate[1]
  testdat$ActivityStartTime.Time[1:3] <- testdat$ActivityStartTime.Time[1]
  testdat$TADA.ComparableDataIdentifier[1:3] <- testdat$TADA.ComparableDataIdentifier[1]
  testdat$ActivityTypeCode[1:3] <- testdat$ActivityTypeCode[1]
  testdat$TADA.ResultMeasureValue[1:3] <- testdat$TADA.ResultMeasureValue[1]
  testdat$OrganizationIdentifier[1:3] <- c("ORG_A", "ORG_B", "ORG_C")
  testdat$ResultIdentifier[1:3] <- c("R1", "R2", "R3")

  result <- EPATADA:::TADA_FindPotentialDuplicatesMultipleOrgs(testdat, clean = TRUE)

  testthat::expect_true(nrow(result) == 4)
  testthat::expect_true(all(result$TADA.MultipleOrgDup.Flag != "Duplicate Not Selected"))
})



testthat::test_that("TADA_FindPotentialDuplicatesMultipleOrgs labels duplicate groups when multiple org duplicates are present", {
  testdat <- Data_R5_TADAPackageDemo |>
    dplyr::filter(StateCode == "17")

  testthat::skip_if(
    is.null(testdat) || NROW(testdat) == 0,
    "Empty test data; skipping test."
  )

  testdat <- testdat |>
    dplyr::slice_head(n = 6)

  # Create a known multi-org duplicate pattern in the first 3 rows
  testdat$ActivityStartDate[1:3] <- testdat$ActivityStartDate[1]
  testdat$ActivityStartTime.Time[1:3] <- testdat$ActivityStartTime.Time[1]
  testdat$TADA.ComparableDataIdentifier[1:3] <- testdat$TADA.ComparableDataIdentifier[1]
  testdat$ActivityTypeCode[1:3] <- testdat$ActivityTypeCode[1]
  testdat$TADA.ResultMeasureValue[1:3] <- testdat$TADA.ResultMeasureValue[1]
  testdat$OrganizationIdentifier[1:3] <- c("ORG_A", "ORG_B", "ORG_C")
  testdat$ResultIdentifier[1:3] <- c("R1", "R2", "R3")

  testdat <- testdat |>
    dplyr::mutate(
      TADA.NearbySites.Flag = "Nearby",
      TADA.NearbySiteGroup = dplyr::if_else(dplyr::row_number() <= 3, 1L, dplyr::row_number()),
      TADA.MonitoringLocationIdentifier = MonitoringLocationIdentifier
    )

  testdat2 <- EPATADA:::TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  testthat::expect_equal(nrow(testdat), nrow(testdat2))
  testthat::expect_true(any(testdat2$TADA.MultipleOrgDup.Flag == "Duplicate Selected"))
  testthat::expect_true(any(testdat2$TADA.MultipleOrgDup.Flag == "Duplicate Not Selected"))
  testthat::expect_true(any(testdat2$TADA.MultipleOrgDupGroupID != "Not a Duplicate"))
})


testthat::test_that("TADA_FindPotentialDuplicatesMultipleOrgs adds non-NA values in expected output columns", {
  testdat <- Data_R5_TADAPackageDemo |>
    dplyr::filter(StateCode == "17")

  testthat::skip_if(
    is.null(testdat) || NROW(testdat) == 0,
    "Empty test data; skipping test."
  )

  testdat <- testdat |>
    dplyr::slice_head(n = 6) |>
    dplyr::mutate(
      TADA.NearbySites.Flag = "Nearby",
      TADA.NearbySiteGroup = dplyr::if_else(dplyr::row_number() <= 3, 1L, dplyr::row_number()),
      TADA.MonitoringLocationIdentifier = MonitoringLocationIdentifier
    )

  testdat$ActivityStartDate[1:3] <- testdat$ActivityStartDate[1]
  testdat$ActivityStartTime.Time[1:3] <- testdat$ActivityStartTime.Time[1]
  testdat$TADA.ComparableDataIdentifier[1:3] <- testdat$TADA.ComparableDataIdentifier[1]
  testdat$ActivityTypeCode[1:3] <- testdat$ActivityTypeCode[1]
  testdat$TADA.ResultMeasureValue[1:3] <- testdat$TADA.ResultMeasureValue[1]
  testdat$OrganizationIdentifier[1:3] <- c("ORG_A", "ORG_B", "ORG_C")
  testdat$ResultIdentifier[1:3] <- c("R1", "R2", "R3")

  testdat2 <- EPATADA:::TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  testthat::expect_false(any(is.na(testdat2$TADA.MultipleOrgDupGroupID)))
  testthat::expect_false(any(is.na(testdat2$TADA.MultipleOrgDup.Flag)))
  testthat::expect_false(any(is.na(testdat2$TADA.MonitoringLocationIdentifier)))
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

test_that("check_location_metadata flags StateCode and CountyCode mismatches", {
  testdat <- dplyr::tibble(
    TADA.LatitudeMeasure = c(44.9509, 44.9509, 44.9509),
    TADA.LongitudeMeasure = c(-89.7590, -89.7590, -89.7590),
    StateCode = c("55", "17", "55"),
    CountyCode = c("073", "073", "067")
  )

  out <- TADA_FlagCoordinates(testdat, check_location_metadata = TRUE)

  expect_equal(out$TADA.SuspectCoordinates.Flag[1], "Pass")
  expect_equal(out$TADA.SuspectCoordinates.Flag[2], "Coordinate_StateMismatch")
  expect_equal(out$TADA.SuspectCoordinates.Flag[3], "Coordinate_CountyMismatch")
})

test_that("check_location_metadata flags StateCode and CountyCode mismatches", {
  testdat <- dplyr::tibble(
    TADA.LatitudeMeasure = c(44.9509, 44.9509, 44.9509, 44.95),
    TADA.LongitudeMeasure = c(-89.7590, -89.7590, -89.7590, -89.75),
    StateCode = c("55", "17", "55", "17"),
    CountyCode = c("073", "073", "067", "073")
  )

  out <- TADA_FlagCoordinates(testdat, check_location_metadata = TRUE)

  expect_equal(out$TADA.SuspectCoordinates.Flag[1], "Pass")

  expect_equal(out$TADA.SuspectCoordinates.Flag[2], "Coordinate_StateMismatch")

  expect_equal(out$TADA.SuspectCoordinates.Flag[3], "Coordinate_CountyMismatch")

  expect_true(stringr::str_detect(
    out$TADA.SuspectCoordinates.Flag[4],
    stringr::fixed("Imprecise_lessthan3decimaldigits")
  ))

  expect_true(stringr::str_detect(
    out$TADA.SuspectCoordinates.Flag[4],
    stringr::fixed("Coordinate_StateMismatch")
  ))
})
