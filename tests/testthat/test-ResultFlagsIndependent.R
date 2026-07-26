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

test_that("TADA_FindPotentialDuplicates functions do not grow dataset", {
  testdat <- Data_R5_TADAPackageDemo |> dplyr::filter(StateCode == "17")

  # Skip the test if the test dataframe is empty
  if (dim(testdat)[1] == 0) {
    skip("Test dataframe is empty, skipping test.")
  }

  testdat1 <- TADA_FindPotentialDuplicatesSingleOrg(testdat)
  testdat2 <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)

  expect_true(dim(testdat)[1] == dim(testdat1)[1])
  expect_true(dim(testdat)[1] == dim(testdat2)[1])
})

test_that("TADA_FindPotentialDuplicatesMultipleOrgs labels nearby site and multiple org groupings incrementally if duplicates are found", {
  testdat <- Data_R5_TADAPackageDemo |> dplyr::filter(StateCode == "17")

  testthat::skip_if(
    is.null(testdat) || NROW(testdat) == 0,
    "Empty test data; skipping test."
  )

  testdat <- tryCatch(
    TADA_FindPotentialDuplicatesMultipleOrgs(testdat),
    error = function(e) {
      if (
        grepl(
          "HTTP 502|Bad Gateway|NHD",
          conditionMessage(e),
          ignore.case = TRUE
        )
      ) {
        testthat::skip("NHD service unavailable; skipping test.")
      }
      stop(e)
    }
  )

  testdat1 <- testdat |>
    dplyr::distinct(TADA.NearbySiteGroup) |>
    dplyr::filter(
      !is.na(TADA.NearbySiteGroup),
      grepl("^[0-9]+$", TADA.NearbySiteGroup)
    ) |>
    dplyr::pull(TADA.NearbySiteGroup)

  testdat2 <- testdat |>
    dplyr::select(TADA.MultipleOrgDupGroupID) |>
    dplyr::filter(TADA.MultipleOrgDupGroupID != "Not a duplicate") |>
    unique()

  expect_true(length(testdat1) == 0 || length(unique(diff(testdat1))) < 2)
  expect_true(length(testdat2) == 0 || length(unique(diff(testdat2))) < 2)
})

test_that("TADA_FindPotentialDuplicatesMultipleOrgs has non-NA values for each row in columns added in function", {
  testdat <- Data_R5_TADAPackageDemo |> dplyr::filter(StateCode == "17")

  testthat::skip_if(
    is.null(testdat) || NROW(testdat) == 0,
    "Empty test data; skipping test."
  )

  testdat <- TADA_FindPotentialDuplicatesMultipleOrgs(testdat)
  expect_false(any(is.na(testdat$TADA.MultipleOrgDupGroupID)))
  expect_false(any(is.na(testdat$TADA.MultipleOrgDuplicate)))
  expect_false(any(is.na(testdat$TADA.MonitoringLocationIdentifier)))
  expect_false(any(is.na(testdat$TADA.ResultSelectedMultipleOrgs)))
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
