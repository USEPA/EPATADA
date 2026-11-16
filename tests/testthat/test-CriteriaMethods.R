testthat::test_that("returns empty dataframe (names only) when all args missing", {
  res <- TADA_DefineCriteriaMethodology()
  expect_type(res, "list")
  expect_named(
    res,
    c("DefineCriteriaMethodology", "DataDictionary", "AllowableValues")
  )

  dcm <- res$DefineCriteriaMethodology
  expect_true(is.data.frame(dcm))

  # Expected columns (keep in sync with desired_cols in the function)
  expected_cols <- c(
    "ATTAINS.OrganizationIdentifier",
    "ATTAINS.ParameterName",
    "ATTAINS.UseName",
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "ATTAINS.WaterType",
    "SaltFresh",
    "DepthCategory",
    "UniqueSpatialCriteria",
    "AcuteChronic",
    "EquationBased",
    "MagnitudeValueLower",
    "MagnitudeValueUpper",
    "MagnitudeUnit",
    "DurationValue",
    "DurationUnit",
    "DurationMethod",
    "FreqValue",
    "FreqMethod",
    "AssessPeriodStartDate",
    "AssessPeriodEndDate",
    "SeasonStartDate",
    "SeasonEndDate",
    "DistrCount",
    "DistrPeriod",
    "DistrMinSample",
    "Notes",
    "Links",
    "EquationType",
    "EquationFormula",
    "pHThreshold",
    "pHDirection",
    "hardness_param_1",
    "hardness_param_2",
    "hardness_param_3",
    "hardness_param_4",
    "AmmoniaEqType",
    "pH_param_1",
    "pH_param_2",
    "pH_param_3",
    "pH_param_4",
    "pH_param_5",
    "pH_param_6",
    "pH_param_7",
    "pH_param_8",
    "pH_param_9",
    "pH_param_10",
    "pH_param_11",
    "pH_param_12",
    "MinEqMagnitude",
    "MaxEqMagnitude"
  )
  expect_identical(names(dcm), expected_cols)
  expect_equal(nrow(dcm), 0)
})

test_that("auto_assign must be logical", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  expect_error(
    TADA_DefineCriteriaMethodology(.data = df, auto_assign = "yes"),
    "auto_assign must be TRUE/FALSE"
  )
})

test_that("cannot supply both MLSummaryRef and criteriaMethods", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  ml <- data.frame(
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.MonitoringLocationTypeName = "River",
    TADA.LongitudeMeasure = -122.5,
    TADA.LatitudeMeasure = 47.6,
    TADA.ComparableDataIdentifier = "C1",
    ATTAINS.OrganizationIdentifier = "ORGX",
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    IncludeOrExclude = "Include",
    UniqueSpatialCriteria = NA_character_,
    Flag.UseInput = TRUE,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_error(
    TADA_DefineCriteriaMethodology(
      .data = df,
      MLSummaryRef = ml,
      criteriaMethods = data.frame()
    ),
    "TADA_DefineCriteriaMethodology: only one of MLSummaryRef or criteriaMethods is allowed as an input."
  )
})

testthat::test_that("criteriaMethods + auto_assign = TRUE errors", {
  df <- Data_MT_MissoulaCounty
  expect_error(
    TADA_DefineCriteriaMethodology(
      .data = df,
      org_id = "MTDEQ",
      criteriaMethods = data.frame(ATTAINS.ParameterName = "PARAM_X"),
      auto_assign = TRUE
    ),
    "TADA_DefineCriteriaMethodology: criteriaMethods and auto_assign = TRUE are not valid together."
  )
})

testthat::test_that("MLSummaryRef must contain required columns", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  # Missing SaltFresh on purpose
  ml_bad <- data.frame(
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    ATTAINS.OrganizationIdentifier = "ORGX",
    UniqueSpatialCriteria = NA_character_,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.ComparableDataIdentifier = "C1",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_error(
    TADA_DefineCriteriaMethodology(.data = df, MLSummaryRef = ml_bad),
    "missing required columns"
  )
})

testthat::test_that("returns rows from MLSummaryRef path and hides ComparableDataIdentifier when displayUniqueId = FALSE", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    stringsAsFactors = FALSE
  )

  ml <- data.frame(
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.MonitoringLocationTypeName = "River",
    TADA.LongitudeMeasure = -122.5,
    TADA.LatitudeMeasure = 47.6,
    TADA.ComparableDataIdentifier = "C1",
    ATTAINS.OrganizationIdentifier = "ORGX",
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    IncludeOrExclude = "Include",
    UniqueSpatialCriteria = NA_character_,
    Flag.UseInput = TRUE,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )

  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = FALSE,
    excel = FALSE
  )

  res <- res$DefineCriteriaMethodology
  expect_true(is.data.frame(res))
  expect_true("ATTAINS.OrganizationIdentifier" %in% names(res))
  expect_true(all(is.na(res$TADA.ComparableDataIdentifier)))
  expect_true(any(res$ATTAINS.OrganizationIdentifier == "ORGX"))
  expect_true(any(res$ATTAINS.UseName == "USE1"))
  expect_true(any(res$ATTAINS.ParameterName == "PARAM_X"))
})

test_that("criteriaMethods path fills missing columns and handles missing TADA.ComparableDataIdentifier safely", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
    TADA.ResultSampleFractionText = c(NA_character_, NA_character_),
    TADA.MethodSpeciationName = c(NA_character_, NA_character_),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )
  # No TADA.ComparableDataIdentifier column here (exercise any_of)
  cm <- data.frame(
    ATTAINS.OrganizationIdentifier = c("ORGX"),
    ATTAINS.ParameterName = c("PARAM_X"),
    ATTAINS.UseName = c("USE1"),
    TADA.CharacteristicName = c("CHAR_A"),
    stringsAsFactors = FALSE
  )

  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    org_id = c("ORGX"),
    criteriaMethods = cm,
    excel = FALSE
  )

  res <- res$DefineCriteriaMethodology
  # Expect desired columns present
  expected_cols <- c(
    "ATTAINS.OrganizationIdentifier",
    "ATTAINS.ParameterName",
    "ATTAINS.UseName",
    "TADA.ComparableDataIdentifier",
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "ATTAINS.WaterType",
    "SaltFresh",
    "DepthCategory",
    "UniqueSpatialCriteria",
    "AcuteChronic",
    "EquationBased",
    "MagnitudeValueLower",
    "MagnitudeValueUpper",
    "MagnitudeUnit",
    "DurationValue",
    "DurationUnit",
    "DurationMethod",
    "FreqValue",
    "FreqMethod",
    "AssessPeriodStartDate",
    "AssessPeriodEndDate",
    "SeasonStartDate",
    "SeasonEndDate",
    "DistrCount",
    "DistrPeriod",
    "DistrMinSample",
    "Notes"
  )
  expect_true(all(expected_cols %in% names(res)))
  # Should include at least the CHAR_A row
  expect_true(any(res$TADA.CharacteristicName == "CHAR_A"))
  # Should include ORGX
  expect_true(any(res$ATTAINS.OrganizationIdentifier == "ORGX"))
})


####### Commenting out for now and handling EPA304a in future PR - KW 11/13/26

# test_that("USEPA enrichment adds EPA304a rows only when overlapping characteristics exist", {
#   # Skip if EPA304a table is not available in the package
#   epa_file <- system.file(
#     "extdata",
#     "EPA304a_criteria_table.csv",
#     package = "EPATADA"
#   )
#   skip_if_not(
#     nzchar(epa_file) && file.exists(epa_file),
#     "EPA304a table not found"
#   )
#   epa_tbl <- utils::read.csv(epa_file, fileEncoding = "UTF-8-BOM")
#   skip_if(nrow(epa_tbl) == 0, "EPA304a table empty")
#
#   # Choose one characteristic present in EPA304a
#   char_candidates <- unique(epa_tbl$TADA.CharacteristicName)
#   skip_if(length(char_candidates) == 0, "No characteristic names in EPA304a")
#   char_pick <- char_candidates[1]
#
#   df <- data.frame(
#     TADA.ComparableDataIdentifier = "C_EPA",
#     TADA.CharacteristicName = char_pick,
#     TADA.ResultMeasure.MeasureUnitCode = "mg/L",
#     stringsAsFactors = FALSE
#   )
#   ml <- data.frame(
#     ATTAINS.ParameterName = "PARAM_EPA",
#     ATTAINS.UseName = "USE_EPA",
#     ATTAINS.OrganizationIdentifier = "ORGX",
#     UniqueSpatialCriteria = NA_character_,
#     ATTAINS.WaterType = "RIVER",
#     ATTAINS.AssessmentUnitIdentifier = "AU_EPA",
#     TADA.ComparableDataIdentifier = "C_EPA",
#     SaltFresh = "F",
#     DepthCategory = NA_character_,
#     stringsAsFactors = FALSE
#   )
#
#   res <- TADA_DefineCriteriaMethodology(
#     .data = df,
#     MLSummaryRef = ml,
#     org_id = c("USEPA", "ORGX"),
#     displayUniqueId = TRUE,
#     excel = FALSE
#   )
#   res <- res$DefineCriteriaMethodology
#
#   # Expect at least some rows for USEPA if overlap exists
#   expect_true(any(res$ATTAINS.OrganizationIdentifier == "USEPA"))
#   # EPA rows should include the picked characteristic
#   expect_true(any(
#     res$ATTAINS.OrganizationIdentifier == "USEPA" &
#       res$TADA.CharacteristicName == char_pick
#   ))
# })

test_that("Excel output is written to temporary Downloads and DataDictionary can be added", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")

  tmp_xlsx <- file.path(tempdir(), "CriteriaMethodology.xlsx")
  # Can we add DataDictionary to the same workbook
  .TADA_CriteriaDataDictionary(tmp_xlsx)

  expect_true(file.exists(tmp_xlsx))

  wb <- openxlsx::loadWorkbook(tmp_xlsx)
  expect_true("DataDictionary" %in% names(wb))
})

test_that("displayUniqueId = TRUE retains ComparableDataIdentifier", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1"),
    TADA.CharacteristicName = c("CHAR_A"),
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L"),
    stringsAsFactors = FALSE
  )

  ml <- data.frame(
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.MonitoringLocationTypeName = "River",
    TADA.LongitudeMeasure = -122.5,
    TADA.LatitudeMeasure = 47.6,
    TADA.ComparableDataIdentifier = "C1",
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    ATTAINS.OrganizationIdentifier = "ORGX",
    IncludeOrExclude = "Include",
    Flag.UseInput = TRUE,
    UniqueSpatialCriteria = NA_character_,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )

  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = TRUE,
    excel = FALSE
  )

  res <- res$DefineCriteriaMethodology

  expect_true("TADA.ComparableDataIdentifier" %in% names(res))
  expect_true(any(res$TADA.ComparableDataIdentifier == "C1", na.rm = TRUE))
})

#### Commenting out EPA304a case for a future PR - KW 11/13/26

# test_that("USEPA only with no .data returns EPA304a rows (when available)", {
#   epa_file <- system.file(
#     "extdata",
#     "EPA304a_criteria_table.csv",
#     package = "EPATADA"
#   )
#   skip_if_not(
#     nzchar(epa_file) && file.exists(epa_file),
#     "EPA304a table not found"
#   )
#   res <- TADA_DefineCriteriaMethodology(org_id = "USEPA", excel = FALSE)
#   res <- res$DefineCriteriaMethodology
#   expect_true(is.data.frame(res))
#   expect_true(nrow(res) >= 0) # not asserting non-empty to be robust to local data
#   expect_true("ATTAINS.OrganizationIdentifier" %in% names(res))
#   # If non-empty, USEPA should be present
#   if (nrow(res) > 0) {
#     expect_true(any(res$ATTAINS.OrganizationIdentifier == "USEPA"))
#   }
# })

test_that("Date columns have Date class after MLSummaryRef path", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )

  ml <- data.frame(
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.MonitoringLocationTypeName = "River",
    TADA.LongitudeMeasure = -122.5,
    TADA.LatitudeMeasure = 47.6,
    TADA.ComparableDataIdentifier = "C1",
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    ATTAINS.OrganizationIdentifier = "ORGX",
    IncludeOrExclude = "Include",
    Flag.UseInput = TRUE,
    UniqueSpatialCriteria = NA_character_,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )

  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    excel = FALSE
  )

  res <- res$DefineCriteriaMethodology

  expect_s3_class(res$AssessPeriodStartDate, "Date")
  expect_s3_class(res$AssessPeriodEndDate, "Date")
})

test_that("displayUniqueId = TRUE retains ComparableDataIdentifier", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultSampleFractionText = NA_character_,
    TADA.MethodSpeciationName = NA_character_,
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )

  ml <- data.frame(
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.MonitoringLocationTypeName = "River",
    TADA.LongitudeMeasure = -122.5,
    TADA.LatitudeMeasure = 47.6,
    TADA.ComparableDataIdentifier = "C1",
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    ATTAINS.OrganizationIdentifier = "ORGX",
    IncludeOrExclude = "Include",
    Flag.UseInput = TRUE,
    UniqueSpatialCriteria = NA_character_,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )

  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = TRUE,
    excel = FALSE
  )

  res <- res$DefineCriteriaMethodology

  expect_true("TADA.ComparableDataIdentifier" %in% names(res))
  expect_true(any(res$TADA.ComparableDataIdentifier == "C1", na.rm = TRUE))
})

test_that("final formatting preserves a single NA UseName summary row", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  # Create MLSummaryRef that will lead to both a specific use row and a NA-UseName row
  ml <- data.frame(
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    ATTAINS.OrganizationIdentifier = "ORGX",
    UniqueSpatialCriteria = NA_character_,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.ComparableDataIdentifier = "C1",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )
  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = TRUE,
    excel = FALSE
  )
  res <- res$DefineCriteriaMethodology
  # Create an artificial duplicate NA Use row by binding a NA UseName copy; then run final formatting logic indirectly by calling function again via criteriaMethods
  cm <- res
  cm$ATTAINS.UseName <- NA_character_
  res2 <- TADA_DefineCriteriaMethodology(
    .data = df,
    org_id = "ORGX",
    criteriaMethods = cm,
    displayUniqueId = TRUE,
    excel = FALSE
  )
  res2 <- res2$DefineCriteriaMethodology
  # Expect exactly one NA UseName row for this characteristic/param/org
  sub <- subset(
    res2,
    is.na(ATTAINS.UseName) &
      ATTAINS.OrganizationIdentifier == "ORGX" &
      ATTAINS.ParameterName == "PARAM_X" &
      TADA.CharacteristicName == "CHAR_A"
  )
  expect_true(nrow(sub) <= 1)
})

test_that("MLSummaryRef numeric (non-data.frame, non-character) errors clearly", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  # numeric will trigger 'must be a data frame' in the type guard
  expect_error(
    TADA_DefineCriteriaMethodology(.data = df, MLSummaryRef = 123),
    "MLSummaryRef must be a data frame"
  )
})

###### commenting out to handle in future PR for org_id = NULL - KW 11/13/26

# test_that("org_id NULL becomes empty string in criteriaMethods path", {
#   df <- data.frame(
#     TADA.ComparableDataIdentifier = c("C1", "C2"),
#     TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
#     TADA.ResultSampleFractionText = NA_character_,
#     TADA.MethodSpeciationName = NA_character_,
#     TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
#     stringsAsFactors = FALSE
#   )
#
#   cm <- data.frame(
#     ATTAINS.OrganizationIdentifier = NA_character_,
#     ATTAINS.ParameterName = "PARAM_X",
#     ATTAINS.UseName = "USE1",
#     TADA.CharacteristicName = "CHAR_A",
#     TADA.ResultSampleFractionText = NA_character_,
#     TADA.MethodSpeciationName = NA_character_,
#     TADA.ComparableDataIdentifier = NA_character_,
#     ATTAINS.WaterType = NA_character_,
#     SaltFresh = NA_character_,
#     DepthCategory = NA_character_,
#     UniqueSpatialCriteria = NA_character_,
#     AcuteChronic = NA_character_,
#     EquationBased = NA_character_,
#     MagnitudeValueLower = NA_real_,
#     MagnitudeValueUpper = NA_real_,
#     MagnitudeUnit = NA_character_,
#     DurationValue = NA_real_,
#     DurationUnit = NA_character_,
#     DurationMethod = NA_character_,
#     FreqValue = NA_real_,
#     FreqMethod = NA_character_,
#     AssessPeriodStartDate = as.Date(NA),
#     AssessPeriodEndDate = as.Date(NA),
#     SeasonStartDate = as.Date(NA),
#     SeasonEndDate = as.Date(NA),
#     DistrCount = NA_real_,
#     DistrPeriod = NA_character_,
#     DistrMinSample = NA_real_,
#     Notes = NA_character_,
#     Links = NA_character_,
#     stringsAsFactors = FALSE
#   )
#
#   res <- TADA_DefineCriteriaMethodology(
#     .data = df,
#     criteriaMethods = cm,
#     excel = FALSE
#   )
#
#   res <- res$DefineCriteriaMethodology
#
#   expect_true(is.data.frame(res))
#   expect_true(any(res$ATTAINS.OrganizationIdentifier == "", na.rm = TRUE))
#   expect_true(any(res$TADA.CharacteristicName == "CHAR_A", na.rm = TRUE))
# })

###### commenting out to handle case when org_id = all for now for separate PR - 11/13/26

# test_that("org_id 'all' correctly includes organizations found in user supplied criteriaMethods.", {
#   df <- data.frame(
#     TADA.ComparableDataIdentifier = c("C1", "C2", "C3"),
#     TADA.CharacteristicName = c("CHAR_A", "CHAR_B", "CHAR_C"),
#     TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "mg/L"),
#     stringsAsFactors = FALSE
#   )
#   cm <- data.frame(
#     ATTAINS.ParameterName = c("PARAM_X", "PARAM_Z"),
#     ATTAINS.UseName = c("USE1", "USE2"),
#     TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
#     ATTAINS.OrganizationIdentifier = c("MTDEQ", "ORG_B"),
#     stringsAsFactors = FALSE
#   )
#   # org_id omitted -> becomes ""
#   res <- TADA_DefineCriteriaMethodology(
#     .data = df,
#     criteriaMethods = cm,
#     org_id = "all",
#     excel = FALSE
#   )
#   res <- res$DefineCriteriaMethodology
#   expect_true(is.data.frame(res))
#   expect_true(all(
#     res$ATTAINS.OrganizationIdentifier %in% cm$ATTAINS.OrganizationIdentifier
#   ))
#   expect_true(any(res$TADA.CharacteristicName == "CHAR_A"))
# })

test_that("MLSummaryRef numeric errors clearly", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  expect_error(
    TADA_DefineCriteriaMethodology(.data = df, MLSummaryRef = 123),
    "MLSummaryRef must be a data frame"
  )
})
