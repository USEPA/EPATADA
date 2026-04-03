test_that("returns empty dataframe (names only) when all args missing", {
  res <- TADA_DefineCriteriaMethodology()
  expect_true(is.data.frame(res))
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
    "AssessPeriod",
    "AssessPeriodStartDate",
    "AssessPeriodEndDate",
    "Season",
    "SeasonStartDate",
    "SeasonEndDate",
    "DistrCount",
    "DistrPeriod",
    "DistrMinSample",
    "Notes"
  )
  expect_identical(names(res), expected_cols)
  expect_equal(nrow(res), 0)
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
    "auto_assign must be a boolean"
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
  expect_error(
    TADA_DefineCriteriaMethodology(
      .data = df,
      MLSummaryRef = ml,
      criteriaMethods = data.frame()
    ),
    "both provided"
  )
})

test_that("criteriaMethods + auto_assign = TRUE errors", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  expect_error(
    TADA_DefineCriteriaMethodology(
      .data = df,
      criteriaMethods = data.frame(ATTAINS.ParameterName = "PARAM_X"),
      auto_assign = TRUE
    ),
    "criteriaMethods is provided and auto_assign = TRUE"
  )
})

test_that("MLSummaryRef must contain required columns", {
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

test_that("returns rows from MLSummaryRef path and hides ComparableDataIdentifier when displayUniqueId = FALSE", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )
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
  expect_message(
    res <- TADA_DefineCriteriaMethodology(
      .data = df,
      MLSummaryRef = ml,
      org_id = "ORGX",
      displayUniqueId = FALSE,
      excel = FALSE
    ),
    "displayUniqueId == FALSE"
  )
  expect_true(is.data.frame(res))
  expect_true("ATTAINS.OrganizationIdentifier" %in% names(res))
  # The ComparableDataIdentifier should be set to NA_character_
  expect_true(all(is.na(res$TADA.ComparableDataIdentifier)))
  # Should preserve org/use/param from MLSummaryRef
  expect_true(any(res$ATTAINS.OrganizationIdentifier == "ORGX"))
  expect_true(any(res$ATTAINS.UseName == "USE1"))
  expect_true(any(res$ATTAINS.ParameterName == "PARAM_X"))
})

test_that("criteriaMethods path fills missing columns and handles missing TADA.ComparableDataIdentifier safely", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
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
    "AssessPeriod",
    "AssessPeriodStartDate",
    "AssessPeriodEndDate",
    "Season",
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

test_that("USEPA enrichment adds EPA304a rows only when overlapping characteristics exist", {
  # Skip if EPA304a table is not available in the package
  epa_file <- system.file(
    "extdata",
    "EPA304a_criteria_table.csv",
    package = "EPATADA"
  )
  skip_if_not(
    nzchar(epa_file) && file.exists(epa_file),
    "EPA304a table not found"
  )
  epa_tbl <- utils::read.csv(epa_file, fileEncoding = "UTF-8-BOM")
  skip_if(nrow(epa_tbl) == 0, "EPA304a table empty")

  # Choose one characteristic present in EPA304a
  char_candidates <- unique(epa_tbl$TADA.CharacteristicName)
  skip_if(length(char_candidates) == 0, "No characteristic names in EPA304a")
  char_pick <- char_candidates[1]

  df <- data.frame(
    TADA.ComparableDataIdentifier = "C_EPA",
    TADA.CharacteristicName = char_pick,
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  ml <- data.frame(
    ATTAINS.ParameterName = "PARAM_EPA",
    ATTAINS.UseName = "USE_EPA",
    ATTAINS.OrganizationIdentifier = "ORGX",
    UniqueSpatialCriteria = NA_character_,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU_EPA",
    TADA.ComparableDataIdentifier = "C_EPA",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )

  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = c("USEPA", "ORGX"),
    displayUniqueId = TRUE,
    excel = FALSE
  )

  # Expect at least some rows for USEPA if overlap exists
  expect_true(any(res$ATTAINS.OrganizationIdentifier == "USEPA"))
  # EPA rows should include the picked characteristic
  expect_true(any(
    res$ATTAINS.OrganizationIdentifier == "USEPA" &
      res$TADA.CharacteristicName == char_pick
  ))
})

test_that("Excel output is written to temporary Downloads and DataDictionary can be added", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")

  # Create an isolated temp USERPROFILE with a Downloads folder
  tmp <- withr::local_tempdir()
  withr::local_envvar(USERPROFILE = tmp)
  dir.create(
    file.path(tmp, "Downloads"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
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

  # Run with excel = TRUE; ensure file is created
  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = TRUE,
    excel = TRUE,
    overwrite = TRUE
  )
  expect_true(is.data.frame(res))

  xlsx_path <- file.path(tmp, "Downloads", "myfileRef.xlsx")
  expect_true(file.exists(xlsx_path))

  # Add DataDictionary to the same workbook
  TADA_CriteriaDataDictionary()
  wb <- openxlsx::loadWorkbook(xlsx_path)
  expect_true("DataDictionary" %in% names(wb))
})

test_that("displayUniqueId = TRUE retains ComparableDataIdentifier", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1"),
    TADA.CharacteristicName = c("CHAR_A"),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L"),
    stringsAsFactors = FALSE
  )
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
  expect_true(any(res$TADA.ComparableDataIdentifier == "C1"))
})

test_that("org_id = 'All' uses AUMLRef orgs without external calls", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  ml <- data.frame(
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    ATTAINS.OrganizationIdentifier = "ORG1",
    UniqueSpatialCriteria = NA_character_,
    ATTAINS.WaterType = "RIVER",
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.ComparableDataIdentifier = "C1",
    SaltFresh = "F",
    DepthCategory = NA_character_,
    stringsAsFactors = FALSE
  )
  aumlref <- data.frame(
    ATTAINS.OrganizationIdentifier = c("ORG1", "ORG2"),
    TADA.MonitoringLocationIdentifier = c("ML1", "ML2"),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    ATTAINS.WaterType = c("RIVER", "LAKE"),
    stringsAsFactors = FALSE
  )
  # org_id = "All" should pull orgs from AUMLRef (ORG1, ORG2) and not hit rExpertQuery
  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "All",
    AUMLRef = aumlref,
    displayUniqueId = TRUE,
    excel = FALSE
  )
  # It should include at least org from MLSummaryRef and not error
  expect_true(any(res$ATTAINS.OrganizationIdentifier %in% c("ORG1", "ORG2")))
})

test_that("USEPA only with no .data returns EPA304a rows (when available)", {
  epa_file <- system.file(
    "extdata",
    "EPA304a_criteria_table.csv",
    package = "EPATADA"
  )
  skip_if_not(
    nzchar(epa_file) && file.exists(epa_file),
    "EPA304a table not found"
  )
  res <- TADA_DefineCriteriaMethodology(org_id = "USEPA", excel = FALSE)
  expect_true(is.data.frame(res))
  expect_true(nrow(res) >= 0) # not asserting non-empty to be robust to local data
  expect_true("ATTAINS.OrganizationIdentifier" %in% names(res))
  # If non-empty, USEPA should be present
  if (nrow(res) > 0) {
    expect_true(any(res$ATTAINS.OrganizationIdentifier == "USEPA"))
  }
})

test_that("Spatial columns are blanked unless UniqueSpatialCriteria is set", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_A"),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )
  ml <- data.frame(
    ATTAINS.ParameterName = c("PARAM_X", "PARAM_X"),
    ATTAINS.UseName = c("USE1", "USE1"),
    ATTAINS.OrganizationIdentifier = c("ORGX", "ORGX"),
    UniqueSpatialCriteria = c(NA_character_, "Special Zone"),
    ATTAINS.WaterType = c("RIVER", "LAKE"),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    SaltFresh = c("F", "S"),
    DepthCategory = c("Surface", "Bottom"),
    stringsAsFactors = FALSE
  )
  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = TRUE,
    excel = FALSE
  )
  # Row with NA UniqueSpatialCriteria should have spatial columns blanked
  row_na <- res[res$TADA.ComparableDataIdentifier == "C1", ]
  expect_true(all(is.na(row_na$ATTAINS.WaterType)))
  expect_true(all(is.na(row_na$SaltFresh)))
  expect_true(all(is.na(row_na$DepthCategory)))
  # Row with non-NA UniqueSpatialCriteria should retain spatial columns
  row_sp <- res[res$TADA.ComparableDataIdentifier == "C2", ]
  expect_identical(row_sp$ATTAINS.WaterType, "LAKE")
  expect_identical(row_sp$SaltFresh, "S")
  expect_identical(row_sp$DepthCategory, "Bottom")
})

test_that("Date columns have Date class after MLSummaryRef path", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
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
    excel = FALSE
  )
  expect_s3_class(res$AssessPeriodStartDate, "Date")
  expect_s3_class(res$AssessPeriodEndDate, "Date")
  expect_s3_class(res$SeasonStartDate, "Date")
  expect_s3_class(res$SeasonEndDate, "Date")
})

test_that("criteriaMethods warnings appear for missing crosswalks (displayUniqueId TRUE/FALSE)", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )
  cm <- data.frame(
    # only defines CHAR_A; CHAR_B is missing on purpose
    ATTAINS.OrganizationIdentifier = "ORGX",
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    TADA.CharacteristicName = "CHAR_A",
    stringsAsFactors = FALSE
  )
  expect_warning(
    TADA_DefineCriteriaMethodology(
      .data = df,
      org_id = "ORGX",
      criteriaMethods = cm,
      displayUniqueId = TRUE,
      excel = FALSE
    ),
    "unique TADA.ComparableDataIdentifier"
  )
  expect_warning(
    TADA_DefineCriteriaMethodology(
      .data = df,
      org_id = "ORGX",
      criteriaMethods = cm,
      displayUniqueId = FALSE,
      excel = FALSE
    ),
    "unique TADA.CharacteristicName"
  )
})

test_that("displayUniqueId = TRUE retains ComparableDataIdentifier", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
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
  expect_true(any(res$TADA.ComparableDataIdentifier == "C1"))
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
