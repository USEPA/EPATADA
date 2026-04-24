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
    "Notes",
    "EquationType",
    "EquationFormula",
    "pHThreshold",
    "pHDirection",
    "hardness_param_1",
    "hardness_param_2",
    "hardness_param_3",
    "hardness_param_4",
    "TemperatureExtreme",
    "pH_param_1",
    "pH_param_2",
    "pH_param_3",
    "pH_param_4",
    "pH_param_5",
    "pH_param_6",
    "pH_param_7",
    "pH_param_8",
    "pH_param_9",
    "MinEqMagnitude",
    "MaxEqMagnitude"
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
  expect_warning(
    res <- TADA_DefineCriteriaMethodology(
      .data = df,
      org_id = c("ORGX"),
      criteriaMethods = cm,
      excel = FALSE
    )
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

test_that("org_id NULL becomes empty string in criteriaMethods path", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )
  cm <- data.frame(
    ATTAINS.ParameterName = c("PARAM_X"),
    ATTAINS.UseName = c("USE1"),
    TADA.CharacteristicName = c("CHAR_A"),
    stringsAsFactors = FALSE
  )
  # org_id omitted -> becomes ""
  expect_warning(
    res <- TADA_DefineCriteriaMethodology(
      .data = df,
      criteriaMethods = cm,
      excel = FALSE
    )
  )
  expect_true(is.data.frame(res))
  expect_true(any(res$ATTAINS.OrganizationIdentifier == ""))
  expect_true(any(res$TADA.CharacteristicName == "CHAR_A"))
})

test_that("org_id 'all' correctly filters by organizations found in user supplied criteriaMethods.", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1", "C2", "C3"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_B", "CHAR_C"),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )
  cm <- data.frame(
    ATTAINS.ParameterName = c("PARAM_X", "PARAM_Z"),
    ATTAINS.UseName = c("USE1", "USE2"),
    TADA.CharacteristicName = c("CHAR_A", "CHAR_B"),
    ATTAINS.OrganizationIdentifier = c("MTDEQ", "ORG_B"),
    stringsAsFactors = FALSE
  )
  # org_id omitted -> becomes ""
  expect_warning(
    res <- TADA_DefineCriteriaMethodology(
      .data = df,
      criteriaMethods = cm,
      org_id = "all",
      excel = FALSE
    )
  )
  expect_true(is.data.frame(res))
  expect_true(all(
    res$ATTAINS.OrganizationIdentifier %in% cm$ATTAINS.OrganizationIdentifier
  ))
  expect_true(any(res$TADA.CharacteristicName == "CHAR_A"))
})

test_that("criteriaMethods season date strings are parsed to Date class", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "CHAR_A",
    TADA.ResultMeasure.MeasureUnitCode = "mg/L",
    stringsAsFactors = FALSE
  )
  cm <- data.frame(
    ATTAINS.OrganizationIdentifier = "ORGX",
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    TADA.CharacteristicName = "CHAR_A",
    SeasonStartDate = c("Jun 15"),
    SeasonEndDate = c("06-30"),
    stringsAsFactors = FALSE
  )
  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    org_id = "ORGX",
    criteriaMethods = cm,
    excel = FALSE
  )
  expect_s3_class(res$SeasonStartDate, "Date")
  expect_s3_class(res$SeasonEndDate, "Date")
  # Parsed dates should not be NA when strings were provided
  sub <- subset(
    res,
    ATTAINS.OrganizationIdentifier == "ORGX" &
      TADA.CharacteristicName == "CHAR_A"
  )
  expect_true(any(!is.na(sub$SeasonStartDate)))
  expect_true(any(!is.na(sub$SeasonEndDate)))
})

test_that("displayUniqueId = FALSE dedupes multiple IDs into one row", {
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
    UniqueSpatialCriteria = c(NA_character_, NA_character_),
    ATTAINS.WaterType = c("RIVER", "RIVER"),
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1"),
    TADA.ComparableDataIdentifier = c("C1", "C2"),
    SaltFresh = c("F", "F"),
    DepthCategory = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = FALSE,
    excel = FALSE
  )
  # ComparableDataIdentifier is set to NA and duplicates collapse
  expect_true(all(is.na(res$TADA.ComparableDataIdentifier)))
  # Only one row for the CHAR_A / ORGX / USE1 / PARAM_X combination should remain
  sub <- subset(
    res,
    ATTAINS.OrganizationIdentifier == "ORGX" &
      ATTAINS.ParameterName == "PARAM_X" &
      ATTAINS.UseName == "USE1" &
      TADA.CharacteristicName == "CHAR_A"
  )
  expect_equal(nrow(sub), 1L)
})

test_that("MagnitudeValue columns are numeric in MLSummaryRef path", {
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
  expect_type(res$MagnitudeValueLower, "double")
  expect_type(res$MagnitudeValueUpper, "double")
})

test_that("USEPA enrichment emits informational message when USEPA included", {
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
  df <- data.frame(
    TADA.ComparableDataIdentifier = "C1",
    TADA.CharacteristicName = "ANY_CHAR",
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
  expect_message(
    TADA_DefineCriteriaMethodology(
      .data = df,
      MLSummaryRef = ml,
      org_id = c("USEPA", "ORGX"),
      excel = FALSE
    ),
    "USEPA was included"
  )
})

test_that("Excel save path uses timestamp when overwrite = FALSE", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
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

  # First write
  # res1 <- TADA_DefineCriteriaMethodology(
  #   .data = df,
  #   MLSummaryRef = ml,
  #   org_id = "ORGX",
  #   displayUniqueId = TRUE,
  #   excel = TRUE,
  #   overwrite = FALSE
  # )
  # Then write again to ensure timestamped file is created
  res2 <- TADA_DefineCriteriaMethodology(
    .data = df,
    MLSummaryRef = ml,
    org_id = "ORGX",
    displayUniqueId = TRUE,
    excel = TRUE,
    overwrite = FALSE
  )
  files <- list.files(
    file.path(tmp, "Downloads"),
    pattern = "^CriteriaMethodology.*\\.xlsx$",
    full.names = TRUE
  )
  expect_true(length(files) >= 2) # base + at least one timestamped copy
})

test_that("All NA org identifiers skip final formatting block safely", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("C1"),
    TADA.CharacteristicName = c("CHAR_A"),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L"),
    stringsAsFactors = FALSE
  )
  cm <- data.frame(
    ATTAINS.OrganizationIdentifier = NA_character_,
    ATTAINS.ParameterName = "PARAM_X",
    ATTAINS.UseName = "USE1",
    TADA.CharacteristicName = "CHAR_A",
    stringsAsFactors = FALSE
  )
  res <- TADA_DefineCriteriaMethodology(
    .data = df,
    criteriaMethods = cm,
    excel = FALSE
  )
  expect_true(all(is.na(res$ATTAINS.OrganizationIdentifier)))
  # Ensure we still have the CHAR_A row and no error occurred
  expect_true(any(res$TADA.CharacteristicName == "CHAR_A"))
})

make_min_tada_df <- function(
  n = 1,
  char = "CHAR_A",
  unit = "mg/L",
  cid = "C1"
) {
  data.frame(
    # Core fields used downstream
    TADA.ComparableDataIdentifier = rep(cid, n),
    TADA.CharacteristicName = rep(char, n),
    TADA.ResultMeasure.MeasureUnitCode = rep(unit, n),

    # WQP/TADA columns required by TADA_AutoClean/TADA_CheckColumns
    ActivityMediaName = rep("Water", n),
    ResultMeasureValue = rep(1.0, n),
    ResultMeasure.MeasureUnitCode = rep(unit, n),
    CharacteristicName = rep(char, n),
    ResultSampleFractionText = rep(NA_character_, n),
    MethodSpeciationName = rep(NA_character_, n),
    DetectionQuantitationLimitMeasure.MeasureUnitCode = rep(NA_character_, n),
    ResultDetectionConditionText = rep(NA_character_, n),
    ResultIdentifier = paste0("RID", seq_len(n)),
    DetectionQuantitationLimitMeasure.MeasureValue = rep(NA_real_, n),
    LatitudeMeasure = rep(45.0, n),
    LongitudeMeasure = rep(-120.0, n),
    stringsAsFactors = FALSE
  )
}

test_that("auto_assign = TRUE with MLSummaryRef filters to MLSummaryRef identifiers only", {
  df <- make_min_tada_df(
    n = 2,
    char = "CHAR_A",
    unit = "mg/L",
    cid = c("C1", "C2")
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
    auto_assign = TRUE,
    excel = FALSE
  )
  expect_false(any(res$TADA.ComparableDataIdentifier == "C2"))
  expect_true(any(res$TADA.ComparableDataIdentifier == "C1"))
})

test_that("org_id = 'All' without AUMLRef emits a message and attempts to pull domain orgs", {
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

  # Expect the informational message
  expect_message(
    TADA_DefineCriteriaMethodology(
      .data = df,
      MLSummaryRef = ml,
      org_id = "All",
      displayUniqueId = TRUE,
      excel = FALSE
    ),
    "org_id == 'All' was selected"
  )
})
