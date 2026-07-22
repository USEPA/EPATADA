# get random data
test_dat <- TADA_RandomTestingData()

# Test: Check for potential duplicates during criteria methods table generation
testthat::test_that("TADA_ParametersForAnalysis ", {
  param_ref_none <- TADA_ParametersForAnalysis(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "None",
    excel = FALSE
  )

  suppressWarnings(
    use_param_ref_none <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_none,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = FALSE,
      excel = FALSE
    )
  )
  # a user supplied table for a param_use will not populate TADA_UsesForAnalysis if paramRef crosswalk is left blank.
  user.supplied.uses.param <- data.frame(
    ATTAINS.OrganizationIdentifier = rep(
      "MTDEQ",
      length(unique(test_dat$TADA.ComparableDataIdentifier))
    ), # we have assigned to example uses to each parameter
    ATTAINS.ParameterName = unique(test_dat$TADA.ComparableDataIdentifier),
    ATTAINS.UseName = rep(
      c("example use_name1", "example use_name2"),
      length(unique(test_dat$TADA.ComparableDataIdentifier))
    )
  )

  suppressWarnings(
    use_param_ref_none2 <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_none,
      usesRef = user.supplied.uses.param,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = FALSE,
      excel = FALSE
    )
  )

  # A user supplied table when paramRef has no crosswalk should return 0 rows (empty data frame).
  # Check to make sure there are 7 columns, 0 rows
  testthat::expect_true(
    dim(use_param_ref_none)[1] == 0 &&
      dim(use_param_ref_none)[2] == 7 &&
      dim(use_param_ref_none2)[1] == 0 &&
      dim(use_param_ref_none2)[2] == 7
  )
  #################
  param_ref_all <- TADA_ParametersForAnalysis(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "All",
    excel = FALSE
  )

  suppressWarnings(
    use_param_ref_all <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_all,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = TRUE,
      excel = FALSE
    )
  )

  # a user supplied table for a param_use will not populate TADA_UsesForAnalysis if paramRef crosswalk is left blank.
  user.supplied.uses.param <- data.frame(
    ATTAINS.OrganizationIdentifier = rep(
      "MTDEQ",
      length(unique(param_ref_all$ATTAINS.ParameterName))
    ), # we have assigned to example uses to each parameter
    ATTAINS.ParameterName = unique(param_ref_all$ATTAINS.ParameterName),
    ATTAINS.UseName = rep(
      c("example use_name1", "example use_name2"),
      length(unique(param_ref_all$ATTAINS.ParameterName))
    )
  )

  suppressWarnings(
    use_param_ref_all2 <- TADA_UsesForAnalysis(
      test_dat,
      paramRef = param_ref_all,
      usesRef = user.supplied.uses.param,
      org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = FALSE,
      excel = FALSE
    )
  )

  # A user supplied table when paramRef is filled out when auto_assign = FALSE
  # should all reflect the user supplied crosswalk. Check all unique uses to make sure.
  # Check to make sure there are 7 columns, 0 rows
  testthat::expect_true(
    all(
      unique(use_param_ref_all2$ATTAINS.UseName) %in%
        unique(user.supplied.uses.param$ATTAINS.UseName)
    ) &&
      dim(use_param_ref_all)[2] == 7 &&
      dim(use_param_ref_all2)[2] == 7
  )
  #################
  param_ref_org <- TADA_ParametersForAnalysis(
    test_dat,
    org_id = "MTDEQ", # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
    auto_assign = "Org",
    excel = FALSE
  )

  # check to make sure all rows contain all unique ComparableDataIdentifiers in the TADA data frame
  testthat::expect_true(
    length(unique(test_dat$TADA.ComparableDataIdentifier)) ==
      length(unique(param_ref_none$TADA.ComparableDataIdentifier)) &&
      length(unique(param_ref_all$TADA.ComparableDataIdentifier)) ==
        length(unique(param_ref_none$TADA.ComparableDataIdentifier))
  )
})

testthat::test_that("TADA_DefineCriteriaMethodology auto_assign table contains all unique TADA.ComparableDataIdentifier", {
  suppressWarnings(
    Criteria_autofill <- TADA_DefineCriteriaMethodology(
      test_dat,
      org_id = NULL,
      auto_assign = TRUE,
      displayUniqueId = FALSE,
      excel = FALSE
    )
  )

  suppressWarnings(
    Criteria_autofill_w_uniqueID <- TADA_DefineCriteriaMethodology(
      test_dat,
      org_id = NULL,
      auto_assign = TRUE,
      displayUniqueId = TRUE,
      excel = FALSE
    )
  )

  testthat::expect_setequal(
    unique(test_dat$TADA.ComparableDataIdentifier),
    unique(
      Criteria_autofill_w_uniqueID$DefineCriteriaMethodology$TADA.ComparableDataIdentifier
    )
  )

  testthat::expect_setequal(
    unique(test_dat$TADA.CharacteristicName),
    unique(Criteria_autofill$DefineCriteriaMethodology$TADA.CharacteristicName)
  )
})

testthat::test_that("Excel file generation works correctly with overwrite = F in TADA_UsesForAnalysis when the ParamUseMLCrosswalks.xlsx does not exist yet.", {
  # specify downloads path
  downloads_path <- .get_downloads_path("ParamUseMLCrosswalks.xlsx")

  # 1. Remove the file if it already exists
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # 2. Run the function being tested
  paramTest <- TADA_ParametersForAnalysis(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ"
  )
  msg <- utils::capture.output(
    usesTest <- TADA_UsesForAnalysis(
      Data_MT_MissoulaCounty,
      org_id = "MTDEQ",
      paramRef = paramTest,
      excel = T,
      overwrite = F
    ),
    type = "message"
  )
  # 3. Assertions
  expect_true(file.exists(downloads_path))

  # find the timestamped copy index name
  idx_pat <- which(grepl("ParamUseMLCrosswalks_", msg, fixed = TRUE))

  # find timestamped path
  timestamp_path <- gsub("Saved as: ", "", msg[idx_pat])

  # remove time stamped path
  if (file.exists(timestamp_path)) {
    file.remove(timestamp_path)
  }

  # Optional: Clean up after test completes
  on.exit(if (file.exists(downloads_path)) file.remove(downloads_path))
})

testthat::test_that("Excel file generation works correctly with overwrite = T in TADA_UsesForAnalysis when the ParamUseMLCrosswalks.xlsx does not exist yet.", {
  # specify downloads path
  downloads_path <- .get_downloads_path("ParamUseMLCrosswalks.xlsx")

  # 1. Remove the file if it already exists
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # 2. Run the function being tested
  paramTest <- TADA_ParametersForAnalysis(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ"
  )
  usesTest <- TADA_UsesForAnalysis(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ",
    paramRef = paramTest,
    excel = T,
    overwrite = T
  )
  # 3. Assertions
  expect_true(file.exists(downloads_path))

  # Optional: Clean up after test completes
  on.exit(if (file.exists(downloads_path)) file.remove(downloads_path))
})

testthat::test_that("Excel file generation works correctly with overwrite = F in TADA_MLSummaryRef when the ParamUseMLCrosswalks.xlsx does not exist yet.", {
  # specify downloads path
  downloads_path <- .get_downloads_path("ParamUseMLCrosswalks.xlsx")

  # 1. Remove the file if it already exists
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # 2. Run the function being tested
  paramTest <- TADA_ParametersForAnalysis(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ"
  )
  usesTest <- TADA_UsesForAnalysis(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ",
    paramRef = paramTest
  )
  msg <- utils::capture.output(
    final <- TADA_MLSummary(
      Data_MT_MissoulaCounty,
      org_id = "MTDEQ",
      usesRef = usesTest,
      excel = T,
      overwrite = F
    ),
    type = "message"
  )
  # 3. Assertions
  expect_true(file.exists(downloads_path))

  # find the timestamped copy index name
  idx_pat <- which(grepl("ParamUseMLCrosswalks_", msg, fixed = TRUE))

  # find timestamped path
  timestamp_path <- gsub("Saved as: ", "", msg[idx_pat])

  # remove time stamped path
  if (file.exists(timestamp_path)) {
    file.remove(timestamp_path)
  }

  # Optional: Clean up after test completes
  on.exit(if (file.exists(downloads_path)) file.remove(downloads_path))
})

testthat::test_that("Excel file generation works correctly with overwrite = T in TADA_MLSummaryRef when the ParamUseMLCrosswalks.xlsx does not exist yet.", {
  # specify downloads path
  downloads_path <- .get_downloads_path("ParamUseMLCrosswalks.xlsx")

  # 1. Remove the file if it already exists
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # 2. Run the function being tested
  paramTest <- TADA_ParametersForAnalysis(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ"
  )
  usesTest <- TADA_UsesForAnalysis(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ",
    paramRef = paramTest
  )
  final <- TADA_MLSummary(
    Data_MT_MissoulaCounty,
    org_id = "MTDEQ",
    usesRef = usesTest,
    excel = T,
    overwrite = T
  )
  # 3. Assertions
  expect_true(file.exists(downloads_path))

  # Optional: Clean up after test completes
  on.exit(if (file.exists(downloads_path)) file.remove(downloads_path))
})

testthat::test_that("Excel file generation works with blank inputs in TADA_ParametersForAnalysis even when excel file does not exist yet.", {
  # specify downloads path
  downloads_path <- .get_downloads_path("ParamUseMLCrosswalks.xlsx")

  # Remove the file if it already exists
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # 2. Run the function being tested
  paramTest <- TADA_ParametersForAnalysis(excel = T, overwrite = T)
  # Reload the updated workbook so wb now includes those tabs
  wb <- openxlsx::loadWorkbook(downloads_path)
  nsheets <- length(names(wb))
  # now continue any remaining edits if needed, then final save
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)

  expect_true(file.exists(downloads_path))
  expect_true(nsheets == 3)

  # Remove the file and test with overwrite = F
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }
  # Run the function with overwrite = F now
  msg <- utils::capture.output(
    paramTest2 <- TADA_ParametersForAnalysis(excel = T, overwrite = F),
    type = "message"
  )
  # Reload the updated workbook so wb now includes those tabs
  wb <- openxlsx::loadWorkbook(downloads_path)
  nsheets2 <- length(names(wb))
  expect_true(nsheets2 == 3)
  # now continue any remaining edits if needed, then final save
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)
  # find the timestamped copy index
  idx_pat <- which(grepl("ParamUseMLCrosswalks_", msg, fixed = TRUE))

  # find timestamped path
  timestamp_path <- msg[idx_pat]

  # remove time stamped path
  if (file.exists(timestamp_path)) {
    file.remove(timestamp_path)
  }

  # Clean up after test completes
  on.exit(if (file.exists(downloads_path)) file.remove(downloads_path))
})

testthat::test_that("Excel file generation works with blank inputs in TADA_UsesForAnalysis even when excel file does not exist yet.", {
  # specify downloads path
  downloads_path <- .get_downloads_path("ParamUseMLCrosswalks.xlsx")

  # Remove the file if it already exists
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # Run the function being tested
  usesTest <- TADA_UsesForAnalysis(excel = T, overwrite = T)
  # Reload the updated workbook so wb now includes those tabs
  wb <- openxlsx::loadWorkbook(downloads_path)
  nsheets <- length(names(wb))
  expect_true(nsheets == 4)
  # now continue any remaining edits if needed, then final save
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)

  expect_true(file.exists(downloads_path))

  # Remove the file and test with overwrite = F
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }
  # Run the function with overwrite = F now
  msg <- utils::capture.output(
    usesTest2 <- TADA_UsesForAnalysis(excel = T, overwrite = F),
    type = "message"
  )
  # Reload the updated workbook so wb now includes those tabs
  wb <- openxlsx::loadWorkbook(downloads_path)
  nsheets2 <- length(names(wb))
  # When excel file doesn't exist yet, it will only contain Parameter crosswalk in this file. So it is 1 less than nsheets
  expect_true(nsheets2 == 3)
  # now save again
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)

  # find the timestamped copy index
  idx_pat <- which(grepl("ParamUseMLCrosswalks_", msg, fixed = TRUE))

  # find timestamped path
  timestamp_path <- gsub("Saved as: ", "", msg[idx_pat])

  # remove time stamped path
  if (file.exists(timestamp_path)) {
    file.remove(timestamp_path)
  }

  expect_true(file.exists(downloads_path))

  # Clean up after test completes
  on.exit(if (file.exists(downloads_path)) file.remove(downloads_path))
})

testthat::test_that("Excel file generation works with blank inputs in TADA_MLSummary even when excel file does not exist yet.", {
  # specify downloads path
  downloads_path <- .get_downloads_path("ParamUseMLCrosswalks.xlsx")

  # Remove the file if it already exists
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # Run test
  ML_Test <- TADA_MLSummary(excel = T, overwrite = T)
  # Reload the updated workbook so wb now includes those tabs
  wb <- openxlsx::loadWorkbook(downloads_path)

  nsheets <- length(names(wb))
  expect_true(nsheets == 5)
  # now save again
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)
  # file should exist
  expect_true(file.exists(downloads_path))

  # Remove the file
  if (file.exists(downloads_path)) {
    file.remove(downloads_path)
  }

  # Run the function with overwrite = F now
  msg <- utils::capture.output(
    ML_Test2 <- TADA_MLSummary(excel = T, overwrite = F),
    type = "message"
  )
  # Reload the updated workbook so wb now includes those tabs
  wb <- openxlsx::loadWorkbook(downloads_path)
  nsheets2 <- length(names(wb))
  # When excel file doesn't exist yet, it will only contain Parameter & uses crosswalk in this file. So it is 1 less than nsheets
  expect_true(nsheets2 == 4)
  # now save again
  openxlsx::saveWorkbook(wb, downloads_path, overwrite = TRUE)

  # find the timestamped copy index name
  idx_pat <- which(grepl("ParamUseMLCrosswalks_", msg, fixed = TRUE))

  # find timestamped path
  timestamp_path <- gsub("Saved as: ", "", msg[idx_pat])

  # remove time stamped path
  if (file.exists(timestamp_path)) {
    file.remove(timestamp_path)
  }

  # orginal copy still exists check
  expect_true(file.exists(downloads_path))

  # Clean up after test completes
  on.exit(if (file.exists(downloads_path)) file.remove(downloads_path))
})

# Test TADA_CrosswalkATTAINSWaterTypes
test_that("TADA_CrosswalkATTAINSWaterTypes fills missing ATTAINS.WaterType", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("id1", "id2"),
    TADA.MonitoringLocationTypeName = c("RIVER/STREAM", "LAKE"),
    ATTAINS.WaterType = c(NA, "")
  )

  out <- TADA_CrosswalkATTAINSWaterTypes(df)

  expect_s3_class(out, "data.frame")
  expect_true("ATTAINS.WaterType" %in% names(out))
  expect_false(any(is.na(out$ATTAINS.WaterType)))
})

test_that("TADA_CrosswalkATTAINSWaterTypes does not overwrite existing values when replace_all = FALSE", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("id1", "id2"),
    TADA.MonitoringLocationTypeName = c("RIVER/STREAM", "LAKE"),
    ATTAINS.WaterType = c("CUSTOM TYPE", NA)
  )

  out <- TADA_CrosswalkATTAINSWaterTypes(df, replace_all = FALSE)

  expect_equal(out$ATTAINS.WaterType[1], "CUSTOM TYPE")
})

test_that("TADA_CrosswalkATTAINSWaterTypes overwrites all values when replace_all = TRUE", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("id1", "id2"),
    TADA.MonitoringLocationTypeName = c("RIVER/STREAM", "LAKE"),
    ATTAINS.WaterType = c("CUSTOM TYPE", "ANOTHER TYPE")
  )

  out <- TADA_CrosswalkATTAINSWaterTypes(df, replace_all = TRUE)

  expect_false(any(out$ATTAINS.WaterType %in% c("CUSTOM TYPE", "ANOTHER TYPE")))
})

test_that("TADA_CrosswalkATTAINSWaterTypes creates ATTAINS.WaterType when missing", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("id1", "id2"),
    TADA.MonitoringLocationTypeName = c("RIVER", "LAKE")
  )

  out <- TADA_CrosswalkATTAINSWaterTypes(df)

  expect_true("ATTAINS.WaterType" %in% names(out))
})

test_that("TADA_CrosswalkATTAINSWaterTypes errors when required columns are missing", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("id1", "id2")
  )

  expect_error(
    TADA_CrosswalkATTAINSWaterTypes(df),
    "must contain TADA.MonitoringLocationIdentifier and TADA.MonitoringLocationTypeName"
  )
})

test_that("TADA_CrosswalkATTAINSWaterTypes errors on invalid org_id", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = "id1",
    TADA.MonitoringLocationTypeName = "RIVER"
  )

  expect_error(
    TADA_CrosswalkATTAINSWaterTypes(df, org_id = 1),
    "org_id must be NULL or a single non-NA character string"
  )
})

test_that("TADA_CrosswalkATTAINSWaterTypes errors on invalid replace_all", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = "id1",
    TADA.MonitoringLocationTypeName = "RIVER"
  )

  expect_error(
    TADA_CrosswalkATTAINSWaterTypes(df, replace_all = "yes"),
    "replace_all must be a single non-NA logical"
  )
})

# Test TADA_ReviewATTAINSWaterTypes
test_that("TADA_ReviewATTAINSWaterTypes errors when required columns are missing", {
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = "id1",
    TADA.MonitoringLocationTypeName = "RIVER"
  )

  expect_error(
    TADA_ReviewATTAINSWaterTypes(df),
    "must contain TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationTypeName, and ATTAINS.WaterType"
  )
})

test_that("TADA_ReviewATTAINSWaterTypes creates a flag column for invalid values", {
  skip_if_not_installed("rExpertQuery")

  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = "id1",
    TADA.MonitoringLocationTypeName = "RIVER",
    ATTAINS.WaterType = "INVALID WATER TYPE"
  )

  out <- TADA_ReviewATTAINSWaterTypes(df, review_action = "flag")

  expect_true("TADA.ATTAINSWaterType.Flag" %in% names(out))
  expect_true(any(grepl("does not match any allowable", out$TADA.ATTAINSWaterType.Flag)))
})

test_that("TADA_ReviewATTAINSWaterTypes leaves valid values flagged as valid", {
  skip_if_not_installed("rExpertQuery")

  # Use values that are very likely valid in ATTAINS, but if your environment
  # is uncertain, replace these with values known to be valid in your data.
  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = "id1",
    TADA.MonitoringLocationTypeName = "RIVER/STREAM",
    ATTAINS.WaterType = "STREAM"
  )

  out <- TADA_ReviewATTAINSWaterTypes(df, review_action = "flag")

  expect_true("TADA.ATTAINSWaterType.Flag" %in% names(out))
  expect_true(any(grepl("matches an allowable", out$TADA.ATTAINSWaterType.Flag)) ||
                any(is.na(out$TADA.ATTAINSWaterType.Flag)))
})

test_that("TADA_ReviewATTAINSWaterTypes updates invalid values when review_action = 'update'", {
  skip_if_not_installed("rExpertQuery")

  df <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("id1", "id2"),
    TADA.MonitoringLocationTypeName = c("RIVER/STREAM", "LAKE"),
    ATTAINS.WaterType = c("INVALID WATER TYPE 1", "INVALID WATER TYPE 2")
  )

  out <- TADA_ReviewATTAINSWaterTypes(df, review_action = "update")

  expect_true("TADA.ATTAINSWaterType.Flag" %in% names(out))
  expect_false(any(out$ATTAINS.WaterType %in% c("INVALID WATER TYPE 1", "INVALID WATER TYPE 2")))
})

# Test TADA_CreatePointAUs

test_that("TADA_CreatePointAUs errors when TADA.MonitoringLocationIdentifier is missing", {
  df <- data.frame(
    TADA.MonitoringLocationTypeName = c("Stream", "Lake"),
    stringsAsFactors = FALSE
  )

  expect_error(
    TADA_CreatePointAUs(df),
    "Missing required column\\(s\\): TADA\\.MonitoringLocationIdentifier"
  )
})

test_that("TADA_CreatePointAUs adds missing ATTAINS.AssessmentUnitIdentifier and fills blanks/NA without prefix", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2", "LOC3"),
    TADA.MonitoringLocationTypeName = c("Stream", "Lake", "Estuary"),
    ATTAINS.AssessmentUnitIdentifier = c(NA_character_, "EXISTING_AU_001", ""),
    ATTAINS.WaterType = c("STREAM", "LAKE", "ESTUARY"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUs(df)

  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c(
      "ATTAINS.MonitoringLocationIdentifier",
      "ATTAINS.AssessmentUnitIdentifier",
      "ATTAINS.WaterType"
    )
  )

  expect_equal(
    result$ATTAINS.MonitoringLocationIdentifier,
    c("LOC1", "LOC2", "LOC3")
  )
  expect_equal(
    result$ATTAINS.AssessmentUnitIdentifier,
    c("LOC1", "EXISTING_AU_001", "LOC3")
  )
  expect_equal(result$ATTAINS.WaterType, c("STREAM", "LAKE", "ESTUARY"))
})

test_that("TADA_CreatePointAUs applies auid_prefix only to newly created AUIDs", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2", "LOC3"),
    TADA.MonitoringLocationTypeName = c("Stream", "Lake", "Estuary"),
    ATTAINS.AssessmentUnitIdentifier = c(NA_character_, "EXISTING_AU_001", ""),
    ATTAINS.WaterType = c("STREAM", "LAKE", "ESTUARY"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUs(df, auid_prefix = "WQX_")

  expect_equal(
    result$ATTAINS.AssessmentUnitIdentifier,
    c("WQX_LOC1", "EXISTING_AU_001", "WQX_LOC3")
  )
})

test_that("TADA_CreatePointAUs treats blank AUIDs as missing", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2"),
    TADA.MonitoringLocationTypeName = c("Stream", "Lake"),
    ATTAINS.AssessmentUnitIdentifier = c("   ", ""),
    ATTAINS.WaterType = c("STREAM", "LAKE"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUs(df)

  expect_equal(result$ATTAINS.AssessmentUnitIdentifier, c("LOC1", "LOC2"))
})

test_that("TADA_CreatePointAUs calls TADA_CrosswalkATTAINSWaterTypes when ATTAINS.WaterType is missing", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2"),
    TADA.MonitoringLocationTypeName = c("Stream", "Lake"),
    ATTAINS.AssessmentUnitIdentifier = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  mock_crosswalk <- function(
    .data,
    overwrite_existing = FALSE,
    validation = "none"
  ) {
    expect_false(overwrite_existing)
    expect_identical(validation, "none")
    .data$ATTAINS.WaterType <- c("STREAM", "LAKE")
    .data
  }

  testthat::local_mocked_bindings(
    TADA_CrosswalkATTAINSWaterTypes = mock_crosswalk,
    .env = environment(TADA_CreatePointAUs)
  )

  result <- TADA_CreatePointAUs(df)

  expect_equal(result$ATTAINS.WaterType, c("STREAM", "LAKE"))
  expect_equal(result$ATTAINS.AssessmentUnitIdentifier, c("LOC1", "LOC2"))
})

test_that("TADA_CreatePointAUs calls TADA_CrosswalkATTAINSWaterTypes when ATTAINS.WaterType has blanks", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2"),
    TADA.MonitoringLocationTypeName = c("Stream", "Lake"),
    ATTAINS.AssessmentUnitIdentifier = c(NA_character_, NA_character_),
    ATTAINS.WaterType = c("STREAM", ""),
    stringsAsFactors = FALSE
  )

  mock_crosswalk <- function(
    .data,
    overwrite_existing = FALSE,
    validation = "none"
  ) {
    .data$ATTAINS.WaterType <- c("STREAM", "LAKE")
    .data
  }

  testthat::local_mocked_bindings(
    TADA_CrosswalkATTAINSWaterTypes = mock_crosswalk,
    .env = environment(TADA_CreatePointAUs)
  )

  result <- TADA_CreatePointAUs(df)

  expect_equal(result$ATTAINS.WaterType, c("STREAM", "LAKE"))
})

test_that("TADA_CreatePointAUs errors when water crosswalk is needed but TADA.MonitoringLocationTypeName is missing", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2"),
    ATTAINS.AssessmentUnitIdentifier = c(NA_character_, NA_character_),
    ATTAINS.WaterType = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  expect_error(
    TADA_CreatePointAUs(df),
    "Missing required column: TADA\\.MonitoringLocationTypeName"
  )
})

test_that("TADA_CreatePointAUs returns distinct rows", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC1"),
    TADA.MonitoringLocationTypeName = c("Stream", "Stream"),
    ATTAINS.AssessmentUnitIdentifier = c(NA_character_, NA_character_),
    ATTAINS.WaterType = c("STREAM", "STREAM"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUs(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$ATTAINS.MonitoringLocationIdentifier, "LOC1")
  expect_equal(result$ATTAINS.AssessmentUnitIdentifier, "LOC1")
  expect_equal(result$ATTAINS.WaterType, "STREAM")
})

test_that("TADA_CreatePointAUs does not modify existing non-missing, non-blank AUIDs when prefix is supplied", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2"),
    TADA.MonitoringLocationTypeName = c("Stream", "Lake"),
    ATTAINS.AssessmentUnitIdentifier = c("EXISTING_AU_001", NA_character_),
    ATTAINS.WaterType = c("STREAM", "LAKE"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUs(df, auid_prefix = "WQX_")

  expect_equal(
    result$ATTAINS.AssessmentUnitIdentifier,
    c("EXISTING_AU_001", "WQX_LOC2")
  )
})


