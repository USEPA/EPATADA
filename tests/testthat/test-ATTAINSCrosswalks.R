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

# Test: Auto_assign criteria table should contain all unique TADA.Characteristics/TADA.ComparableDataIdentifier
testthat::test_that("TADA_DefineCriteriaMethodology ", {
  suppressWarnings(
    Criteria_autofill <- TADA_DefineCriteriaMethodology(
      test_dat,
      org_id = NULL, # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = TRUE,
      displayUniqueId = FALSE,
      excel = FALSE
      # uncomment to run the excel file
      # excel = TRUE, overwrite = TRUE
    )
  )

  suppressWarnings(
    Criteria_autofill_w_uniqueID <- TADA_DefineCriteriaMethodology(
      test_dat,
      org_id = NULL, # org_id doesn't need to match WQP. should not matter what org_id is used for testing.
      auto_assign = TRUE,
      displayUniqueId = TRUE,
      excel = FALSE
      # uncomment to run the excel file
      # excel = TRUE, overwrite = TRUE
    )
  )
  # check to make sure all criteria table has same number of TADA.Characteristics/TADA.ComparableDataIdentifiers
  testthat::expect_true(
    length(unique(test_dat$TADA.ComparableDataIdentifier)) ==
      length(unique(
        Criteria_autofill_w_uniqueID$TADA.ComparableDataIdentifier
      )) &&
      length(unique(test_dat$TADA.CharacteristicName)) ==
        length(unique(Criteria_autofill$TADA.CharacteristicName))
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

testthat::test_that("TADA_CrosswalkATTAINSWaterTypes does not replace valid ATTAINS.WaterType entries", {
  # create list of allowable ATTAINS water types
  attains.types <- quiet(
    rExpertQuery::EQ_DomainValues("water_type") |>
      dplyr::select(name) |>
      dplyr::distinct() |>
      dplyr::pull()
  )

  # example TADA df already including an ATTAINS.WaterType column
  MT_exData <- Data_MT_AUMLRef$TADA_with_ATTAINS |>
    sf::st_drop_geometry() |>
    dplyr::filter(ATTAINS.WaterType %in% attains.types)

  # run TADA_CrosswalkATTAINSWaterTypes
  MT_exDataCw <- TADA_CrosswalkATTAINSWaterTypes(MT_exData)

  # compare dfs by anti-join
  MT_compare <- MT_exData |>
    dplyr::anti_join(MT_exDataCw, by = names(MT_exData))

  # check to see that there are no rows in the df resulting from the anti-join
  testthat::expect_equal(NROW(MT_compare), 0)
})

testthat::test_that("In TADA_CrosswalkATTAINSWaterType ATTAINS.WaterType values are only added for rows missing ATTAINS.WaterType", {
  # load test data and drop geometry
  MT_exData <- Data_MT_AUMLRef$TADA_with_ATTAINS |> sf::st_drop_geometry()

  # create a list of TADA.MonitoringLocationIdentifiers with existing ATTAINS.WaterType
  WT_yes <- MT_exData |>
    dplyr::filter(!is.na(ATTAINS.WaterType), ATTAINS.WaterType != "") |>
    dplyr::select(TADA.MonitoringLocationIdentifier) |>
    dplyr::distinct() |>
    dplyr::pull()

  # create a list of TADA.MonitoringLocationIdentifiers without existing ATTAINS.WaterType
  WT_no <- MT_exData |>
    dplyr::filter(!TADA.MonitoringLocationIdentifier %in% WT_yes) |>
    dplyr::select(TADA.MonitoringLocationIdentifier) |>
    dplyr::distinct() |>
    dplyr::pull()

  # add ATTAINS.WaterType only for rows without values in that column
  MT_addMissing <- TADA_CrosswalkATTAINSWaterTypes(MT_exData)

  # compare existing ATTAINS.WaterType before and after running function
  MT_filtYesOrig <- MT_exData |>
    dplyr::filter(TADA.MonitoringLocationIdentifier %in% WT_yes)

  # filter new data set for
  MT_filtYesNew <- MT_addMissing |>
    dplyr::filter(TADA.MonitoringLocationIdentifier %in% WT_yes)

  # compare rows with existing ATTAINS.WaterType before and after running function
  MT_compare <- MT_filtYesNew |>
    dplyr::anti_join(MT_filtYesOrig, by = names(MT_filtYesOrig))

  # filter for rows with newly assigned ATTAINS.WaterType
  MT_filtNoNew <- MT_addMissing |>
    dplyr::filter(
      TADA.MonitoringLocationIdentifier %in% WT_no,
      !is.na(ATTAINS.WaterType),
      ATTAINS.WaterType != ""
    )

  # check to see that now rows with existing ATTAINS.WaterType values were changed
  testthat::expect_equal(NROW(MT_compare), 0)
  # check to see that new ATTAINS.WaterType values were added for rows without existing ATTAINS.WaterType values
  testthat::expect_equal(NROW(MT_filtNoNew), 76)
})

testthat::test_that("TADA_CrosswalkATTAINSWaterType identifies and updates invalid ATTAINS.WaterType values.", {

  # create list of allowable ATTAINS water types
  attains.types <- quiet(
    rExpertQuery::EQ_DomainValues("water_type") |>
      dplyr::select(name) |>
      dplyr::distinct() |>
      dplyr::pull()
  )

  # add ATTAINS.WaterType to TADA df without ATTAINS.WaterType column
  Tribal_addAll <- TADA_CrosswalkATTAINSWaterTypes(
    Data_TribalNations_Harmonized
  )

  # modify tribal example data to include an ATTAINS.WaterType not allowed by ATTAINS
  Tribal_modified <- Tribal_addAll |>
    dplyr::mutate(
      ATTAINS.WaterType = ifelse(
        TADA.MonitoringLocationIdentifier %in%
          c(
            "REDLAKE_WQX-GREE-REDLAKE",
            "UTEMTN-COTTONWOOD WASH SPRING",
            "BLCKFEET-00000054",
            "BLCKFEET-00000056"
          ),
        "INVALID WATER TYPE",
        ATTAINS.WaterType
      )
    )

  # add ATTAINS.WaterType for any rows where it is missing, review all ATTAINS.WaterType
  # values and update any that are not allowed
  Tribal_reviewUpdate <- TADA_CrosswalkATTAINSWaterTypes(
    Tribal_modified,
    review_all = TRUE,
    review_action = "update"
  )

  # filter to retain only monitoring locations that had invalid water types before function was run
  Tribal_reviewFilt <- Tribal_reviewUpdate |>
    dplyr::filter(
      TADA.MonitoringLocationIdentifier %in%
        c(
          "REDLAKE_WQX-GREE-REDLAKE",
          "UTEMTN-COTTONWOOD WASH SPRING",
          "BLCKFEET-00000054",
          "BLCKFEET-00000056"
        )
    ) |>
    dplyr::select(TADA.MonitoringLocationIdentifier, ATTAINS.WaterType) |>
    dplyr::filter(ATTAINS.WaterType %in% attains.types) |>
    dplyr::distinct()

  testthat::expect_equal(NROW(Tribal_reviewFilt), 4)
})
