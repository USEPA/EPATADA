# tests for TADA_FlagDepthCategory

testthat::test_that("TADA_FlagDepthCategory adds expected colums", {

  expected.cols <- c("TADA.DepthCategory.Flag",
                     "TADA.DepthProfileAggregation.Flag",
                     "TADA.ConsolidatedDepth",
                     "TADA.ConsolidatedDepth.Bottom",
                     "TADA.ConsolidatedDepth.Unit")

  testdat <- TADA_FlagDepthCategory(Data_Nutrients_UT)

  testthat::expect_all_true(expected.cols %in% names(testdat))
})


testthat::test_that("TADA_FlagDepthCategory assigns categories as expected", {

  # create test df
  testdat <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 1.5, 5, 6, 9, 10),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 6),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(10, 20, 30, 40, 50, 60),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc1", 6),
    OrganizationIdentifier = rep("Org1", 6),
    ActivityStartDate = as.Date(rep("2024-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )

  # run flagging function
  testdat.flag <- TADA_FlagDepthCategory(testdat) |>
    dplyr::group_by(TADA.DepthCategory.Flag) |>
    dplyr::summarize(NCat = length(ResultIdentifier)) |>
    dplyr::filter(NCat == 2)

  # test to see that all three categories had two results
  testthat::expect_true(NROW(testdat.flag) == 3)
})

testthat::test_that("TADA_FlagDepthCategory uses result depth when present", {

  # create test df
  testdat <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 1.5),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 2),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(1,2),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep("m:", 2),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 2),
    TADA.CharacteristicName = rep("Chlorophyll", 2),
    TADA.ResultMeasureValue = c(10, 20),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 2),
    ResultIdentifier = paste0("r", 1:2),
    TADA.MonitoringLocationIdentifier = rep("Loc1", 2),
    OrganizationIdentifier = rep("Org1", 2),
    ActivityStartDate = as.Date(rep("2024-01-01", 2)),
    ActivityRelativeDepthName = rep(NA, 2)
  )

  # run flagging function
  testdat.flag <- TADA_FlagDepthCategory(testdat)

  # test to see that all three categories had two results
  testthat::expect_all_true(testdat.flag$TADA.ConsolidatedDepth == 1)
})

testthat::test_that("TADA_FlagDepthCategory filters out non-cateogory rows when clean = TRUE", {

  # create test df
  # create test df
  testdat <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 1.5, 5, 6, 9, NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(10, 10, 10, 10, 10, NA),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(10, 20, 30, 40, 50, 60),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc1", 6),
    OrganizationIdentifier = rep("Org1", 6),
    ActivityStartDate = as.Date(rep("2024-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )

  # run flagging function
  testdat.flag <- TADA_FlagDepthCategory(testdat, clean = TRUE)

  # test to see that all three categories had two results
  testthat::expect_true(NROW(testdat.flag) == 5)
})

testthat::test_that("TADA_FlagDepthCategory consolidated depth columns are NA when no depth data are available", {

  # create test df
  testdat <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 1),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 1),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 1),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(NA),
    TADA.CharacteristicName = rep("Chlorophyll", 1),
    TADA.ResultMeasureValue = c(10),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 1),
    ResultIdentifier = paste0("r", 1:1),
    TADA.MonitoringLocationIdentifier = rep("Loc1", 1),
    OrganizationIdentifier = rep("Org1", 1),
    ActivityStartDate = as.Date(rep("2024-01-01", 1)),
    ActivityRelativeDepthName = rep(NA, 1)
  )

  # run flagging function
  testdat.flag <- TADA_FlagDepthCategory(testdat)

  # test to see that all three categories had two results
  testthat::expect_true(is.na(testdat.flag$TADA.ConsolidatedDepth))
  testthat::expect_true(is.na(testdat.flag$TADA.DepthCategory.Flag))
})

testthat::test_that("TADA_FlagDepthCategory bycategory filters correctly", {

  # create test df
  testdat <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 1.5, 5, 6, 9, 10),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 6),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(10, 20, 30, 40, 50, 60),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc1", 6),
    OrganizationIdentifier = rep("Org1", 6),
    ActivityStartDate = as.Date(rep("2024-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )


  out <- TADA_FlagDepthCategory(testdat, bycategory = "surface", dailyagg = "none")

  testthat::expect_true(all(out$TADA.DepthCategory.Flag == "Surface"))
})

testthat::test_that("TADA_FlagDepthCategory does not increase row count when dailyagg equals 'none' and increases rows count when dailyagg equals 'avg'", {

  # create test df
  testdat <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 1.5, 1.5, 6, 9, 10),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 6),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(10, 20, 30, 40, 50, 60),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc1", 6),
    OrganizationIdentifier = rep("Org1", 6),
    ActivityStartDate = as.Date(rep("2024-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )


  out.avg <- TADA_FlagDepthCategory(testdat, dailyagg = "avg")

  out.none <- TADA_FlagDepthCategory(testdat, dailyagg = "none")

  testthat::expect_true(NROW(out.avg) == 7)
  testthat::expect_true(NROW(out.none) == 6)
  testthat::expect_true(any(startsWith(out.avg$ResultIdentifier, "TADA-")))
})

testthat::test_that("TADA_FlagDepthCategory identifies one record per depth profile when dailyagg equals 'min' or 'max'", {

  # create test df
  testdat.loc1 <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 1.5, 1.5, 6, 9, 10),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 6),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(10, 20, 30, 40, 50, 60),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc1", 6),
    OrganizationIdentifier = rep("Org1", 6),
    ActivityStartDate = as.Date(rep("2024-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )

  testdat.loc2 <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(2, 4, 8, 12, 14, 18),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 6),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(15, 25, 35, 45, 55, 65),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc2", 6),
    OrganizationIdentifier = rep("Org2", 6),
    ActivityStartDate = as.Date(rep("2025-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )

  testdat <- testdat.loc1 |>
    dplyr::bind_rows(testdat.loc2)

  rm(testdat.loc1, testdat.loc2)

  testdat.min<- TADA_FlagDepthCategory(testdat,
                                       dailyagg = "min",
                                       aggregatedonly = TRUE)

  testdat.max<- TADA_FlagDepthCategory(testdat, dailyagg = "max",
                                       aggregatedonly = TRUE)


  testthat::expect_true(NROW(testdat.min) == 2)
  testthat::expect_true(NROW(testdat.max) == 2)
})

testthat::test_that("TADA_FlagDepthCategory aggregatedonly = TRUE with dailyagg = 'none' errors", {

  # create test df
  testdat <- data.frame(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(2, 4, 8, 12, 14, 18),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 6),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(15, 25, 35, 45, 55, 65),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc2", 6),
    OrganizationIdentifier = rep("Org2", 6),
    ActivityStartDate = as.Date(rep("2025-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )


  testthat::expect_error(TADA_FlagDepthCategory(testdat,
                                                dailyagg = 'none',
                                                aggregatedonly = TRUE))
})

testthat::test_that("TADA_FlagDepthCategory errors when required columns is missing", {

  # create test df
  testdat <- data.frame(
    #TADA.ActivityDepthHeightMeasure.MeasureValue = c(2, 4, 8, 12, 14, 18),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = rep("m", 6),
    TADA.ResultDepthHeightMeasure.MeasureValue = rep(NA, 6),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = rep(NA, 6),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = rep(10, 6),
    TADA.CharacteristicName = rep("Chlorophyll", 6),
    TADA.ResultMeasureValue = c(15, 25, 35, 45, 55, 65),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    ResultIdentifier = paste0("r", 1:6),
    TADA.MonitoringLocationIdentifier = rep("Loc2", 6),
    OrganizationIdentifier = rep("Org2", 6),
    ActivityStartDate = as.Date(rep("2025-01-01", 6)),
    ActivityRelativeDepthName = rep(NA, 6)
  )


  testthat::expect_error(TADA_FlagDepthCategory(testdat))
})

# tests for TADA_IDDepthProfiles

testthat::test_that("TADA_IDDepthProfiles works with pre-flagged data", {
  testdat <- data.frame(
    # identifiers
    ResultIdentifier = paste0("r", 1:2),
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC1"),
    TADA.MonitoringLocationName = c("Site 1", "Site 1"),
    TADA.MonitoringLocationTypeName = c("River", "River"),
    OrganizationIdentifier = c("ORG1", "ORG1"),
    ActivityStartDate = as.Date(c("2024-01-01", "2024-01-01")),
    ActivityStartTime.Time = c("10:00", "11:00"),

    # characteristic info
    TADA.CharacteristicName = c("PH", "PH"),
    TADA.ComparableDataIdentifier = c("PH_NA_NA_NA", "PH_NA_NA_NA"),

    # result values
    TADA.ResultMeasureValue = c(7.1, 7.3),
    TADA.ResultMeasure.MeasureUnitCode = c("NA", "NA"),

    # depth info already present
    TADA.ConsolidatedDepth = c(1, 3),
    TADA.ConsolidatedDepth.Unit = c("m", "m"),
    TADA.ConsolidatedDepth.Bottom = c(10, 10),
    TADA.DepthCategory.Flag = c("Surface", "Middle"),
    TADA.DepthProfileAggregation.Flag = c("No aggregation needed", "No aggregation needed"),

    # columns needed by TADA_FlagDepthCategory
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(1, 3),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(1, 3),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(10, 10),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c("m", "m"),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c("m", "m"),
    TADA.ActivityMediaName = c("WATER", "WATER"),
    ActivityMediaSubdivisionName = c(NA_character_, NA_character_),
    ActivityRelativeDepthName = c(NA_character_, NA_character_),

    # more columns used in plot/profile functions
    TADA.MethodSpeciationName = c("NA", "NA"),
    TADA.ResultSampleFractionText = c("NA", "NA")
  )

  out <- TADA_IDDepthProfiles(testdat)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true("TADA.CharacteristicsForDepthProfile" %in% names(out))
  testthat::expect_gt(nrow(out), 0)
})

testthat::test_that("TADA_IDDepthProfiles runs TADA_FlagDepthCategory when needed", {
  df <- tibble::tibble(
    # identifiers
    ResultIdentifier = paste0("r", 1:2),
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC1"),
    TADA.MonitoringLocationName = c("Site 1", "Site 1"),
    TADA.MonitoringLocationTypeName = c("River", "River"),
    OrganizationIdentifier = c("ORG1", "ORG1"),
    ActivityStartDate = as.Date(c("2024-01-01", "2024-01-01")),
    ActivityStartTime.Time = c("10:00", "11:00"),

    # characteristic info
    TADA.CharacteristicName = c("PH", "PH"),
    TADA.ComparableDataIdentifier = c("PH_NA_NA_NA", "PH_NA_NA_NA"),

    # result values
    TADA.ResultMeasureValue = c(7.1, 7.3),
    TADA.ResultMeasure.MeasureUnitCode = c("NA", "NA"),

    # columns needed so TADA_FlagDepthCategory can run
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(1, 3),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(10, 10),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c("m", "m"),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    TADA.ActivityMediaName = c("WATER", "WATER"),
    ActivityMediaSubdivisionName = c(NA_character_, NA_character_),
    ActivityRelativeDepthName = c(NA_character_, NA_character_),

    # columns used by plot/profile functions
    TADA.MethodSpeciationName = c("NA", "NA"),
    TADA.ResultSampleFractionText = c("NA", "NA")
  )

  out <- TADA_IDDepthProfiles(df)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true("TADA.CharacteristicsForDepthProfile" %in% names(out))
  testthat::expect_gt(nrow(out), 0)
})


testthat::test_that("TADA_IDDepthProfile nresults param includes or omits counts correctly", {

  # create test df
  testdat <- Data_Nutrients_UT

  out.include <- TADA_IDDepthProfiles(testdat, nresults = TRUE)

  out.omit <- TADA_IDDepthProfiles(testdat, nresults = FALSE)


  testthat::expect_all_true(grepl("\\(\\d+\\)", out.include$TADA.CharacteristicsForDepthProfile))
  testthat::expect_all_false(grepl("\\(\\d+\\)$", out.omit$TADA.CharacteristicsForDepthProfile))
})

testthat::test_that("TADA_IDDepthProfile nvalue threshold works", {

testdat <- Data_Nutrients_UT


 # id depth profiles
  profiles.n2 <- TADA_IDDepthProfiles(testdat, nvalue = 2)
  profiles.n3 <- TADA_IDDepthProfiles(testdat, nvalue = 3)



  testthat::expect_true(NROW(profiles.n2) == 26)
  testthat::expect_true(NROW(profiles.n3) == 23)
})

testthat::test_that("TADA_IDDepthProfile includes all 'depth params' regardless of count", {

  depth.params <- c(
    "DEPTH, SECCHI DISK DEPTH",
    "DEPTH, SECCHI DISK DEPTH (CHOICE LIST)",
    "DEPTH, SECCHI DISK DEPTH REAPPEARS",
    "TRANSPARENCY, SECCHI TUBE WITH DISK",
    "DEPTH, DATA-LOGGER (NON-PORTED)",
    "DEPTH, DATA-LOGGER (PORTED)",
    "RBP STREAM DEPTH - RIFFLE",
    "RBP STREAM DEPTH - RUN",
    "THALWEG DEPTH"
  )

  testdat <- Data_TribalNations_Harmonized

  testdat.depth.params <- testdat |>
    dplyr::select(TADA.CharacteristicName) |>
    dplyr::filter(TADA.CharacteristicName %in% depth.params) |>
    dplyr::distinct() |>
    dplyr::pull()

  testdat.profile.params <- TADA_IDDepthProfiles(testdat) |>
    dplyr::ungroup() |>
    tidyr::separate_longer_delim(TADA.CharacteristicsForDepthProfile, delim = "; ") |>
    dplyr::select(TADA.CharacteristicsForDepthProfile) |>
    dplyr::distinct() |>
    dplyr::pull()

    testdat.params.list <- sub("_.*", "", testdat.profile.params) |>
      stringr::str_unique() |>
      intersect(depth.params)


  testthat::expect_all_true(sort(testdat.params.list) == sort(testdat.depth.params))
})

testthat::test_that("TADA_IDDepthProfile excludes mean-generated aggregate rows", {

  # one location for test
  testdat <- Data_Nutrients_UT |>
    dplyr::filter(TADA.MonitoringLocationIdentifier == "UTAHDWQ_WQX-5952770")

  # run TADA_FlagDepthCategory with dailyagg = "avg"
  testdat.agg <- TADA_FlagDepthCategory(testdat, dailyagg = "avg")

  testid.agg <- TADA_IDDepthProfiles(testdat.agg)

  # run TADA_FlagDepthCategory with dailyagg = "none"
  testdat.none <- TADA_FlagDepthCategory(testdat, dailyagg = "none")

  testid.none <- TADA_IDDepthProfiles(testdat.none)

  # compare TADA_IDDepthProfile results
  testthat::expect_true(NROW(testid.agg |>
                               dplyr::anti_join(testid.none,
                                                by = dplyr::join_by(TADA.MonitoringLocationIdentifier, TADA.MonitoringLocationName,
                                                                    TADA.MonitoringLocationTypeName, OrganizationIdentifier, ActivityStartDate,
                                                                    TADA.CharacteristicsForDepthProfile))) == 0)
})

# 8) Output has unique rows at expected granularity
# Check that output is unique by:
#
#   TADA.MonitoringLocationIdentifier
# OrganizationIdentifier
# ActivityStartDate
# maybe TADA.CharacteristicsForDepthProfile
# 9) NA result values are removed
# Make sure rows with TADA.ResultMeasureValue = NA do not contribute.

# tests for TADA_DepthProfilePlot

testthat::test_that("TADA_DepthProfilePlot returns a plotly object", {

  testplot <- TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
                        groups = c("TEMPERATURE_NONE_NONE_DEG C", "PH_NONE_NONE_NONE",
                                   "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"),
                        location = "REDLAKE_WQX-BASS-SE",
                        activity_date = "2025-07-16"
   )

  testthat::expect_s3_class(testplot, "plotly")
})

testthat::test_that("TADA_DepthProfilePlot fails when required params are missing", {

  testthat::expect_error(TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
                                    groups = c("TEMPERATURE_NONE_NONE_DEG C", "PH_NONE_NONE_NONE",
                                               "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"),
                                    #location = "REDLAKE_WQX-BASS-SE",
                                    activity_date = "2025-07-16"
                                    ))

  testthat::expect_error(TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
                                               groups = c("TEMPERATURE_NONE_NONE_DEG C", "PH_NONE_NONE_NONE",
                                                          "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"),
                                               location = "REDLAKE_WQX-BASS-SE",
                                               #activity_date = "2025-07-16"
                                               ))

  testthat::expect_error(TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
                                               #groups = c("TEMPERATURE_NONE_NONE_DEG C", "PH_NONE_NONE_NONE",
                                                         # "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"),
                                               location = "REDLAKE_WQX-BASS-SE",
                                               activity_date = "2025-07-16"
  ))
})

testthat::test_that("TADA_DepthProfilePlot fails when selected groups, location or activity date are not in df", {

  testthat::expect_error(TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
                                               groups = c("TEMPERATURE_NONE_NONE_DEG C", "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
                                                          "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"),
                                               location = "REDLAKE_FAKE_LAKE", # location not in df
                                               activity_date = "2025-07-16"
  ))

  testthat::expect_error(TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
                                               groups = c("TEMPERATURE_NONE_NONE_DEG C", "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
                                                          "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"),
                                               location = "REDLAKE_WQX-BASS-SE",
                                               activity_date = "2000-01-01" # activity date not in df
  ))

  testthat::expect_error(TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
                                               groups = c("TEMPERATURE_NONE_NONE_DEG C", "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
                                                "GROUP_DOESNT_EXIST"),
                                               location = "REDLAKE_WQX-BASS-SE",
                                               activity_date = "2025-07-16"
  ))
})


#
# 6) Handles data that are not yet flagged
# Provide raw input without depth category columns and confirm it internally calls TADA_FlagDepthCategory().
#
# 7) Rejects depth unit mismatch
# If input depth unit is "ft" and user asks for "m" but no conversion is present, expect error or at least a warning depending on behavior.
#
# 8) Includes or excludes depth category annotation lines
# Test:
#
#   depthcat = TRUE adds annotations/shapes
# depthcat = FALSE does not
# You can inspect:
#
#   r
# Copy code
#
# expect_true(length(out$x$layout$annotations) > 0)
# or equivalent depending on plotly object structure.
#
# 9) Supports 1, 2, and 3 groups
# Create separate tests for:
#
#   one group
# two groups
# three groups
# Check that the title changes appropriately and traces are added.
#
# 10) Handles depth-parameter groups differently
# If one selected group is a depth parameter like SECCHI DISK DEPTH, confirm it is drawn as a line rather than a scatter trace.
#
# 11) Removes NA result rows from plotting
# Test that NA-result rows are excluded and that the message about NA removal appears.
#
# Use expect_message().
#
# 12) Surface/bottom line rendering
# With depthcat = TRUE and numeric surfacevalue/bottomvalue, check that the plot includes the expected horizontal delineation lines.
#







