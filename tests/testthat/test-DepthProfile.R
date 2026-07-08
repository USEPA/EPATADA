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










# need more details here: 4) Uses activity bottom depth if available
# If TADA.ActivityBottomDepthHeightMeasure.MeasureValue is present, confirm it is used rather than max(TADA.ConsolidatedDepth).




