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

    TADA.ActivityDepthHeightMeasure.MeasureValue <- c(0.5, 1.5, 8)

    TADA.ResultDepthHeightMeasure.MeasureValue <- c(0.5, 1.5, 8)

    "ActivityRelativeDepthName",
    "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
    "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
    "TADA.CharacteristicName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ResultIdentifier",
    "TADA.MonitoringLocationIdentifier",
    "OrganizationIdentifier",
    "ActivityStartDate"
  )

  testdat <- Data_Nutrients_UT

  testdat2 <- TADA_FlagDepthCategory(testdat)

  testselect <- testdat2 |>
    dplyr::filter(TADA.DepthCategory.Flag == "No depth info")

  getex <- testdat2 |> dplyr::filter(ResultIdentifier %in% c("NWIS-119875779", # bottom
                                                             "STORET-1025042052", # middle
                                                             "NWIS-119875753", # surface,
                                                             "STORET-1007651127", #Epilimnion-surface
                                                             "STORET-1025077262", # hypolimnion-bottom,
                                                             "NWIS-119860547", # No depth info,
                                                             # Not enough depth info to determine category

                                                             ))

  sort(unique(testdat2$TADA.DepthCategory.Flag))


})


