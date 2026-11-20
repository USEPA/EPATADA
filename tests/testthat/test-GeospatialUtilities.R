testthat::test_that(".TADA_prepareFlaggedSites returns flagged sites with FlaggedColor", {
  flagged <- .TADA_prepareFlaggedSites(Data_Nutrients_UT)

  testthat::expect_true(nrow(flagged) > 0)
  testthat::expect_true("FlaggedColor" %in% names(flagged))
  testthat::expect_true("TADA.SuspectCoordinates.Flag" %in% names(flagged))
})

testthat::test_that(".TADA_addFlaggedLayer adds markers", {
  invalid <- .TADA_prepareFlaggedSites(Data_Nutrients_UT)

  map <- leaflet::leaflet()

  result <- .TADA_addFlaggedLayer(map, invalid)

  testthat::expect_true(all(c("leaflet", "htmlwidget") %in% class(result)))
})

testthat::test_that(".TADA_addFlaggedLegend adds leaflet legend", {
  map <- leaflet::leaflet()

  result <- .TADA_addFlaggedLegend(map)

  testthat::expect_true(all(c("leaflet", "htmlwidget") %in% class(result)))
})
