# basic  map tests - checking to see if the functions return leaflet maps
test_that("TADA_ViewATTAINS return leaflet map", {
  testmap <- TADA_ViewATTAINS(Data_MT_AUMLRef)

  expect_true(all(c("leaflet", "htmlwidget") %in% class(testmap)))
})

test_that("TADA_OveriewMap return leaflet map", {
  testmap <- TADA_OverviewMap(Data_Nutrients_UT)

  expect_true(all(c("leaflet", "htmlwidget") %in% class(testmap)))
})

test_that("TADA_FlaggedSitesMap return leaflet map", {
  testmap <- TADA_FlaggedSitesMap(Data_Nutrients_UT)

  expect_true(all(c("leaflet", "htmlwidget") %in% class(testmap)))
})

testthat::test_that("TADA_NearbySitesMap returns a leaflet map", {
  if (
    !tryCatch(
      {
        dummy <- tibble::tibble(
          TADA.MonitoringLocationIdentifier = "dummy",
          TADA.MonitoringLocationName = "dummy",
          TADA.LongitudeMeasure = -110,
          TADA.LatitudeMeasure = 45,
          HorizontalCoordinateReferenceSystemDatumName = "WGS84"
        )
        dummy_sf <- TADA_MakeSpatial(dummy)
        invisible(EPATADA:::fetchNHD(.data = dummy_sf, resolution = "Hi"))
        TRUE
      },
      error = function(e) FALSE
    )
  ) {
    testthat::skip("NHD service is unavailable")
  }

  out <- TADA_NearbySitesMap(...)
  testthat::expect_s3_class(out, "leaflet")
})
