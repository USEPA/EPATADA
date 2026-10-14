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

testthat::test_that("TADA_NearbySitesMap returns a leaflet map with expected marker layers", {
  fake_nearby <- Data_Nutrients_UT |>
    dplyr::mutate(TADA.NearbySiteGroup = 1L)

  testmap <- TADA_NearbySitesMap(fake_nearby)

  testthat::expect_s3_class(testmap, "leaflet")
  testthat::expect_s3_class(testmap, "htmlwidget")

  marker_calls <- Filter(
    function(call) identical(call$method, "addCircleMarkers"),
    testmap$x$calls
  )

  testthat::expect_length(marker_calls, 1)

  marker_data <- marker_calls[[1]]$args[[1]]
  expected_n <- fake_nearby |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::select(TADA.MonitoringLocationIdentifier) |>
    dplyr::distinct() |>
    nrow()

  testthat::expect_equal(length(marker_data), expected_n)
})

