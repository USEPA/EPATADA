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
  fake_nearby <- Data_Nutrients_UT |> dplyr::mutate(TADA.NearbySiteGroup = 1L)

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

# tests/testthat/test-TADA_ViewNewAUs.R

testthat::test_that("TADA_ViewNewAUs errors when an image path is missing", {
  testdat <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -110
  )

  fake_images <- c(
    "/this/path/does/not/exist.png",
    rep(
      system.file("extdata/icons", "circle-dashed.png", package = "EPATADA"),
      13
    )
  )
  fake_labels <- paste0("label", 1:14)

  testthat::local_mocked_bindings(
    getMapIconLabels = function() list(fake_images, fake_labels),
    .package = "EPATADA"
  )

  testthat::expect_error(TADA_ViewNewAUs(testdat), "Image file not found")
})


testthat::test_that("TADA_ViewNewAUs adds TADA.AURefSource if missing", {
  testdat <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    TADA.MonitoringLocationIdentifier = c("ML1", "ML2"),
    TADA.LatitudeMeasure = c(45, 46),
    TADA.LongitudeMeasure = c(-110, -111)
  )

  fake_img <- system.file(
    "extdata/icons",
    "circle-dashed.png",
    package = "EPATADA"
  )
  fake_images <- rep(fake_img, 14)
  fake_labels <- paste0("label", 1:14)

  testthat::local_mocked_bindings(
    getMapIconLabels = function() list(fake_images, fake_labels),
    createTADABasemap = function(.data) {
      testthat::expect_true("TADA.AURefSource" %in% names(.data))
      testthat::expect_true(all(
        .data$TADA.AURefSource == "TADA_CreatePointAUs"
      ))
      leaflet::leaflet()
    },
    addWQPSites = function(
      .data,
      map,
      icons,
      icon_labels,
      ref_icons,
      overlay_groups
    ) {
      testthat::expect_true("Assessment Units" %in% overlay_groups)
      list(map = map, overlay_groups = overlay_groups)
    },
    addTADAMapLegend = function(...) leaflet::leaflet(),
    addLegendToggle = function(map) map,
    addLayerControl = function(map, overlay_groups) map,
    .package = "EPATADA"
  )

  testthat::expect_s3_class(TADA_ViewNewAUs(testdat), "leaflet")
})


testthat::test_that("TADA_ViewNewAUs preserves existing TADA.AURefSource", {
  testdat <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -110,
    TADA.AURefSource = "SomeOtherSource"
  )

  fake_img <- system.file(
    "extdata/icons",
    "circle-dashed.png",
    package = "EPATADA"
  )
  fake_images <- rep(fake_img, 14)
  fake_labels <- paste0("label", 1:14)

  testthat::local_mocked_bindings(
    getMapIconLabels = function() list(fake_images, fake_labels),
    createTADABasemap = function(.data) {
      testthat::expect_equal(.data$TADA.AURefSource, "SomeOtherSource")
      leaflet::leaflet()
    },
    addWQPSites = function(
      .data,
      map,
      icons,
      icon_labels,
      ref_icons,
      overlay_groups
    ) {
      list(map = map, overlay_groups = overlay_groups)
    },
    addTADAMapLegend = function(...) leaflet::leaflet(),
    addLegendToggle = function(map) map,
    addLayerControl = function(map, overlay_groups) map,
    .package = "EPATADA"
  )

  testthat::expect_s3_class(TADA_ViewNewAUs(testdat), "leaflet")
})


testthat::test_that("TADA_ViewNewAUs passes Assessment Units overlay group to downstream helpers", {
  testdat <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -110
  )

  fake_img <- system.file(
    "extdata/icons",
    "circle-dashed.png",
    package = "EPATADA"
  )
  fake_images <- rep(fake_img, 14)
  fake_labels <- paste0("label", 1:14)

  testthat::local_mocked_bindings(
    getMapIconLabels = function() list(fake_images, fake_labels),
    createTADABasemap = function(.data) leaflet::leaflet(),
    addWQPSites = function(
      .data,
      map,
      icons,
      icon_labels,
      ref_icons,
      overlay_groups
    ) {
      expect_true("Assessment Units" %in% overlay_groups)
      list(map = map, overlay_groups = overlay_groups)
    },
    addTADAMapLegend = function(...) leaflet::leaflet(),
    addLegendToggle = function(map) map,
    addLayerControl = function(map, overlay_groups) {
      expect_true("Assessment Units" %in% overlay_groups)
      map
    },
    .package = "EPATADA"
  )

  testthat::expect_s3_class(TADA_ViewNewAUs(testdat), "leaflet")
})


testthat::test_that("TADA_ViewNewAUs returns a leaflet map object", {
  testdat <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.LatitudeMeasure = 45,
    TADA.LongitudeMeasure = -110
  )

  fake_img <- system.file(
    "extdata/icons",
    "circle-dashed.png",
    package = "EPATADA"
  )
  fake_images <- rep(fake_img, 14)
  fake_labels <- paste0("label", 1:14)

  testthat::local_mocked_bindings(
    getMapIconLabels = function() list(fake_images, fake_labels),
    createTADABasemap = function(.data) leaflet::leaflet(),
    addWQPSites = function(
      .data,
      map,
      icons,
      icon_labels,
      ref_icons,
      overlay_groups
    ) {
      list(map = map, overlay_groups = overlay_groups)
    },
    addTADAMapLegend = function(...) leaflet::leaflet(),
    addLegendToggle = function(map) map,
    addLayerControl = function(map, overlay_groups) map,
    .package = "EPATADA"
  )

  testthat::expect_s3_class(TADA_ViewNewAUs(testdat), "leaflet")
})
