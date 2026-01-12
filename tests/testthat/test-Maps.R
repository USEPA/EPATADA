# Helper to list leaflet call methods for a map htmlwidget
get_leaflet_methods <- function(map) {
  vapply(map$x$calls, function(x) x$method, character(1))
}

# Helper to extract the first call object by method name
extract_call_by_method <- function(map, method) {
  idx <- which(vapply(map$x$calls, function(x) x$method == method, logical(1)))
  if (length(idx) == 0) return(NULL)
  map$x$calls[[idx[1]]]
}

# Simple coalesce operator for tests
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Minimal valid input for error-path test
minimal_overview_df <- function() {
  data.frame(
    MonitoringLocationIdentifier = rep("ML1", 3),
    MonitoringLocationName = rep("Site 1", 3),
    TADA.LatitudeMeasure = rep(40.0, 3),
    TADA.LongitudeMeasure = rep(-120.0, 3),
    ResultIdentifier = c("R1", "R2", "R3"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-02-01", "2020-02-01")),
    TADA.CharacteristicName = c("pH", "pH", "Temperature"),
    OrganizationFormalName = rep("Org A", 3),
    OrganizationIdentifier = rep("ORG_A", 3),
    stringsAsFactors = FALSE
  )
}

test_that("TADA_OverviewMap runs on real random test data and adds key layers (integration)", {
  skip_on_cran()
  skip_if_not_installed("EPATADA")
  
  set.seed(123)
  testdat <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  
  map <- TADA_OverviewMap(testdat)
  expect_s3_class(map, "leaflet")
  methods <- vapply(map$x$calls, function(x) x$method, character(1))
  expect_true("addProviderTiles" %in% methods)
  expect_true("addCircleMarkers" %in% methods)
  expect_true("addLegend" %in% methods)
})

test_that("TADA_OverviewMap errors if required columns are missing", {
  df <- minimal_overview_df()
  df_missing <- subset(df, select = -ResultIdentifier)
  expect_error(TADA_OverviewMap(df_missing))
})

# Synthetic dataset with no flags to assert no markers get added
no_flags_df <- function() {
  data.frame(
    MonitoringLocationIdentifier = "ML3",
    MonitoringLocationName = "Site 3",
    OrganizationFormalName = "Org C",
    TADA.LatitudeMeasure = 40.123,   # precise (>= 3 decimals)
    TADA.LongitudeMeasure = -120.123,
    stringsAsFactors = FALSE
  )
}

test_that("TADA_FlaggedSitesMap runs on real random test data (integration)", {
  skip_on_cran()
  
  set.seed(123)
  testdat <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  
  map <- TADA_FlaggedSitesMap(testdat)
  expect_s3_class(map, "leaflet")
  
  methods <- get_leaflet_methods(map)
  expect_true("addProviderTiles" %in% methods)
  # Not asserting marker categories due to randomness
})

test_that("TADA_FlaggedSitesMap returns a leaflet map even with no flagged rows", {
  map <- TADA_FlaggedSitesMap(no_flags_df())
  expect_s3_class(map, "leaflet")
  methods <- get_leaflet_methods(map)
  expect_equal(sum(methods == "addAwesomeMarkers"), 0)
})

test_that("TADA_NearbySitesMap runs on real random test data and adds circles (integration)", {
  skip_on_cran()
  set.seed(123)
  testdat <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  
  nearby <- TADA_FindNearbySites(testdat)
  has_groups <- any(!is.na(nearby$TADA.NearbySiteGroup))
  
  map <- TADA_NearbySitesMap(testdat, dist_buffer = 100)
  expect_s3_class(map, "leaflet")
  methods <- vapply(map$x$calls, function(x) x$method, character(1))
  expect_true("addProviderTiles" %in% methods)
  if (has_groups) {
    expect_true("addCircleMarkers" %in% methods)
    expect_true("addCircles" %in% methods)
  }
})

# Error-path helper dataframes
minimal_attains_table <- function() {
  data.frame(
    TADA.LongitudeMeasure = -120.2,
    TADA.LatitudeMeasure = 40.2,
    HorizontalCoordinateReferenceSystemDatumName = "WGS84",
    TADA.CharacteristicName = "pH",
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.MonitoringLocationName = "Site 1",
    ResultIdentifier = "RID-1",
    ActivityStartDate = as.Date("2020-01-01"),
    OrganizationFormalName = "Org A",
    OrganizationIdentifier = "ORG_A",
    ATTAINS.AssessmentUnitIdentifier = NA_character_,
    stringsAsFactors = FALSE
  )
}

test_that("TADA_ViewATTAINS runs on real random test data (integration)", {
  skip_on_cran()
  skip_if_not_installed("leaflegend")
  skip_if_not_installed("EPATADA")
  
  set.seed(123)
  tada_data <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  
  attains_list <- tryCatch({
    TADA_CreateATTAINSAUMLCrosswalk(
      tada_data,
      fill_USGS_catch = TRUE,   # optional; set FALSE to avoid USGS path
      return_nearest = TRUE,
      return_sf = TRUE
    )
  }, error = function(e) {
    skip(paste("ATTAINS crosswalk unavailable:", conditionMessage(e)))
  })
  
  map <- TADA_ViewATTAINS(attains_list, ref_icons = FALSE)
  expect_s3_class(map, "leaflet")
  methods <- vapply(map$x$calls, function(x) x$method, character(1))
  expect_true("addProviderTiles" %in% methods)
  expect_true("addMarkers" %in% methods)
  expect_true("addLayersControl" %in% methods)
})

test_that("TADA_ViewATTAINS errors on missing required list names", {
  bad_list <- list(something_else = data.frame())
  expect_error(TADA_ViewATTAINS(bad_list))
})

test_that("TADA_ViewATTAINS errors when WQP-style required columns are missing", {
  attains_table <- minimal_attains_table()
  attains_table <- subset(attains_table, select = -TADA.LongitudeMeasure)
  
  input_list <- list(
    TADA_with_ATTAINS = attains_table,
    ATTAINS_catchments = NULL,
    ATTAINS_points = NULL,
    ATTAINS_lines = NULL,
    ATTAINS_polygons = NULL
  )
  
  expect_error(TADA_ViewATTAINS(input_list))
})

test_that("TADA_ViewATTAINS errors if there are no WQP observations", {
  empty_attains_table <- minimal_attains_table()[0, ]
  
  input_list <- list(
    TADA_with_ATTAINS = empty_attains_table,
    ATTAINS_catchments = NULL,
    ATTAINS_points = NULL,
    ATTAINS_lines = NULL,
    ATTAINS_polygons = NULL
  )
  
  expect_error(TADA_ViewATTAINS(input_list))
})
