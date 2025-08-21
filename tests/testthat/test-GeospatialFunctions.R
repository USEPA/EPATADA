# Testing the Geospatial Functions ----
# Tests for the functions in GeoSpatialFunctions.R using sample data

TADA_dataframe <- Data_HUC8_02070004_Mod1Output %>%
  dplyr::filter(TADA.CharacteristicName == "PH")

TADA_spatial <- TADA_MakeSpatial(TADA_dataframe)

# TADA_MakeSpatial Tests ----
testthat::test_that("TADA_MakeSpatial converts non-spatial data to sf object", {
  test_sf <- TADA_MakeSpatial(.data = TADA_dataframe)

  # Check that result is an sf object
  testthat::expect_s3_class(test_sf, "sf")

  # Check that geometry column exists and contains points
  testthat::expect_true("geometry" %in% names(test_sf))
  testthat::expect_s3_class(sf::st_geometry(test_sf), "sfc_POINT")
})

testthat::test_that("TADA_MakeSpatial preserves input data structure and content", {
  test_sf <- TADA_MakeSpatial(.data = TADA_dataframe)

  # Row count should be preserved
  testthat::expect_equal(nrow(TADA_dataframe), nrow(test_sf))

  # All original columns should be preserved
  testthat::expect_true(all(names(TADA_dataframe) %in% names(test_sf)))

  # Data values should be preserved
  no_geom_test <- sf::st_drop_geometry(test_sf)
  testthat::expect_equal(dim(TADA_dataframe)[1], dim(no_geom_test)[1])
})

testthat::test_that("TADA_MakeSpatial handles custom CRS correctly", {
  test_wgs84 <- TADA_MakeSpatial(.data = TADA_dataframe, crs = 4326)
  test_nad83 <- TADA_MakeSpatial(.data = TADA_dataframe, crs = 4269)

  # Check that the CRS is set correctly
  testthat::expect_equal(sf::st_crs(test_wgs84)$epsg, 4326)
  testthat::expect_equal(sf::st_crs(test_nad83)$epsg, 4269)
})

testthat::test_that("TADA_MakeSpatial fails with appropriate errors", {
  # Test with data that's missing required columns
  invalid_data <- data.frame(a = 1, b = 2)
  testthat::expect_error(
    TADA_MakeSpatial(.data = invalid_data)
  )

  # Test with data that's already spatial
  testthat::expect_error(
    TADA_MakeSpatial(.data = TADA_spatial),
    "Your data is already a spatial object"
  )

  # Test with NULL data
  testthat::expect_error(
    TADA_MakeSpatial(.data = NULL)
  )
})

testthat::test_that("fetchATTAINS fails with appropriate errors", {
  # Test with NULL data
  testthat::expect_error(
    EPATADA:::fetchATTAINS(.data = NULL),
    "The dataframe does not"
  )
})

testthat::test_that("fetchATTAINS handles catchments_only parameter", {
  # Create a small valid dataset
  valid_data <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(-80.0, 35.0))),
    crs = 4326
  )

  # Test with catchments_only = TRUE
  testthat::expect_no_error(
    result_catchments_only <- EPATADA:::fetchATTAINS(.data = valid_data, 
                                                     catchments_only = TRUE)
  )

  # Test with catchments_only = FALSE
  testthat::expect_no_error(
    result_all_features <- EPATADA:::fetchATTAINS(.data = valid_data, catchments_only = FALSE)
  )

  # If we got data back, check that catchments_only returns fewer elements
  if (!is.null(result_catchments_only) && !is.null(result_all_features)) {
    testthat::expect_lte(length(result_catchments_only), length(result_all_features))
  }
})

testthat::test_that("TADA_GetATTAINS correctly identifies already joined ATTAINS data", {
  # Create mock data with ATTAINS columns
  mock_attains_data <- TADA_dataframe
  mock_attains_data$ATTAINS.AssessmentUnitIdentifier <- "TEST"

  testthat::expect_error(
    TADA_CreateATTAINSAUMLCrosswalk(mock_attains_data),
    "Your data has already been joined with ATTAINS data"
  )
})

testthat::test_that("TADA_GetATTAINS handles empty datasets appropriately", {
  # Create an empty dataframe with required structure
  empty_df <- tibble::tibble(
    ResultIdentifier = character(0),
    LongitudeMeasure = character(0),
    LatitudeMeasure = character(0),
    HorizontalCoordinateReferenceSystemDatumName = character(0)
  )

  result <- TADA_CreateATTAINSAUMLCrosswalk(.data = empty_df, return_sf = FALSE)
  testthat::expect_true(nrow(result) == 0)
  testthat::expect_true("ResultIdentifier" %in% names(result))
  testthat::expect_true(any(grepl("^ATTAINS\\.", names(result))))
})


# testthat::test_that("TADA_GetATTAINS rejects invalid resolution values", {
#   testthat::expect_error(
#     TADA_CreateATTAINSAUMLCrosswalk(.data = TADA_dataframe, fill_catchments = TRUE, resolution = "Invalid", return_sf = FALSE),
#     "User-supplied resolution unavailable"
#   )
# })

testthat::test_that("TADA_ViewATTAINS validates input structure", {
  # Test with data that's missing required ATTAINS components
  invalid_data <- list("TADA_with_ATTAINS" = TADA_dataframe)
  testthat::expect_error(
    TADA_ViewATTAINS(invalid_data),
    "Your input dataframe was not produced from"
  )

  # Test with single dataframe instead of list
  testthat::expect_error(
    TADA_ViewATTAINS(TADA_dataframe),
    "Your input dataframe was not produced from"
  )
})

testthat::test_that("TADA_ViewATTAINS rejects empty datasets", {
  # Create an empty dataframe with ATTAINS structure
  empty_attains_df <- tibble::tibble(
    ResultIdentifier = character(0),
    LongitudeMeasure = character(0),
    LatitudeMeasure = character(0),
    CharacteristicName = character(0),
    MonitoringLocationIdentifier = character(0),
    MonitoringLocationName = character(0),
    ActivityStartDate = character(0),
    OrganizationIdentifier = character(0)
  )

  invalid_list <- list(
    "TADA_with_ATTAINS" = empty_attains_df,
    "ATTAINS_catchments" = data.frame(),
    "ATTAINS_points" = data.frame(),
    "ATTAINS_lines" = data.frame(),
    "ATTAINS_polygons" = data.frame()
  )

  testthat::expect_error(
    TADA_ViewATTAINS(invalid_list),
    "Your WQP dataframe has no observations"
  )
})

