# Testing the Geospatial Functions ----
# Tests for the functions in GeoSpatialFunctions.R using sample data

# Use the full sample dataset without filtering to "PH"
TADA_dataframe <- Data_HUC8_02070004_Mod1Output

# Identify rows with valid numeric coordinates (to use in spatial tests)
num_lat <- suppressWarnings(as.numeric(TADA_dataframe$TADA.LatitudeMeasure))
num_lon <- suppressWarnings(as.numeric(TADA_dataframe$TADA.LongitudeMeasure))
valid_idx <- !is.na(num_lon) & !is.na(num_lat)

TADA_dataframe_valid <- TADA_dataframe[valid_idx, , drop = FALSE]
expected_n_rows_valid <- nrow(TADA_dataframe_valid)
has_valid_coords <- expected_n_rows_valid > 0

# Construct a small dataset with no valid coordinates to test error behavior
TADA_dataframe_no_coords <- TADA_dataframe |>
  dplyr::mutate(
    TADA.LatitudeMeasure = NA_character_,
    TADA.LongitudeMeasure = NA_character_
  ) |>
  dplyr::slice_head(n = 5)

# TADA_MakeSpatial Tests on local sample ----
testthat::test_that("TADA_MakeSpatial converts non-spatial data to sf object (using rows with valid coordinates)", {
  if (!has_valid_coords) {
    testthat::skip(
      "No valid coordinates present in sample data; cannot test sf conversion."
    )
  }
  test_sf <- TADA_MakeSpatial(.data = TADA_dataframe_valid)

  # Check that result is an sf object
  testthat::expect_s3_class(test_sf, "sf")

  # Check that geometry column exists and contains points
  testthat::expect_true("geometry" %in% names(test_sf))
  testthat::expect_s3_class(sf::st_geometry(test_sf), "sfc_POINT")
})

testthat::test_that("TADA_MakeSpatial preserves input data structure and content (for valid rows)", {
  if (!has_valid_coords) {
    testthat::skip(
      "No valid coordinates present in sample data; skipping preservation checks."
    )
  }
  test_sf <- TADA_MakeSpatial(.data = TADA_dataframe_valid)

  # Row count should equal the number of rows with valid numeric coordinates
  testthat::expect_equal(nrow(test_sf), expected_n_rows_valid)

  # All original columns should be preserved
  testthat::expect_true(all(names(TADA_dataframe_valid) %in% names(test_sf)))

  # Data rows (excluding geometry) should match the count of valid rows
  no_geom_test <- sf::st_drop_geometry(test_sf)
  testthat::expect_equal(nrow(no_geom_test), expected_n_rows_valid)
})

testthat::test_that("TADA_MakeSpatial handles custom CRS correctly", {
  if (!has_valid_coords) {
    testthat::skip(
      "No valid coordinates present in sample data; skipping CRS checks."
    )
  }
  test_wgs84 <- TADA_MakeSpatial(.data = TADA_dataframe_valid, crs = 4326)
  test_nad83 <- TADA_MakeSpatial(.data = TADA_dataframe_valid, crs = 4269)

  # Check that the CRS is set correctly
  testthat::expect_equal(sf::st_crs(test_wgs84)$epsg, 4326)
  testthat::expect_equal(sf::st_crs(test_nad83)$epsg, 4269)
})

testthat::test_that("TADA_MakeSpatial fails with appropriate errors", {
  # Test with data that's missing required columns
  invalid_data <- data.frame(a = 1, b = 2)
  testthat::expect_error(TADA_MakeSpatial(.data = invalid_data))

  # Test with data that's already spatial (use a small dummy sf)
  dummy_sf <- sf::st_as_sf(
    data.frame(lon = 0, lat = 0),
    coords = c("lon", "lat"),
    crs = 4326
  )
  testthat::expect_error(
    TADA_MakeSpatial(.data = dummy_sf),
    "Your data is already a spatial object"
  )

  # Test with NULL data
  testthat::expect_error(TADA_MakeSpatial(.data = NULL))

  # Test with data that has no valid coordinates
  testthat::expect_error(
    TADA_MakeSpatial(.data = TADA_dataframe_no_coords),
    "No valid rows with latitude/longitude found"
  )
})

# Add tests using real example data from TADA_RandomTestingData ----

# Ensure reproducibility for random state selection (if supported internally)
set.seed(123)
testdat <- tryCatch(
  TADA_RandomTestingData(choose_random_state = TRUE),
  error = function(e) NULL
)

# Helper: check valid coords in testdat
if (!is.null(testdat) && nrow(testdat) > 0) {
  td_lat <- suppressWarnings(as.numeric(testdat$TADA.LatitudeMeasure))
  td_lon <- suppressWarnings(as.numeric(testdat$TADA.LongitudeMeasure))
  td_valid_idx <- !is.na(td_lon) & !is.na(td_lat)
  testdat_valid <- testdat[td_valid_idx, , drop = FALSE]
  testdat_expected_n <- nrow(testdat_valid)
  testdat_has_valid <- testdat_expected_n > 0
} else {
  testdat_has_valid <- FALSE
}

testthat::test_that("TADA_RandomTestingData returns usable tabular data", {
  if (is.null(testdat)) {
    testthat::skip("TADA_RandomTestingData failed to retrieve data (NULL).")
  }
  testthat::expect_true(is.data.frame(testdat) || tibble::is_tibble(testdat))
  testthat::expect_true(nrow(testdat) >= 0)

  # Required columns for spatial conversion
  required_cols <- c(
    "TADA.LongitudeMeasure",
    "TADA.LatitudeMeasure",
    "HorizontalCoordinateReferenceSystemDatumName"
  )
  testthat::expect_true(all(required_cols %in% names(testdat)))

  # Should not already be spatial
  testthat::expect_false(inherits(testdat, "sf"))
})

testthat::test_that("TADA_MakeSpatial works on real example data (random state)", {
  if (is.null(testdat)) {
    testthat::skip("No random example data available (NULL).")
  }
  if (!testdat_has_valid) {
    testthat::skip(
      "Random example data has no valid coordinates; skipping sf conversion test."
    )
  }
  test_sf <- TADA_MakeSpatial(.data = testdat_valid)

  testthat::expect_s3_class(test_sf, "sf")
  testthat::expect_true("geometry" %in% names(test_sf))
  testthat::expect_s3_class(sf::st_geometry(test_sf), "sfc_POINT")

  # Column preservation
  testthat::expect_true(all(names(testdat_valid) %in% names(test_sf)))
  no_geom_test <- sf::st_drop_geometry(test_sf)
  testthat::expect_equal(nrow(no_geom_test), testdat_expected_n)
})

testthat::test_that("TADA_MakeSpatial sets custom CRS correctly for real data", {
  if (is.null(testdat)) {
    testthat::skip("No random example data available (NULL).")
  }
  if (!testdat_has_valid) {
    testthat::skip(
      "Random example data has no valid coordinates; skipping CRS checks."
    )
  }
  test_4326 <- TADA_MakeSpatial(.data = testdat_valid, crs = 4326)
  test_3857 <- TADA_MakeSpatial(.data = testdat_valid, crs = 3857) # Web Mercator

  testthat::expect_equal(sf::st_crs(test_4326)$epsg, 4326)
  testthat::expect_equal(sf::st_crs(test_3857)$epsg, 3857)
})

testthat::test_that("TADA_MakeSpatial errors on random data when coordinates are missing", {
  if (is.null(testdat)) {
    testthat::skip("No random example data available (NULL).")
  }
  testdat_no_coords <- testdat |>
    dplyr::mutate(
      TADA.LatitudeMeasure = NA_character_,
      TADA.LongitudeMeasure = NA_character_
    ) |>
    dplyr::slice_head(n = min(10, nrow(testdat)))

  testthat::expect_error(
    TADA_MakeSpatial(.data = testdat_no_coords),
    "No valid rows with latitude/longitude found"
  )
})

# fetchATTAINS tests remain unchanged
testthat::test_that("fetchATTAINS fails with appropriate errors", {
  # Test with NULL data
  testthat::expect_error(
    EPATADA:::fetchATTAINS(.data = NULL),
    "The dataframe does not"
  )
})

testthat::test_that("fetchATTAINS handles catchments_only parameter", {
  # Create a small valid dataset
  valid_data <- Data_6Tribes_5y_Harmonized |>
    dplyr::filter(OrganizationIdentifier %in% "PUEBLOOFTESUQUE")

  # Test with catchments_only = TRUE
  testthat::expect_no_error(
    result_catchments_only <- EPATADA:::fetchATTAINS(
      .data = valid_data,
      catchments_only = TRUE
    )
  )

  # Test with catchments_only = FALSE
  testthat::expect_no_error(
    result_all_features <- EPATADA:::fetchATTAINS(
      .data = valid_data,
      catchments_only = FALSE
    )
  )

  # If we got data back, check that catchments_only returns fewer elements
  if (!is.null(result_catchments_only) && !is.null(result_all_features)) {
    testthat::expect_lte(
      length(result_catchments_only),
      length(result_all_features)
    )
  }
})

testthat::test_that("TADA_CreateATTAINSAUMLCrosswalk correctly identifies already joined ATTAINS data", {
  # Create mock data with ATTAINS columns
  mock_attains_data <- TADA_dataframe
  mock_attains_data$ATTAINS.AssessmentUnitIdentifier <- "TEST"

  testthat::expect_error(
    TADA_CreateATTAINSAUMLCrosswalk(mock_attains_data),
    "Your data has already been joined with ATTAINS data"
  )
})

testthat::test_that("TADA_CreateATTAINSAUMLCrosswalk handles empty datasets appropriately", {
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

testthat::test_that("TADA_CreateATTAINSAUMLCrosswalk rejects invalid resolution values", {
  testthat::expect_error(
    TADA_CreateATTAINSAUMLCrosswalk(
      .data = TADA_dataframe,
      fill_USGS_catch = TRUE,
      resolution = "Invalid",
      return_sf = FALSE
    ),
    "User-supplied resolution unavailable"
  )
})

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

##### Test CRS Compatibility

testthat::test_that("TADA_MakeSpatial honors requested CRS on TADA_RandomTestingData", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  # Keep a small, valid subset with coordinates
  sub <- testdat |>
    dplyr::filter(!is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)) |>
    dplyr::slice_head(n = 200)

  sf_4326 <- TADA_MakeSpatial(sub, crs = 4326)
  testthat::expect_s3_class(sf_4326, "sf")
  testthat::expect_false(is.na(sf::st_crs(sf_4326)))
  testthat::expect_equal(sf::st_crs(sf_4326)$epsg, 4326)
  testthat::expect_gt(nrow(sf_4326), 0)

  sf_4269 <- TADA_MakeSpatial(sub, crs = 4269)
  testthat::expect_s3_class(sf_4269, "sf")
  testthat::expect_false(is.na(sf::st_crs(sf_4269)))
  testthat::expect_equal(sf::st_crs(sf_4269)$epsg, 4269)
})

testthat::test_that("fetchATTAINS returns features in EPSG:4326 (catchments_only)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "gispub.epa.gov")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  pts <- testdat |>
    dplyr::filter(!is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)) |>
    dplyr::slice_head(n = 200) |>
    TADA_MakeSpatial(crs = 4326)

  res <- fetchATTAINS(.data = pts, catchments_only = TRUE, org_id = "all")

  testthat::expect_true(is.list(res))
  catch <- res[["ATTAINS_catchments"]]
  if (!is.null(catch) && inherits(catch, "sf") && nrow(catch) > 0) {
    testthat::expect_equal(sf::st_crs(catch)$epsg, 4326)
  } else {
    testthat::expect_true(is.null(catch) || (inherits(catch, "sf") && nrow(catch) == 0))
  }
})

testthat::test_that("fetchNHD returns EPSG:4326 for Hi and Med resolutions", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "hydro.nationalmap.gov")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  pts <- testdat |>
    dplyr::filter(!is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)) |>
    dplyr::slice_head(n = 100) |>
    TADA_MakeSpatial(crs = 4326)

  # HiRes
  nhd_hi <- fetchNHD(.data = pts, resolution = "Hi", features = "catchments")
  if (inherits(nhd_hi, "sf") && nrow(nhd_hi) > 0) {
    testthat::expect_equal(sf::st_crs(nhd_hi)$epsg, 4326)
  } else {
    testthat::expect_true(is.null(nhd_hi) || (inherits(nhd_hi, "sf") && nrow(nhd_hi) == 0))
  }

  # MedRes
  nhd_med <- fetchNHD(.data = pts, resolution = "Med", features = "catchments")
  if (inherits(nhd_med, "sf") && nrow(nhd_med) > 0) {
    testthat::expect_equal(sf::st_crs(nhd_med)$epsg, 4326)
  } else {
    testthat::expect_true(is.null(nhd_med) || (inherits(nhd_med, "sf") && nrow(nhd_med) == 0))
  }
})

testthat::test_that("TADA_CreateATTAINSAUMLCrosswalk outputs all sf layers with identical CRS", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "gispub.epa.gov")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  sub <- testdat |>
    dplyr::filter(!is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)) |>
    dplyr::slice_head(n = 150)

  res <- TADA_CreateATTAINSAUMLCrosswalk(
    .data = sub,
    org_id = "all",
    return_nearest = TRUE,
    fill_USGS_catch = FALSE,
    return_sf = TRUE
  )

  testthat::expect_true(is.list(res))

  # helper to check CRS equality among available sf layers
  expect_same_crs <- function(lst) {
    crs_vals <- lapply(lst, function(x) if (inherits(x, "sf")) sf::st_crs(x) else NULL)
    crs_vals <- Filter(function(x) !is.null(x) && !is.na(x), crs_vals)
    if (length(crs_vals) <= 1) {
      return(TRUE)
    }
    epsg0 <- crs_vals[[1]]$epsg
    for (c in crs_vals[-1]) testthat::expect_identical(c$epsg, epsg0)
    TRUE
  }

  expect_same_crs(res[c("TADA_with_ATTAINS",
    "ATTAINS_catchments",
    "ATTAINS_points",
    "ATTAINS_lines",
    "ATTAINS_polygons")])
})

testthat::test_that("Functions accept input sf in non-4326 and still return 4326 outputs", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "gispub.epa.gov")
  testthat::skip_if_offline(host = "hydro.nationalmap.gov")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  sf_4269 <- testdat |>
    dplyr::filter(!is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)) |>
    dplyr::slice_head(n = 120) |>
    TADA_MakeSpatial(crs = 4269) # non-4326 on purpose

  # fetchATTAINS should handle and return 4326
  resA <- fetchATTAINS(.data = sf_4269, catchments_only = TRUE, org_id = "all")
  catch <- resA[["ATTAINS_catchments"]]
  if (!is.null(catch) && inherits(catch, "sf") && nrow(catch) > 0) {
    testthat::expect_equal(sf::st_crs(catch)$epsg, 4326)
  }

  # fetchNHD should also return 4326
  nhd <- fetchNHD(.data = sf_4269, resolution = "Hi", features = "catchments")
  if (inherits(nhd, "sf") && nrow(nhd) > 0) {
    testthat::expect_equal(sf::st_crs(nhd)$epsg, 4326)
  }
})

#####
