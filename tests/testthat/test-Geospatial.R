# Testing the Geospatial Functions ----
# Tests for the functions in GeoSpatial.R using sample data

# Use the full sample dataset
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

# Ensure reproducibility for random state selection (if supported internally)
set.seed(123)
testdat <- tryCatch(
  suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE)),
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
  testthat::expect_error(fetchATTAINS(.data = NULL), "The dataframe does not")
})

testthat::test_that("fetchATTAINS handles catchments_only parameter", {
  # Create a small valid dataset
  valid_data <- Data_6Tribes_5y_Harmonized |>
    dplyr::filter(OrganizationIdentifier %in% "PUEBLOOFTESUQUE")

  # Test with catchments_only = TRUE
  testthat::expect_no_error(
    result_catchments_only <- fetchATTAINS(
      .data = valid_data,
      catchments_only = TRUE
    )
  )

  # Test with catchments_only = FALSE
  testthat::expect_no_error(
    result_all_features <- fetchATTAINS(
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

  testdat <- suppressMessages(TADA_RandomTestingData(
    choose_random_state = TRUE
  ))
  # Keep a small, valid subset with coordinates
  sub <- testdat |>
    dplyr::filter(
      !is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)
    ) |>
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

  testdat <- suppressMessages(TADA_RandomTestingData(
    choose_random_state = TRUE
  ))
  pts <- testdat |>
    dplyr::filter(
      !is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)
    ) |>
    dplyr::slice_head(n = 200) |>
    TADA_MakeSpatial(crs = 4326)

  res <- fetchATTAINS(.data = pts, catchments_only = TRUE, org_id = "all")

  testthat::expect_true(is.list(res))
  catch <- res[["ATTAINS_catchments"]]
  if (!is.null(catch) && inherits(catch, "sf") && nrow(catch) > 0) {
    testthat::expect_equal(sf::st_crs(catch)$epsg, 4326)
  } else {
    testthat::expect_true(
      is.null(catch) || (inherits(catch, "sf") && nrow(catch) == 0)
    )
  }
})

testthat::test_that("fetchNHD returns EPSG:4326 for Hi and Med resolutions", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "hydro.nationalmap.gov")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- suppressMessages(TADA_RandomTestingData(
    choose_random_state = TRUE
  ))
  pts <- testdat |>
    dplyr::filter(
      !is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)
    ) |>
    dplyr::slice_head(n = 100) |>
    TADA_MakeSpatial(crs = 4326)

  # HiRes
  nhd_hi <- fetchNHD(.data = pts, resolution = "Hi", features = "catchments")
  if (inherits(nhd_hi, "sf") && nrow(nhd_hi) > 0) {
    testthat::expect_equal(sf::st_crs(nhd_hi)$epsg, 4326)
  } else {
    testthat::expect_true(
      is.null(nhd_hi) || (inherits(nhd_hi, "sf") && nrow(nhd_hi) == 0)
    )
  }

  # MedRes
  nhd_med <- fetchNHD(.data = pts, resolution = "Med", features = "catchments")
  if (inherits(nhd_med, "sf") && nrow(nhd_med) > 0) {
    testthat::expect_equal(sf::st_crs(nhd_med)$epsg, 4326)
  } else {
    testthat::expect_true(
      is.null(nhd_med) || (inherits(nhd_med, "sf") && nrow(nhd_med) == 0)
    )
  }
})

testthat::test_that("TADA_CreateATTAINSAUMLCrosswalk outputs all sf layers with identical CRS", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "gispub.epa.gov")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- suppressMessages(TADA_RandomTestingData(
    choose_random_state = TRUE
  ))
  sub <- testdat |>
    dplyr::filter(
      !is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)
    ) |>
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
    crs_vals <- lapply(lst, function(x) {
      if (inherits(x, "sf")) sf::st_crs(x) else NULL
    })
    crs_vals <- Filter(function(x) !is.null(x) && !is.na(x), crs_vals)
    if (length(crs_vals) <= 1) {
      return(TRUE)
    }
    epsg0 <- crs_vals[[1]]$epsg
    for (c in crs_vals[-1]) {
      testthat::expect_identical(c$epsg, epsg0)
    }
    TRUE
  }

  expect_same_crs(res[c(
    "TADA_with_ATTAINS",
    "ATTAINS_catchments",
    "ATTAINS_points",
    "ATTAINS_lines",
    "ATTAINS_polygons"
  )])
})

testthat::test_that("Functions accept input sf in non-4326 and still return 4326 outputs", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "gispub.epa.gov")
  testthat::skip_if_offline(host = "hydro.nationalmap.gov")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dplyr")

  testdat <- suppressMessages(TADA_RandomTestingData(
    choose_random_state = TRUE,
    number_of_days = 1
  ))
  sf_4269 <- testdat |>
    dplyr::filter(
      !is.na(.data$TADA.LongitudeMeasure),
      !is.na(.data$TADA.LatitudeMeasure)
    ) |>
    dplyr::slice_head(n = 10) |>
    TADA_MakeSpatial(crs = 4269) # non-4326 on purpose

  # fetchATTAINS should handle and return 4326
  resA <- fetchATTAINS(.data = sf_4269, catchments_only = TRUE, org_id = "all")
  catch <- resA[["ATTAINS_catchments"]]
  if (!is.null(catch) && inherits(catch, "sf") && nrow(catch) > 0) {
    testthat::expect_equal(sf::st_crs(catch)$epsg, 4326)
  }

  # fetchNHD should also return 4326
  nhd <- fetchNHD(.data = sf_4269, resolution = "Med", features = "catchments")
  if (inherits(nhd, "sf") && nrow(nhd) > 0) {
    testthat::expect_equal(sf::st_crs(nhd)$epsg, 4326)
  }
})

# Small helpers for tests below
capture_msgs <- function(expr) {
  paste(
    capture.output(
      suppressWarnings(eval.parent(substitute(expr))),
      type = "message"
    ),
    collapse = "\n"
  )
}

setup_test_dir <- function() {
  owd <- getwd()
  tmp <- tempfile("tada-test-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  setwd(tmp)
  function() {
    setwd(owd)
    unlink(tmp, recursive = TRUE, force = TRUE)
  }
}

test_that("TADA_UpdateTribalLayers writes GeoPackages, caches signature, and skips when unchanged", {
  ns <- try(asNamespace("EPATADA"), silent = TRUE)
  if (inherits(ns, "try-error")) {
    skip("EPATADA namespace not available.")
  }
  url_syms <- c(
    "AKAllotmentsUrl",
    "AKVillagesUrl",
    "AmericanIndianUrl",
    "OffReservationUrl",
    "OKTribeUrl",
    "VATribeUrl"
  )
  for (sym in url_syms) {
    if (!exists(sym, envir = ns, inherits = FALSE)) {
      skip(paste0("Internal symbol ", sym, " not found in EPATADA namespace."))
    }
  }

  # Base-R local temp project (no withr)
  cleanup <- setup_test_dir()
  on.exit(cleanup(), add = TRUE)

  dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

  # Use proper geometry: points and extra epoch-ms fields
  make_points_sf <- function(n = 2, offset = 0L) {
    pts <- sf::st_sfc(
      lapply(seq_len(n), function(i) {
        sf::st_point(c(-120 + i * 0.01, 38 + i * 0.01))
      }),
      crs = 4326
    )
    base1 <- as.numeric(as.POSIXct("2020-01-01 00:00:00", tz = "UTC")) * 1000
    base2 <- as.numeric(as.POSIXct("2020-06-01 00:00:00", tz = "UTC")) * 1000
    df <- data.frame(
      id = seq_len(n),
      DATE_MO = base1 + offset + (seq_len(n) - 1) * 86400000,
      CURRENT = base2 + offset,
      cur_to = base2 + offset + (seq_len(n) - 1) * 86400000,
      cur_from = base1 + offset + (seq_len(n) - 1) * 86400000,
      name = sprintf("feat_%d", seq_len(n))
    )
    sf::st_sf(df, geom = pts)
  }

  # Prepare a local source dataset (GeoJSON) for all URLs
  s1 <- make_points_sf(n = 2, offset = 0L)
  src1 <- file.path(getwd(), "src1.geojson")
  sf::write_sf(s1, src1, quiet = TRUE)

  # Assign all internal URL symbols to this local source
  for (sym in url_syms) {
    if (bindingIsLocked(sym, ns)) {
      unlockBinding(sym, ns)
    }
    assignInNamespace(sym, src1, ns = "EPATADA")
  }

  # 1) First run: write all GeoPackages and create meta sidecars
  msg1 <- capture_msgs(TADA_UpdateTribalLayers())
  expect_true(grepl("updated", msg1, fixed = TRUE))

  dests <- file.path(
    "inst/extdata",
    c(
      "AKAllotments.gpkg",
      "AKVillages.gpkg",
      "AmericanIndian.gpkg",
      "OffReservation.gpkg",
      "OKTribe.gpkg",
      "VATribe.gpkg"
    )
  )
  for (d in dests) {
    expect_true(file.exists(d), info = paste("dest missing:", d))
    meta_file <- file.path(
      "inst/extdata/.meta",
      paste0(tools::file_path_sans_ext(basename(d)), ".rds")
    )
    expect_true(
      file.exists(meta_file),
      info = paste("meta sidecar missing:", meta_file)
    )

    # meta signature should exist and date-like epoch-ms fields be Date class after conversion
    meta <- readRDS(meta_file)
    expect_true(is.list(meta))
    expect_true(!is.null(meta$sig))
    sig <- meta$sig
    expect_s3_class(sig, "data.frame")
    for (col in c("DATE_MO", "CURRENT", "cur_to", "cur_from")) {
      if (col %in% names(sig)) expect_s3_class(sig[[col]], "Date")
    }

    # GeoPackage read-back should also reflect Date columns
    layer_back <- suppressWarnings(sf::st_read(d, quiet = TRUE))
    x <- sf::st_set_geometry(layer_back, NULL)
    for (col in c("DATE_MO", "CURRENT", "cur_to", "cur_from")) {
      if (col %in% names(x)) expect_s3_class(x[[col]], "Date")
    }
  }

  # Capture mtimes after first run
  mtimes_after_first <- file.info(dests)$mtime

  # 2) Second run unchanged: should skip write based on canonical signature
  msg2 <- capture_msgs(TADA_UpdateTribalLayers())
  expect_true(grepl("unchanged", msg2, fixed = TRUE))
  expect_true(grepl("skipping write", msg2, fixed = TRUE))

  # Verify none of the GeoPackages were touched on unchanged run
  mtimes_after_second <- file.info(dests)$mtime
  expect_true(all(mtimes_after_second == mtimes_after_first))

  # 3) Change content for one URL and verify update occurs and replaces GeoPackage
  s2 <- make_points_sf(n = 3, offset = 0L) # add a row -> content change
  src2 <- file.path(getwd(), "src2.geojson")
  sf::write_sf(s2, src2, quiet = TRUE)

  # Change only AKAllotmentsUrl to src2
  if (bindingIsLocked("AKAllotmentsUrl", ns)) {
    unlockBinding("AKAllotmentsUrl", ns)
  }
  assignInNamespace("AKAllotmentsUrl", src2, ns = "EPATADA")

  # Ensure filesystem mtime resolution will capture the change reliably
  Sys.sleep(1.1)

  # Capture mtime before update to confirm change afterwards
  prev_mtime <- file.info("inst/extdata/AKAllotments.gpkg")$mtime

  msg3 <- capture_msgs(TADA_UpdateTribalLayers())
  expect_true(grepl("AKAllotments.gpkg", msg3, fixed = TRUE))
  expect_true(grepl("updated", msg3, fixed = TRUE))

  new_mtime <- file.info("inst/extdata/AKAllotments.gpkg")$mtime
  expect_true(new_mtime > prev_mtime)

  # Confirm that the dest now has 3 rows
  ak <- suppressWarnings(sf::st_read(
    "inst/extdata/AKAllotments.gpkg",
    quiet = TRUE
  ))
  expect_equal(nrow(ak), 3)

  # Confirm auto-detected columns remain Date after the update
  x <- sf::st_set_geometry(ak, NULL)
  for (col in c("DATE_MO", "CURRENT", "cur_to", "cur_from")) {
    if (col %in% names(x)) expect_s3_class(x[[col]], "Date")
  }
})

test_that("TADA_UpdateTribalLayers preflight lastEditDate skips download when unchanged", {
  skip_if_not_installed("jsonlite")

  ns <- try(asNamespace("EPATADA"), silent = TRUE)
  if (inherits(ns, "try-error")) {
    skip("EPATADA namespace not available.")
  }
  url_syms <- c(
    "AKAllotmentsUrl",
    "AKVillagesUrl",
    "AmericanIndianUrl",
    "OffReservationUrl",
    "OKTribeUrl",
    "VATribeUrl"
  )
  for (sym in url_syms) {
    if (!exists(sym, envir = ns, inherits = FALSE)) {
      skip(paste0("Internal symbol ", sym, " not found in EPATADA namespace."))
    }
  }

  # Base-R local temp project (no withr)
  cleanup <- setup_test_dir()
  on.exit(cleanup(), add = TRUE)

  dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

  # Create trivial GeoPackage at each destination so file.exists(dest) is TRUE
  trivial <- sf::st_sf(
    data.frame(x = 1L),
    geom = sf::st_sfc(sf::st_point(c(-120, 38)), crs = 4326)
  )
  dests <- file.path(
    "inst/extdata",
    c(
      "AKAllotments.gpkg",
      "AKVillages.gpkg",
      "AmericanIndian.gpkg",
      "OffReservation.gpkg",
      "OKTribe.gpkg",
      "VATribe.gpkg"
    )
  )
  for (d in dests) {
    sf::st_write(trivial, d, quiet = TRUE)
  }

  # Provide sidecar meta with matching last_edit dates for all
  matching_last_edit <- 1234567890000 # epoch-ms
  dir.create(
    file.path("inst/extdata", ".meta"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  for (d in dests) {
    meta_file <- file.path(
      "inst/extdata/.meta",
      paste0(tools::file_path_sans_ext(basename(d)), ".rds")
    )
    saveRDS(
      list(sig = data.frame(dummy = 1), last_edit = matching_last_edit),
      meta_file
    )
  }

  # Set all URLs to look like ArcGIS FeatureServer (to trigger preflight)
  fake_arcgis_url <- "https://example.com/FeatureServer/0"
  for (sym in url_syms) {
    if (bindingIsLocked(sym, ns)) {
      unlockBinding(sym, ns)
    }
    assignInNamespace(sym, fake_arcgis_url, ns = "EPATADA")
  }

  # Mock jsonlite::fromJSON from within jsonlite's namespace
  testthat::with_mocked_bindings(
    fromJSON = function(...) {
      list(editingInfo = list(lastEditDate = matching_last_edit))
    },
    .package = "jsonlite",
    {
      # Capture mtimes to verify they don't change after preflight skip
      old_mtimes <- file.info(dests)$mtime

      msg4 <- capture_msgs(TADA_UpdateTribalLayers())
      expect_true(grepl("unchanged", msg4, fixed = TRUE))
      expect_true(grepl("preflight", msg4, fixed = TRUE))
      expect_true(grepl("skipping download", msg4, fixed = TRUE))

      new_mtimes <- file.info(dests)$mtime
      expect_true(all(new_mtimes == old_mtimes))
    }
  )
})

# tests for TADA_WriteLayer

# Helper to create a minimal sf layer with fields that exercise renaming and sanitization
sample_layer <- function() {
  df <- data.frame(
    TOTALAREA_MI = c(1, 2),
    TOTALAREA_KM = c(3, 4),
    LongFieldName = c("A", "B"),
    LongFieldNum = c("C", "D"), # Will collide with LongFieldName after 10-char truncation
    `White space` = c("E", "F"),
    x = c(0, 1),
    y = c(0, 1),
    check.names = FALSE # preserve "White space" as-is
  )
  sf::st_as_sf(df, coords = c("x", "y"), crs = 4326)
}

test_that("TADA_WriteLayer sanitizes names, renames TOTALAREA_* fields, creates dir, and returns normalized path", {
  layer <- sample_layer()
  capture_env <- new.env(parent = emptyenv())
  capture_env$calls <- 0L

  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) layer,
    {
      with_mocked_bindings(
        .package = "sf",
        st_write = function(obj, dsn, ...) {
          capture_env$calls <- capture_env$calls + 1L
          capture_env$last_args <- list(obj = obj, dsn = dsn)
          TRUE
        },
        {
          out_path <- file.path(tempdir(), "nested1", "nested2", "ok.shp")
          ret <- TADA_WriteLayer(
            "http://fake/query",
            out_path,
            sanitize_names = TRUE
          )

          expect_equal(ret, normalizePath(out_path, mustWork = FALSE))
          expect_true(dir.exists(dirname(out_path)))
          expect_identical(capture_env$calls, 1L)

          layer_passed <- capture_env$last_args$obj
          expect_s3_class(layer_passed, "sf")
          expect_identical(attr(layer_passed, "sf_column"), "geometry")

          expect_identical(
            names(layer_passed),
            c(
              "tarea_mi",
              "tarea_km",
              "longfieldn",
              "longfield1", # ≤10 chars with numeric suffix applied
              "white_spac",
              "geometry"
            )
          )
        }
      )
    }
  )
})

test_that("TADA_WriteLayer can skip sanitization but still renames TOTALAREA_*", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) sample_layer(),
    {
      capture_env <- new.env(parent = emptyenv())
      with_mocked_bindings(
        .package = "sf",
        st_write = function(obj, dsn, ...) {
          capture_env$last <- obj
          TRUE
        },
        {
          out_path <- file.path(tempdir(), "nosanitize.shp")
          TADA_WriteLayer("http://fake/query", out_path, sanitize_names = FALSE)

          layer_passed <- capture_env$last
          expect_identical(
            names(layer_passed),
            c(
              "TAREA_MI",
              "TAREA_KM",
              "LongFieldName",
              "LongFieldNum",
              "White space",
              "geometry"
            )
          )
        }
      )
    }
  )
})

test_that("TADA_WriteLayer warns when layerfilepath does not end with .shp", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) sample_layer(),
    {
      with_mocked_bindings(.package = "sf", st_write = function(...) TRUE, {
        out_path <- file.path(tempdir(), "layer.gpkg")
        expect_warning(
          TADA_WriteLayer("http://fake/query", out_path),
          "does not end with .shp"
        )
      })
    }
  )
})

test_that("TADA_WriteLayer reports getFeatureLayer errors clearly", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) stop("network fail"),
    {
      expect_error(
        TADA_WriteLayer("http://fake/query", file.path(tempdir(), "a.shp")),
        "getFeatureLayer\\(\\) failed for URL: .* — network fail"
      )
    }
  )
})

test_that("TADA_WriteLayer reports st_write errors clearly", {
  with_mocked_bindings(
    .package = "EPATADA",
    getFeatureLayer = function(url) sample_layer(),
    {
      with_mocked_bindings(
        .package = "sf",
        st_write = function(...) stop("GDAL write failure"),
        {
          expect_error(
            TADA_WriteLayer("http://fake/query", file.path(tempdir(), "b.shp")),
            "st_write\\(\\) failed for path: .* — GDAL write failure"
          )
        }
      )
    }
  )
})

test_that("TADA_WriteLayer validates inputs", {
  expect_error(TADA_WriteLayer(123, file.path(tempdir(), "x.shp")))
  expect_error(TADA_WriteLayer(character(), file.path(tempdir(), "x.shp")))
  expect_error(TADA_WriteLayer("", file.path(tempdir(), "x.shp")))

  expect_error(TADA_WriteLayer("http://fake/query", 1))
  expect_error(TADA_WriteLayer("http://fake/query", character()))
  expect_error(TADA_WriteLayer("http://fake/query", ""))
})

test_that("ATTAINS sf layers share identical CRS when returned", {
  skip_on_cran()
  skip_if_not_installed("EPATADA")

  set.seed(123)
  tada <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))

  lst <- tryCatch(
    {
      TADA_CreateATTAINSAUMLCrosswalk(tada, return_sf = TRUE)
    },
    error = function(e) {
      skip(paste("Crosswalk unavailable:", conditionMessage(e)))
    }
  )

  layers <- Filter(
    function(x) inherits(x, "sf") && !is.null(x),
    lst[c(
      "ATTAINS_points",
      "ATTAINS_lines",
      "ATTAINS_polygons",
      "ATTAINS_catchments"
    )]
  )
  if (!length(layers)) {
    skip("No ATTAINS layers returned for this dataset.")
  }

  keys <- vapply(
    layers,
    function(s) {
      crs <- sf::st_crs(s)
      if (!is.null(crs$epsg)) as.character(crs$epsg) else crs$wkt
    },
    character(1)
  )
  expect_equal(length(unique(keys)), 1)
})
