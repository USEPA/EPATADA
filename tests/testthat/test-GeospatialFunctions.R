# Testing the Geospatial Functions ----
# Tests for the functions in GeoSpatialFunctions.R using sample data

TADA_dataframe <- Data_HUC8_02070004_Mod1Output |>
  dplyr::filter(TADA.CharacteristicName == "PH")

TADA_spatial <- TADA_MakeSpatial(TADA_dataframe)

# Test fixtures
# Hill_MT_pH <- EPATADA::TADA_DataRetrieval(
#   characteristicName = "pH",
#   statecode = "MT",
#   countycode = "041",
#   applyautoclean = TRUE
# )
# large_bbox_data
load(testthat::test_path("testdata", "Hill_MT_pH.rda"))
# small area test as subset of large area
small_bbox_data <- large_bbox_data[125:140, ]
expect_cat_n_small <- 2

# data for nearby sites test
nearby_data <- large_bbox_data |>
  dplyr::filter(OrganizationIdentifier %in% c("CHIPCREE_WQX", "USGS-MT"))

# Query specific to sites along state border
# sites = c("NALMS-F1217605",
#           "EMAP_CS_WQX-RI03-0338-B",
#           "EMAP_CS_WQX-RI05-0016-A",
#           "NARS_WQX-NCCA10-1634",
#           "NARS_WQX-NCA_RI-10129"
#           )
# RI_CT_secchi <- EPATADA::TADA_DataRetrieval(
#  characteristicName = "Depth, Secchi disk depth",
#  siteid = sites,
#  applyautoclean = TRUE
#  )
# RI_CT_secchi
load(testthat::test_path("testdata", "RI_CT_secchi.rda"))

# test_au_ref_MTDEQ.rda is static, but was generated using:
# MT_AU_MLRef <- TADA_GetATTAINSAUMLCrosswalk(org_id = "MTDEQ")
# test_au_ref_MTDEQ <- TADA_UpdateATTAINSAUMLCrosswalk(org_id = "MTDEQ",
#                                                     crosswalk = MT_AU_MLRef)
load(testthat::test_path("testdata", "test_au_ref_MTDEQ.rda"))

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
  testthat::expect_error(TADA_MakeSpatial(.data = invalid_data))

  # Test with data that's already spatial
  testthat::expect_error(
    TADA_MakeSpatial(.data = TADA_spatial),
    "Your data is already a spatial object"
  )

  # Test with NULL data
  testthat::expect_error(TADA_MakeSpatial(.data = NULL))
})


testthat::test_that("fetchATTAINS fails with appropriate errors", {
  # Test with NULL data
  testthat::expect_error(
    EPATADA:::fetchATTAINS(.data = NULL),
    "The dataframe does not"
  )
})

testthat::test_that("fetchATTAINS handles small areas", {
  # small_bbox_data is subset of large_bbox_data fixture (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_all_features <- EPATADA:::fetchATTAINS(.data = small_bbox_data)
  )
  testthat::expect_null(result_all_features$ATTAINS_points)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_lines), 2)
  testthat::expect_null(result_all_features$ATTAINS_polygons)
  testthat::expect_equal(
    NROW(result_all_features$ATTAINS_catchments),
    expect_cat_n_small
  )
})

testthat::test_that("fetchATTAINS handles large areas", {
  # large_bbox_data from fixtures (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_all_features <- EPATADA:::fetchATTAINS(.data = large_bbox_data)
  )
  testthat::expect_null(result_all_features$ATTAINS_points)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_lines), 10)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_polygons), 1)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_catchments), 43)
})

testthat::test_that("fetchATTAINS catchments_only parameter", {
  testthat::expect_no_error(
    result_catchments_only <- EPATADA:::fetchATTAINS(
      .data = small_bbox_data,
      catchments_only = TRUE
    )
  )
  testthat::expect_null(nrow(result_catchments_only$ATTAINS_points))
  testthat::expect_null(nrow(result_catchments_only$ATTAINS_lines))
  testthat::expect_null(nrow(result_catchments_only$ATTAINS_polygons))
  # Compare against catchments_only = FALSE (default)
  testthat::expect_equal(
    nrow(result_catchments_only$ATTAINS_catchments),
    expect_cat_n_small
  )
})

testthat::test_that("fetchATTAINS org_id parameter", {
  # Test when non-default (default is 'all')
  org <- "RIDEM"
  testthat::expect_no_error(
    org_results <- EPATADA:::fetchATTAINS(
      .data = RI_CT_secchi,
      catchments_only = TRUE,
      org_id = org
    )
  )
  # Test against normal result when filtered on org_id
  all_org_results <- EPATADA:::fetchATTAINS(
    .data = RI_CT_secchi,
    catchments_only = TRUE
  )
  all_orgs_filtered <- all_org_results$ATTAINS_catchments[
    "organizationid" == org
  ]
  # Compare the two sets of results (should be same)
  testthat::expect_equal(
    NROW(org_results$ATTAINS_catchments),
    NROW(all_orgs_filtered)
  )
})

# make mock data sets for fetchNHD tests
make_fake_hi_nhd <- function() {
  polys <- lapply(1:16, function(i) {
    x <- -90 + i * 0.001
    y <- 40 + i * 0.001
    sf::st_polygon(list(rbind(
      c(x, y),
      c(x, y + 0.0005),
      c(x + 0.0005, y + 0.0005),
      c(x + 0.0005, y),
      c(x, y)
    )))
  })

  sf::st_sf(
    nhdplusid = as.character(1:16),
    areasqkm = rep(1.0, 16),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

make_fake_hr_catchments <- function() {
  sf::st_sf(
    NHD.nhdplusid = c("1", "2", "3"),
    NHD.resolution = c("HR", "HR", "HR"),
    NHD.catchmentareasqkm = c(1.0, 1.1, 1.2),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-90.000, 40.000),
        c(-90.000, 40.001),
        c(-89.999, 40.001),
        c(-89.999, 40.000),
        c(-90.000, 40.000)
      ))),
      sf::st_polygon(list(rbind(
        c(-90.002, 40.002),
        c(-90.002, 40.003),
        c(-90.001, 40.003),
        c(-90.001, 40.002),
        c(-90.002, 40.002)
      ))),
      sf::st_polygon(list(rbind(
        c(-90.004, 40.004),
        c(-90.004, 40.005),
        c(-90.003, 40.005),
        c(-90.003, 40.004),
        c(-90.004, 40.004)
      ))),
      crs = 4326
    )
  )
}

make_fake_hr_flowlines <- function() {
  sf::st_sf(
    flowline_id = c("f1", "f2", "f3"),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(-90.000, 40.000), c(-89.999, 40.001))),
      sf::st_linestring(rbind(c(-90.002, 40.002), c(-90.001, 40.003))),
      sf::st_linestring(rbind(c(-90.004, 40.004), c(-90.003, 40.005))),
      crs = 4326
    )
  )
}

make_fake_hr_waterbodies <- function() {
  sf::st_sf(
    wb_id = c("w1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-90.000, 40.000),
        c(-90.000, 40.0005),
        c(-89.9995, 40.0005),
        c(-89.9995, 40.000),
        c(-90.000, 40.000)
      ))),
      crs = 4326
    )
  )
}

make_fake_med_catchments <- function() {
  sf::st_sf(
    NHD.comid = c("10", "11"),
    NHD.resolution = c("nhdplusV2", "nhdplusV2"),
    NHD.catchmentareasqkm = c(10, 20),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-90.000, 40.000),
        c(-90.000, 40.001),
        c(-89.999, 40.001),
        c(-89.999, 40.000),
        c(-90.000, 40.000)
      ))),
      sf::st_polygon(list(rbind(
        c(-90.002, 40.002),
        c(-90.002, 40.003),
        c(-90.001, 40.003),
        c(-90.001, 40.002),
        c(-90.002, 40.002)
      ))),
      crs = 4326
    )
  )
}

make_fake_med_flowlines <- function() {
  sf::st_sf(
    flowline_id = c("mf1", "mf2"),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(-90.000, 40.000), c(-89.999, 40.001))),
      sf::st_linestring(rbind(c(-90.002, 40.002), c(-90.001, 40.003))),
      crs = 4326
    )
  )
}

make_fake_med_waterbodies <- function() {
  sf::st_sf(
    wb_id = c("mw1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-90.000, 40.000),
        c(-90.000, 40.0005),
        c(-89.9995, 40.0005),
        c(-89.9995, 40.000),
        c(-90.000, 40.000)
      ))),
      crs = 4326
    )
  )
}

testthat::test_that("fetchNHD handles small areas with defaults", {

  fake_hi <- sf::st_sf(
    NHD.nhdplusid = as.character(1:16),
    NHD.resolution = rep("HR", 16),
    NHD.catchmentareasqkm = rep(1.0, 16),
    geometry = sf::st_sfc(
      lapply(1:16, function(i) {
        x <- -90 + i * 0.001
        y <- 40 + i * 0.001
        sf::st_polygon(list(rbind(
          c(x, y),
          c(x, y + 0.0005),
          c(x + 0.0005, y + 0.0005),
          c(x + 0.0005, y),
          c(x, y)
        )))
      }),
      crs = 4326
    )
  )

  testthat::local_mocked_bindings(
    .nhd_get_hr_catchments = function(nhd_hr_catchments, wqp_bboxes) fake_hi,
    .package = "EPATADA"
  )

  result <- EPATADA:::fetchNHD(
    .data = small_bbox_data,
    check_service = FALSE
  )

  testthat::expect_equal(nrow(result), 16)
})

testthat::test_that("fetchNHD returns Hi flowlines and waterbodies", {

  fake_catchments <- make_fake_hr_catchments()
  fake_flowlines <- make_fake_hr_flowlines()
  fake_waterbodies <- make_fake_hr_waterbodies()

  testthat::local_mocked_bindings(
    .nhd_get_hr_catchments = function(...) fake_catchments,
    .nhd_get_hr_flowlines = function(...) fake_flowlines,
    .nhd_get_hr_waterbodies = function(...) fake_waterbodies,
    .package = "EPATADA"
  )

  result <- EPATADA:::fetchNHD(
    small_bbox_data,
    features = c("catchments", "flowlines", "waterbodies"),
    check_service = FALSE
  )

  testthat::expect_true(is.list(result))
  testthat::expect_true(all(c(
    "fill_USGS_catchments",
    "NHD_flowlines",
    "NHD_waterbodies"
  ) %in% names(result)))
})

testthat::test_that("fetchNHD returns Med catchments", {

  fake_med <- make_fake_med_catchments()

  testthat::local_mocked_bindings(
    .nhd_get_med_catchments = function(...) fake_med,
    .package = "EPATADA"
  )

  result <- EPATADA:::fetchNHD(
    small_bbox_data,
    resolution = "Med",
    features = "catchments",
    check_service = FALSE
  )

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true(all(c(
    "NHD.comid",
    "NHD.resolution",
    "NHD.catchmentareasqkm"
  ) %in% names(result)))
})

testthat::test_that("fetchNHD returns Med flowlines and waterbodies", {

  fake_med_catchments <- make_fake_med_catchments()
  fake_med_flowlines <- make_fake_med_flowlines()
  fake_med_waterbodies <- make_fake_med_waterbodies()

  testthat::local_mocked_bindings(
    .nhd_get_med_catchments = function(...) fake_med_catchments,
    .nhd_get_med_flowlines = function(...) fake_med_flowlines,
    .nhd_get_med_waterbodies = function(...) fake_med_waterbodies,
    .package = "EPATADA"
  )

  result <- EPATADA:::fetchNHD(
    small_bbox_data,
    resolution = "Med",
    features = c("catchments", "flowlines", "waterbodies"),
    check_service = FALSE
  )

  testthat::expect_true(is.list(result))
  testthat::expect_true(all(c(
    "fill_USGS_catchments",
    "NHD_flowlines",
    "NHD_waterbodies"
  ) %in% names(result)))
})

testthat::test_that("fetchNHD error when invalid features param", {

  testthat::expect_error(
    EPATADA:::fetchNHD(
      small_bbox_data,
      features = "Hi",
      check_service = FALSE
    ),
    "Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument."
  )
})

testthat::test_that("fetchNHD error when invalid resolution param", {

  testthat::expect_error(
    EPATADA:::fetchNHD(
      small_bbox_data,
      resolution = "Lo",
      check_service = FALSE
    ),
    "User-supplied resolution unavailable"
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
  testthat::expect_true(NROW(result) == 0)
  testthat::expect_true("ResultIdentifier" %in% names(result))
  testthat::expect_true(any(grepl("^ATTAINS\\.", names(result))))
})

testthat::test_that("Get ATTAINS by Assessment Unit ID", {
  # au_id_list <- test_au_ref_MTDEQ$ATTAINS.AssessmentUnitIdentifier

  # When run with defaults (no ExpertQuery fields)
  testthat::skip_on_cran()
  testthat::skip_if_offline("gispub.epa.gov")

  actual_default <- tryCatch(
    TADA_GetATTAINSByAUID(Data_MT_MissoulaCounty, test_au_ref_MTDEQ),
    error = function(e) {
      testthat::skip(paste(
        "ATTAINS default query failed:",
        conditionMessage(e)
      ))
    }
  )

  # Check .data was updated by adding 83 cols (163+83=246)
  testthat::expect_equal(ncol(actual_default$TADA_with_ATTAINS), 246)
  # Check results based on number of rows
  expected_rows <- c(0, 5, 1)
  testthat::expect_equal(NROW(actual_default$ATTAINS_points), expected_rows[1])
  testthat::expect_equal(NROW(actual_default$ATTAINS_lines), expected_rows[2])
  testthat::expect_equal(
    NROW(actual_default$ATTAINS_polygons),
    expected_rows[3]
  )
  # When default fill_ATTAINS_catch = FALSE, catchments are NULL
  testthat::expect_null(actual_default$ATTAINS_catchments)

  # Run with catchments
  actual_catchments <- tryCatch(
    TADA_GetATTAINSByAUID(
      Data_MT_MissoulaCounty,
      test_au_ref_MTDEQ,
      fill_ATTAINS_catch = TRUE
    ),
    error = function(e) {
      testthat::skip(paste(
        "ATTAINS catchment query failed:",
        conditionMessage(e)
      ))
    }
  )

  # Skip if the service returns no spatial features (avoid false failures)
  n_catchments <- NROW(actual_catchments$ATTAINS_catchments)
  n_lines <- NROW(actual_catchments$ATTAINS_lines)
  n_polygons <- NROW(actual_catchments$ATTAINS_polygons)

  if ((n_catchments + n_lines + n_polygons) == 0) {
    testthat::skip(sprintf(
      "ATTAINS returned no spatial features (catchments = %d, lines = %d, polygons = %d); skipping to avoid false failure.",
      n_catchments,
      n_lines,
      n_polygons
    ))
  }

  # Check results based on number of rows (only catchments change from default)
  expected_rows <- c(11, expected_rows)
  testthat::expect_equal(
    NROW(actual_catchments$ATTAINS_catchments),
    expected_rows[1]
  )
  testthat::expect_equal(
    NROW(actual_catchments$ATTAINS_points),
    expected_rows[2]
  )
  testthat::expect_equal(
    NROW(actual_catchments$ATTAINS_lines),
    expected_rows[3]
  )
  testthat::expect_equal(
    NROW(actual_catchments$ATTAINS_polygons),
    expected_rows[4]
  )
})

# new TADA_CreateAUMLCrosswalk tests
testthat::test_that("TADA_CreateAUMLCrosswalk correctly identifies already joined ATTAINS data", {
  # Create mock data with ATTAINS columns
  mock_attains_data <- TADA_dataframe
  mock_attains_data$ATTAINS.AssessmentUnitIdentifier <- "TEST"

  testthat::expect_error(
    TADA_CreateATTAINSAUMLCrosswalk(mock_attains_data),
    "Your data has already been joined with ATTAINS data"
  )
})

testthat::test_that("TADA_CreateAUMLCrosswalk handles empty datasets appropriately", {
  # Create an empty dataframe with required structure
  empty_df <- tibble::tibble(
    ResultIdentifier = character(0),
    LongitudeMeasure = character(0),
    LatitudeMeasure = character(0),
    HorizontalCoordinateReferenceSystemDatumName = character(0)
  )

  result <- TADA_CreateAUMLCrosswalk(.data = empty_df)
  testthat::expect_true(length(result) == 5)
  testthat::expect_true("ResultIdentifier" %in% names(result$TADA_with_ATTAINS))
  testthat::expect_true(any(grepl(
    "^ATTAINS\\.",
    names(result$TADA_with_ATTAINS)
  )))
})


testthat::test_that("TADA_CreateAUMLCrosswalk contains expected AU Ref Source values", {
  # Uses example data set that has already had TADA_CreateAUMLCrosswalk applied
  au.sources <- sort(unique(Data_MT_AUMLRef$ATTAINS_crosswalk$TADA.AURefSource))

  expected <- c(
    "User-supplied Ref",
    "ATTAINS Crosswalk",
    "TADA_CreateATTAINSAUMLCrosswalk"
  )

  # Tests to ensure that all expected values of TADA.AURefSource are returned
  missing <- setdiff(expected, au.sources)
  testthat::expect_equal(missing, character(0))
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

testthat::test_that("TADA_FindNearbySites returns no nearby sites when points are far apart", {

  TADA_fake <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("site1", "site2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-90, -80),
    TADA.LatitudeMeasure = c(40, 50),
    HorizontalCoordinateReferenceSystemDatumName = c("WGS84", "WGS84"),
    OrganizationIdentifier = c("org1", "org2"),
    TADA.MonitoringLocationTypeName = c("WELL", "WELL"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02")),
    TADA.ResultMeasureValue = c(1, 2)
  )

  fake_nhd <- sf::st_sf(
    NHD.nhdplusid = "1001",
    NHD.resolution = "HR",
    NHD.catchmentareasqkm = 1.23,
    geometry = sf::st_sfc(sf::st_point(c(-90, 40)), crs = 4326)
  )

  testthat::local_mocked_bindings(
    .safe_fetchNHD = function(...) fake_nhd,
    .package = "EPATADA"
  )

  result <- EPATADA::TADA_FindNearbySites(TADA_fake, catchment = TRUE, dist_buffer = 100)

  testthat::expect_true(all(is.na(result$TADA.NearbySiteGroup)))
  testthat::expect_true(all(result$TADA.NearbySites.Flag == "No nearby sites detected."))
})

testthat::test_that("TADA_FindNearbySites groups nearby sites across organizations when by_org = FALSE", {

  TADA_fake <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("site1", "site2", "site3"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2", "Site 3"),
    TADA.LongitudeMeasure = c(-90.0000, -90.0001, -90.0002),
    TADA.LatitudeMeasure = c(40.0000, 40.0001, 40.0002),
    HorizontalCoordinateReferenceSystemDatumName = c("WGS84", "WGS84", "WGS84"),
    OrganizationIdentifier = c("org1", "org2", "org1"),
    TADA.MonitoringLocationTypeName = c("WELL", "WELL", "STREAM"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    TADA.ResultMeasureValue = c(1, 2, 3)
  )

  fake_nhd <- sf::st_sf(
    NHD.nhdplusid = c("1001", "1001", "1003"),
    NHD.resolution = c("HR", "HR", "HR"),
    NHD.catchmentareasqkm = c(1.1, 1.2, 1.3),
    geometry = sf::st_sfc(
      sf::st_point(c(-90.0000, 40.0000)),
      sf::st_point(c(-90.0001, 40.0001)),
      sf::st_point(c(-90.0002, 40.0002)),
      crs = 4326
    )
  )

  testthat::local_mocked_bindings(
    .safe_fetchNHD = function(...) fake_nhd,
    .package = "EPATADA"
  )

  result <- EPATADA::TADA_FindNearbySites(
    TADA_fake,
    catchment = TRUE,
    by_org = FALSE,
    dist_buffer = 1000
  )

  testthat::expect_true(any(!is.na(result$TADA.NearbySiteGroup)))

  grouped <- result |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::group_by(TADA.NearbySiteGroup) |>
    dplyr::summarise(n_orgs = dplyr::n_distinct(OrganizationIdentifier), .groups = "drop")

  testthat::expect_true(any(grouped$n_orgs > 1))
})


testthat::test_that("TADA_FindNearbySites separates nearby sites by organization when by_org = TRUE", {

  TADA_fake <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("site1", "site2", "site3"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2", "Site 3"),
    TADA.LongitudeMeasure = c(-90.0000, -90.0001, -90.0002),
    TADA.LatitudeMeasure = c(40.0000, 40.0001, 40.0002),
    HorizontalCoordinateReferenceSystemDatumName = c("WGS84", "WGS84", "WGS84"),
    OrganizationIdentifier = c("org1", "org2", "org1"),
    TADA.MonitoringLocationTypeName = c("WELL", "WELL", "STREAM"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    TADA.ResultMeasureValue = c(1, 2, 3)
  )

  fake_nhd <- sf::st_sf(
    NHD.nhdplusid = c("1001", "1001", "1001"),
    NHD.resolution = c("HR", "HR", "HR"),
    NHD.catchmentareasqkm = c(1.1, 1.2, 1.3),
    geometry = sf::st_sfc(
      sf::st_point(c(-90.0000, 40.0000)),
      sf::st_point(c(-90.0001, 40.0001)),
      sf::st_point(c(-90.0002, 40.0002)),
      crs = 4326
    )
  )

  testthat::local_mocked_bindings(
    .safe_fetchNHD = function(...) fake_nhd,
    .package = "EPATADA"
  )

  result <- EPATADA::TADA_FindNearbySites(
    TADA_fake,
    catchment = TRUE,
    by_org = TRUE,
    dist_buffer = 1000
  )

  grouped <- result |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::group_by(TADA.NearbySiteGroup) |>
    dplyr::summarise(n_orgs = dplyr::n_distinct(OrganizationIdentifier), .groups = "drop")

  testthat::expect_true(nrow(grouped) == 1)
  testthat::expect_true(all(grouped$n_orgs == 1))
})

testthat::test_that("TADA_FindNearbySites selects metadata by count", {

  TADA_fake <- tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("site1", "site1", "site1", "site2"),
    TADA.MonitoringLocationName = c("Site 1", "Site 1", "Site 1", "Site 2"),
    TADA.LongitudeMeasure = c(-90.0000, -90.0000,-90.0000, -90.0003),
    TADA.LatitudeMeasure = c(40.0000, 40.0000, 40.0000, 40.0003),
    HorizontalCoordinateReferenceSystemDatumName = c("WGS84", "WGS84", "WGS84", "WGS84"),
    OrganizationIdentifier = c("org1", "org1", "org1", "org2"),
    TADA.MonitoringLocationTypeName = c("STREAM", "STREAM", "STREAM", "WELL"),
    ActivityStartDate = as.Date(c("2020-01-01", "2021-01-01", "2021-01-02", "2020-01-03")),
    TADA.ResultMeasureValue = c(1, 2, 3, 4)
  )

  fake_nhd <- sf::st_sf(
    NHD.nhdplusid = c("1001", "1001", "1001", "1001"),
    NHD.resolution = c("HR", "HR", "HR", "HR"),
    NHD.catchmentareasqkm = c(1.1, 1.1, 1.1, 1.1),
    geometry = sf::st_sfc(
      sf::st_point(c(-90.0000, 40.0000)),
      sf::st_point(c(-90.0000, 40.0000)),
      sf::st_point(c(-90.0000, 40.0000)),
      sf::st_point(c(-90.0003, 40.0003)),
      crs = 4326
    )
  )

  testthat::local_mocked_bindings(
    .safe_fetchNHD = function(...) fake_nhd,
    .package = "EPATADA"
  )

  result <- EPATADA::TADA_FindNearbySites(
    TADA_fake,
    org_hierarchy = "none",
    meta_select = "count",
    dist_buffer = 1000
  )

  result.watertype <- result |>
    dplyr::filter(OrganizationIdentifier == "org2")

  testthat::expect_true(result$TADA.MonitoringLocationTypeName[1] == "STREAM")
  testthat::expect_true(result$TADA.MonitoringLocationName[1] == "Site 1")
})

testthat::test_that("TADA_FindNearbySites groups nearby sites by distance", {

  fake_nhd <- sf::st_sf(
    NHD.nhdplusid = c("1001", "1001", "1001"),
    NHD.resolution = c("HR", "HR", "HR"),
    NHD.catchmentareasqkm = c(1.1, 1.1, 1.1),
    geometry = sf::st_sfc(
      sf::st_point(c(-90.0000, 40.0000)),
      sf::st_point(c(-90.0001, 40.0001)),
      sf::st_point(c(-90.0002, 40.0002)),
      crs = 4326
    )
  )

  fake_tada <-tibble::tibble(
    TADA.MonitoringLocationIdentifier = c("site_a", "site_b", "site_c"),
    TADA.MonitoringLocationName = c("Site A", "Site B", "Site C"),
    TADA.LongitudeMeasure = c(-90.0000, -90.0001, -90.0002),
    TADA.LatitudeMeasure = c(40.0000, 40.0001, 40.0050),
    HorizontalCoordinateReferenceSystemDatumName = c("WGS84", "WGS84", "WGS84"),
    OrganizationIdentifier = c("org1", "org2", "org1"),
    TADA.MonitoringLocationTypeName = c("STREAM", "STREAM", "STREAM"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    TADA.ResultMeasureValue = c(1, 2, 3)
  )

  testthat::local_mocked_bindings(
    .safe_fetchNHD = function(...) fake_nhd,
    .package = "EPATADA"
  )

  result <- EPATADA::TADA_FindNearbySites(
    fake_tada,
    catchment = FALSE,
    by_org = FALSE,
    dist_buffer = 50
  )

  testthat::expect_true(any(!is.na(result$TADA.NearbySiteGroup)))

  grouped_ids <- result |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::pull(TADA.MonitoringLocationIdentifier)

  testthat::expect_true("[site_a, site_b]" %in% grouped_ids)
  testthat::expect_false("site_c" %in% grouped_ids)
})
