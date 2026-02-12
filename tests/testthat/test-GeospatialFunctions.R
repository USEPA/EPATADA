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
  expect_equal(nrow(result_all_features$ATTAINS_points), 0)
  expect_equal(nrow(result_all_features$ATTAINS_lines), 2)
  expect_equal(nrow(result_all_features$ATTAINS_polygons), 0)
  expect_equal(nrow(result_all_features$ATTAINS_catchments), expect_cat_n_small)
})

testthat::test_that("fetchATTAINS handles large areas", {
  # large_bbox_data from fixtures (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_all_features <- EPATADA:::fetchATTAINS(.data = large_bbox_data)
  )
  expect_equal(nrow(result_all_features$ATTAINS_points), 0)
  expect_equal(nrow(result_all_features$ATTAINS_lines), 10)
  expect_equal(nrow(result_all_features$ATTAINS_polygons), 1)
  expect_equal(nrow(result_all_features$ATTAINS_catchments), 46)
})

testthat::test_that("fetchATTAINS catchments_only parameter", {
  testthat::expect_no_error(
    result_catchments_only <- EPATADA:::fetchATTAINS(
      .data = small_bbox_data,
      catchments_only = TRUE
    )
  )
  expect_null(nrow(result_catchments_only$ATTAINS_points))
  expect_null(nrow(result_catchments_only$ATTAINS_lines))
  expect_null(nrow(result_catchments_only$ATTAINS_polygons))
  # Compare against catchments_only = FALSE (default)
  expect_equal(
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
  expect_equal(nrow(org_results$ATTAINS_catchments), nrow(all_orgs_filtered))
})


testthat::test_that("fetchNHD handles small areas with defaults", {
  # small_bbox_data subset of large_bbox_data fixture (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_NHD_small <- EPATADA:::fetchNHD(.data = small_bbox_data)
  )
  # Expect 16 catchments returned
  expect_equal(nrow(small_bbox_data), 16)
})

testthat::test_that("fetchNHD with valid non-default features params", {
  testthat::expect_no_error(
    flines <- EPATADA:::fetchNHD(
      .data = small_bbox_data,
      features = "flowlines"
    )
  )
  expect_equal(nrow(flines), 6) # Expected results
  testthat::expect_no_error(
    waterbodies <- EPATADA:::fetchNHD(
      .data = small_bbox_data,
      features = "waterbodies"
    )
  )
  expect_equal(nrow(waterbodies), 0) # Expected results
})

testthat::test_that("fetchNHD with valid non-default resolution param Med", {
  testthat::expect_no_error(
    med_cat <- EPATADA:::fetchNHD(.data = small_bbox_data, resolution = "Med")
  )
  expect_equal(nrow(med_cat), 2) # Expected results
})

testthat::test_that("fetchNHD error when invalid features param", {
  testthat::expect_error(
    EPATADA:::fetchNHD(.data = small_bbox_data, features = "Hi"),
    "Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument."
  )
})

testthat::test_that("fetchNHD error when invalid resolution param", {
  testthat::expect_error(
    EPATADA:::fetchNHD(.data = small_bbox_data, resolution = "Lo"),
    'User-supplied resolution unavailable. Please select between "Med" or "Hi".'
  )
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

testthat::test_that("TADA_FindNearbySites returns expected number of site groups", {
  # find nearby sites tests

  # with defaults
  test_defaults <- TADA_FindNearbySites(large_bbox_data)

  n_defaults <- test_defaults |>
    dplyr::select(TADA.NearbySiteGroup) |>
    dplyr::n_distinct()

  testthat::expect_equal(n_defaults, 45)

  # at 50 m with catchment
  test_fifty <- TADA_FindNearbySites(large_bbox_data, dist_buffer = 50)

  n_fifty <- test_fifty |>
    dplyr::select(TADA.NearbySiteGroup) |>
    dplyr::n_distinct()

  testthat::expect_equal(n_fifty, 48)

  # without catchment
  test_bufferonly <- TADA_FindNearbySites(
    large_bbox_data,
    catchment = FALSE,
    dist_buffer = 100
  )

  n_bufferonly <- test_bufferonly |>
    dplyr::select(TADA.NearbySiteGroup) |>
    dplyr::n_distinct()

  testthat::expect_equal(n_bufferonly, 46)

  # with AU
  # the expected value here may need to be updated if geospatial data for Data_MT_AUMLRef change
  test_au <- Data_MT_AUMLRef$TADA_with_ATTAINS |>
    TADA_FindNearbySites(by_AU = TRUE)

  n_au <- test_au |> dplyr::select(TADA.NearbySiteGroup) |> dplyr::n_distinct()

  testthat::expect_equal(n_au, 38)
})

testthat::test_that("TADA_FindNearbySites returns expected metadata", {
  # select by count
  test_count <- TADA_FindNearbySites(
    large_bbox_data,
    org_hierarchy = "none",
    meta_select = "count"
  )

  test_count_filt <- test_count |>
    dplyr::filter(ResultIdentifier == "NWIS-33738169")

  testthat::expect_equal(
    test_count_filt$TADA.MonitoringLocationIdentifier,
    "[USGS-06138570, CHIPCREE-LBS4, CHIPCREE_WQX-LBS4]"
  )

  testthat::expect_equal(test_count_filt$TADA.LatitudeMeasure, 48.4091576)

  testthat::expect_equal(
    test_count_filt$TADA.MonitoringLocationTypeName,
    "STREAM"
  )

  testthat::expect_equal(
    test_count_filt$TADA.NearbySites.Flag,
    "This monitoring location was grouped with other nearby site(s). Metadata were selected from MonitoringLocation with the most results available across all characteristics."
  )

  # select by org hierarchy
  test_org <- TADA_FindNearbySites(
    large_bbox_data,
    org_hierarchy = c(
      "CHIPCREE",
      "CHIPCREE_WQX",
      "USGS-MT",
      "MDEQ_WQ_WQX",
      "MONT_DEQ_WQX",
      "NARS",
      "NARS_WQX"
    )
  )

  test_org_filt <- test_org |>
    dplyr::filter(ResultIdentifier == "NWIS-33738169")

  testthat::expect_equal(
    test_org_filt$TADA.MonitoringLocationIdentifier,
    "[USGS-06138570, CHIPCREE-LBS4, CHIPCREE_WQX-LBS4]"
  )

  testthat::expect_equal(test_org_filt$TADA.LatitudeMeasure, 48.40935910)

  testthat::expect_equal(
    test_org_filt$TADA.MonitoringLocationTypeName,
    "RIVER/STREAM"
  )

  testthat::expect_equal(
    test_org_filt$TADA.NearbySites.Flag,
    "This monitoring location was grouped with other nearby site(s). Metadata were selected randomly."
  )
})
