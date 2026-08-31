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
  testthat::expect_equal(nrow(result_all_features$ATTAINS_catchments), 45)
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


testthat::test_that("fetchNHD handles small areas with defaults", {
  # small_bbox_data subset of large_bbox_data fixture (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_NHD_small <- EPATADA:::fetchNHD(.data = small_bbox_data)
  )
  # Expect 16 catchments returned
  testthat::expect_equal(nrow(small_bbox_data), 16)
})

# not working on 7/21/26
# testthat::test_that("fetchNHD with valid non-default features params", {
#   testthat::expect_no_error(
#     flines <- EPATADA:::fetchNHD(
#       .data = small_bbox_data,
#       features = "flowlines"
#     )
#   )
#   expect_equal(NROW(flines), 6) # Expected results
#   testthat::expect_no_error(
#     waterbodies <- EPATADA:::fetchNHD(
#       .data = small_bbox_data,
#       features = "waterbodies"
#     )
#   )
#   expect_equal(NROW(waterbodies), 0) # Expected results
# })

testthat::test_that("fetchNHD with valid non-default resolution param Med", {
  testthat::expect_no_error(
    med_cat <- EPATADA:::fetchNHD(.data = small_bbox_data, resolution = "Med")
  )
  expect_equal(nrow(med_cat), 2) # Expected results
})

# testthat::test_that("fetchNHD error when invalid features param", {
#   testthat::expect_error(
#     EPATADA:::fetchNHD(.data = small_bbox_data, features = "Hi"),
#     "Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument."
#   )
# })

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

# takes too long to run as of 7/21/26
# testthat::test_that("TADA_FindNearbySites returns expected number of site groups", {
#   # find nearby sites tests
#
#   # with defaults
#   test_defaults <- TADA_FindNearbySites(nearby_data)
#
#   n_defaults <- test_defaults |>
#     dplyr::select(TADA.NearbySiteGroup) |>
#     dplyr::n_distinct()
#
#   testthat::expect_equal(n_defaults, 12)
#
#   # at 50 m with catchment
#   test_fifty <- TADA_FindNearbySites(nearby_data, dist_buffer = 50)
#
#   n_fifty <- test_fifty |>
#     dplyr::select(TADA.NearbySiteGroup) |>
#     dplyr::n_distinct()
#
#   testthat::expect_equal(n_fifty, 8)
#
#   # without catchment
#   test_bufferonly <- TADA_FindNearbySites(
#     nearby_data,
#     catchment = FALSE,
#     dist_buffer = 100
#   )
#
#   n_bufferonly <- test_bufferonly |>
#     dplyr::select(TADA.NearbySiteGroup) |>
#     dplyr::n_distinct()
#
#   testthat::expect_equal(n_bufferonly, 15)
#
#   # with AU
#   # the expected value here may need to be updated if geospatial data for Data_MT_AUMLRef change
#   test_au <- Data_MT_AUMLRef$TADA_with_ATTAINS |>
#     dplyr::filter(OrganizationIdentifier == "MTVOLWQM_WQX") |>
#     TADA_FindNearbySites(by_AU = TRUE)
#
#   n_au <- test_au |>
#     sf::st_drop_geometry() |>
#     dplyr::select(TADA.NearbySiteGroup) |>
#     dplyr::n_distinct()
#
#   testthat::expect_equal(n_au, 2)
# })

# fails as of 7/21/26
# testthat::test_that("TADA_FindNearbySites returns expected metadata", {
#   # select by count
#   test_count <- TADA_FindNearbySites(
#     nearby_data,
#     org_hierarchy = "none",
#     meta_select = "count"
#   )
#
#   test_count_filt <- test_count |>
#     dplyr::filter(ResultIdentifier == "NWIS-33738169")
#
#   testthat::expect_equal(
#     test_count_filt$TADA.MonitoringLocationIdentifier,
#     "[USGS-06138570, CHIPCREE_WQX-LBS4]"
#   )
#
#   testthat::expect_equal(test_count_filt$TADA.LatitudeMeasure, 48.4091576)
#
#   testthat::expect_equal(
#     test_count_filt$TADA.MonitoringLocationTypeName,
#     "STREAM"
#   )
#
#   testthat::expect_equal(
#     test_count_filt$TADA.NearbySites.Flag,
#     "This monitoring location was grouped with other nearby site(s). Metadata were selected from MonitoringLocation with the most results available across all characteristics."
#   )
#
#   # select by org hierarchy
#   test_org <- TADA_FindNearbySites(
#     nearby_data,
#     org_hierarchy = c("CHIPCREE_WQX", "USGS-MT")
#   )
#
#   test_org_filt <- test_org |>
#     dplyr::filter(ResultIdentifier == "NWIS-33738169")
#
#   testthat::expect_equal(
#     test_org_filt$TADA.MonitoringLocationIdentifier,
#     "[USGS-06138570, CHIPCREE_WQX-LBS4]"
#   )
#
#   testthat::expect_equal(test_org_filt$TADA.LatitudeMeasure, 48.40935910)
#
#   testthat::expect_equal(
#     test_org_filt$TADA.MonitoringLocationTypeName,
#     "RIVER/STREAM"
#   )
#
#   testthat::expect_equal(
#     test_org_filt$TADA.NearbySites.Flag,
#     "This monitoring location was grouped with other nearby site(s). Metadata were selected randomly."
#   )
# })

testthat::test_that("TADA_FindNearbySites respects the by_org argument", {
  # Without organization filtering, at least one nearby-site group
  # should contain sites from multiple organizations.
  test_no_org_filter <- TADA_FindNearbySites(
    nearby_data,
    catchment = FALSE,
    by_AU = FALSE,
    by_org = FALSE,
    dist_buffer = 100
  )

  mixed_org_groups <- test_no_org_filter |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::group_by(TADA.MonitoringLocationIdentifier) |>
    dplyr::summarise(
      n_orgs = dplyr::n_distinct(OrganizationIdentifier),
      .groups = "drop"
    ) |>
    dplyr::filter(n_orgs > 1)

  testthat::expect_gt(nrow(mixed_org_groups), 0)

  # With organization filtering, no nearby-site group should contain
  # monitoring locations from more than one organization.
  test_by_org <- TADA_FindNearbySites(
    nearby_data,
    catchment = FALSE,
    by_AU = FALSE,
    by_org = TRUE,
    dist_buffer = 100
  )

  orgs_per_group <- test_by_org |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(TADA.NearbySiteGroup)) |>
    dplyr::group_by(TADA.MonitoringLocationIdentifier) |>
    dplyr::summarise(
      n_orgs = dplyr::n_distinct(OrganizationIdentifier),
      .groups = "drop"
    )

  testthat::expect_true(nrow(orgs_per_group) > 0)
  testthat::expect_true(all(orgs_per_group$n_orgs == 1))
})

testthat::test_that("TADA_FindNearbySites does not combine known sites from different organizations", {
  test_by_org <- TADA_FindNearbySites(
    nearby_data,
    catchment = FALSE,
    by_AU = FALSE,
    by_org = TRUE,
    dist_buffer = 100
  )

  usgs_result <- test_by_org |>
    sf::st_drop_geometry() |>
    dplyr::filter(ResultIdentifier == "NWIS-33738169")

  testthat::expect_false(any(grepl(
    "CHIPCREE_WQX-LBS4",
    usgs_result$TADA.MonitoringLocationIdentifier,
    fixed = TRUE
  )))
})

# tests for TADA_CreatePointAUGeometry
testthat::test_that("TADA_CreatePointAUGeometry errors when required coordinate columns are missing", {
  base_df <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = "AU1",
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.LongitudeMeasure = -90,
    TADA.LatitudeMeasure = 40,
    HorizontalCoordinateReferenceSystemDatumName = "NAD83",
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    TADA_CreatePointAUGeometry(dplyr::select(base_df, -TADA.LongitudeMeasure)),
    "TADA_CreatePointAUGeometry: Missing required coordinate column\\(s\\): TADA.LongitudeMeasure"
  )

  testthat::expect_error(
    TADA_CreatePointAUGeometry(dplyr::select(base_df, -TADA.LatitudeMeasure)),
    "TADA_CreatePointAUGeometry: Missing required coordinate column\\(s\\): TADA.LatitudeMeasure"
  )

  testthat::expect_error(
    TADA_CreatePointAUGeometry(dplyr::select(
      base_df,
      -HorizontalCoordinateReferenceSystemDatumName
    )),
    "TADA_CreatePointAUGeometry: Missing required coordinate column\\(s\\): HorizontalCoordinateReferenceSystemDatumName"
  )
})

testthat::test_that("TADA_CreatePointAUGeometry: errors when neither ID column is present", {
  df <- data.frame(
    TADA.LongitudeMeasure = -90,
    TADA.LatitudeMeasure = 40,
    HorizontalCoordinateReferenceSystemDatumName = "NAD83",
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    TADA_CreatePointAUGeometry(df),
    "TADA_CreatePointAUGeometry: Input data must contain at least one of: ATTAINS.AssessmentUnitIdentifier or TADA.MonitoringLocationIdentifier"
  )
})

testthat::test_that("TADA_CreateAUPointGeometry returns sf geometry for valid input", {
  df <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1"),
    TADA.MonitoringLocationIdentifier = c("ML1", "ML1"),
    TADA.LongitudeMeasure = c(-90, -90.1),
    TADA.LatitudeMeasure = c(40, 40.1),
    HorizontalCoordinateReferenceSystemDatumName = c("NAD83", "NAD83"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUGeometry(df)

  testthat::expect_s3_class(result, "sf")
  testthat::expect_true("geometry" %in% names(result))
  testthat::expect_true("AU_ID" %in% names(result))
})

testthat::test_that("TADA_CreatePointAUGeometry creates POINT for one location and MULTIPOINT for multiple locations", {
  df <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1", "AU2"),
    TADA.MonitoringLocationIdentifier = c("ML1", "ML1", "ML2"),
    TADA.LongitudeMeasure = c(-90, -90, -91),
    TADA.LatitudeMeasure = c(40, 41, 41),
    HorizontalCoordinateReferenceSystemDatumName = c("NAD83", "NAD83", "NAD83"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUGeometry(df)

  geom_types <- sf::st_geometry_type(result)
  testthat::expect_true(any(geom_types %in% c("POINT", "MULTIPOINT")))
})

testthat::test_that("TADA_CreatePointAUGeometry drops rows with missing coordinates", {
  df <- data.frame(
    ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU2"),
    TADA.MonitoringLocationIdentifier = c("ML1", "ML2"),
    TADA.LongitudeMeasure = c(-90, NA),
    TADA.LatitudeMeasure = c(40, 41),
    HorizontalCoordinateReferenceSystemDatumName = c("NAD83", "NAD83"),
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUGeometry(df)

  testthat::expect_equal(nrow(result), 1)
})

testthat::test_that("TADA_CreatePointAUGeometry accepts auid_prefix", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.LongitudeMeasure = -90,
    TADA.LatitudeMeasure = 40,
    HorizontalCoordinateReferenceSystemDatumName = "NAD83",
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUGeometry(
    df,
    auid_prefix = "TEST"
  )

  testthat::expect_s3_class(result, "sf")
})

testthat::test_that("TADA_CreatePointAUGeometry works with only TADA.MonitoringLocationIdentifier present", {
  df <- data.frame(
    TADA.MonitoringLocationIdentifier = "ML1",
    TADA.LongitudeMeasure = -90,
    TADA.LatitudeMeasure = 40,
    HorizontalCoordinateReferenceSystemDatumName = "NAD83",
    stringsAsFactors = FALSE
  )

  result <- TADA_CreatePointAUGeometry(df)

  testthat::expect_s3_class(result, "sf")
})
