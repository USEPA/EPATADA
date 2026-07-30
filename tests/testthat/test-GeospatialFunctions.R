# Tests for the functions in GeoSpatialFunctions.R

# ==========
# UNIT TESTS
# ==========

# test fixtures
TADA_dataframe <- Data_HUC8_02070004_Mod1Output |>
  dplyr::filter(TADA.CharacteristicName == "PH")
TADA_spatial <- TADA_MakeSpatial(TADA_dataframe)

# pH data in MT county
# Hill_MT_pH <- EPATADA::TADA_DataRetrieval(
#   characteristicName = "pH",
#   statecode = "MT",
#   countycode = "041",
#   applyautoclean = TRUE
# )
# save(Hill_MT_pH, file = testthat::test_path("testdata", "Hill_MT_pH.rda"))
load(testthat::test_path("testdata", "Hill_MT_pH.rda"))

# small area test as subset of large area
small_subset_Hill_MT_pH <- Hill_MT_pH[125:140, ]
expect_cat_n_small <- 2

# data for nearby sites test
nearby_data <- Hill_MT_pH |>
  dplyr::filter(OrganizationIdentifier %in% c("CHIPCREE_WQX", "USGS-MT"))

# query specific to sites along state border
# sites = c("NALMS-F1217605",
#           "EMAP_CS_WQX-RI03-0338-B",
#           "EMAP_CS_WQX-RI05-0016-A",
#           "NARS_WQX-NCCA10-1634",
#           "NARS_WQX-NCA_RI-10129"
#           )
# RI_CT_secchi <- EPATADA::TADA_DataRetrieval(
#  characteristicName = "Depth, Secchi disk depth",
#  statecode = "RI",
#  countycode = "CT",
#  applyautoclean = TRUE
#  )
# RI_CT_secchi
# save(RI_CT_secchi, file = testthat::test_path("testdata", "RI_CT_secchi.rda"))
load(testthat::test_path("testdata", "RI_CT_secchi.rda"))

# test_au_ref_MTDEQ.rda is static, but was generated using:
# MT_AU_MLRef <- TADA_GetATTAINSAUMLCrosswalk(org_id = "MTDEQ")
# test_au_ref_MTDEQ <- TADA_UpdateATTAINSAUMLCrosswalk(org_id = "MTDEQ",
#                                                     crosswalk = MT_AU_MLRef)
# save(test_au_ref_MTDEQ, file = testthat::test_path("testdata", "test_au_ref_MTDEQ.rda"))
load(testthat::test_path("testdata", "test_au_ref_MTDEQ.rda"))

# ----------------------
# TADA_MakeSpatial Tests 
# ----------------------

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

# ----------
# fetch_bbox
# ----------

testthat::test_that("fetch_bbox returns sf features for a known bbox", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("gispub.epa.gov")
  
  baseurl <- "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3"
  
  df <- Data_MT_MissoulaCounty |>
    TADA_MakeSpatial()
  
  out <- fetch_bbox(baseurl, df)
  
  testthat::expect_s3_class(out, "sf")
  testthat::expect_gt(nrow(out), 0)
  testthat::expect_true("geometry" %in% names(out))
})

# --------
# fetch_au
# --------

testthat::test_that("fetch_au returns sf features for known assessment units", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("gispub.epa.gov")
  
  baseurl <- "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3"
  au_ids <- c("IL_N-99", "IL_N-12", "IL_N-16", "IL_N-17")
  
  out <- fetch_au(baseurl, assessment_unit_ids = au_ids)
  
  testthat::expect_s3_class(out, "sf")
  testthat::expect_gt(nrow(out), 0)
  testthat::expect_true("assessmentunitidentifier" %in% names(out))
  testthat::expect_true(all(out$assessmentunitidentifier %in% au_ids))
})

# --------------------
# TADA_FindNearbySites
# --------------------

testthat::test_that("TADA_FindNearbySites errors when required columns are missing", {
  bad_data <- nearby_fixture |> dplyr::select(-TADA.LongitudeMeasure)
  
  testthat::expect_error(
    TADA_FindNearbySites(bad_data),
    "required field\\(s\\).*TADA\\.LongitudeMeasure"
  )
})

testthat::test_that("TADA_FindNearbySites errors when org_hierarchy is not character or NULL", {
  testthat::expect_error(
    TADA_FindNearbySites(nearby_fixture, org_hierarchy = 1),
    "Organization hierarchy must be a character vector or NULL"
  )
})

testthat::test_that("TADA_FindNearbySites errors on invalid meta_select", {
  testthat::expect_error(
    TADA_FindNearbySites(nearby_fixture, meta_select = "badvalue"),
    "'arg' should be one of"
  )
})

testthat::test_that("TADA_FindNearbySites returns no-nearby output when no sites are within buffer", {
  far_apart <- tibble::tibble(
    ResultIdentifier = c("R1", "R2", "R3"),
    TADA.MonitoringLocationIdentifier = c("ML1", "ML2", "ML3"),
    TADA.MonitoringLocationName = c("Site 1", "Site 2", "Site 3"),
    TADA.LongitudeMeasure = c(-100, -110, -120),
    TADA.LatitudeMeasure = c(30, 40, 50),
    HorizontalCoordinateReferenceSystemDatumName = c("WGS84", "WGS84", "WGS84"),
    TADA.MonitoringLocationTypeName = c("STREAM", "STREAM", "LAKE"),
    OrganizationIdentifier = c("ORG1", "ORG2", "ORG3"),
    ActivityStartDate = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    TADA.ResultMeasureValue = c(1.1, 2.2, 3.3)
  )
  
  result <- TADA_FindNearbySites(
    far_apart,
    catchment = FALSE,
    by_AU = FALSE,
    by_org = FALSE,
    dist_buffer = 1
  )
  
  testthat::expect_true("TADA.NearbySiteGroup" %in% names(result))
  testthat::expect_true(all(is.na(result$TADA.NearbySiteGroup)))
  testthat::expect_true(
    all(grepl("No nearby sites detected", result$TADA.NearbySites.Flag))
  )
})

testthat::test_that("TADA_FindNearbySites errors when OrganizationIdentifier is missing and by_org = TRUE", {
  no_org <- nearby_fixture |> dplyr::select(-OrganizationIdentifier)
  
  testthat::expect_error(
    TADA_FindNearbySites(
      no_org,
      catchment = FALSE,
      by_AU = FALSE,
      by_org = TRUE,
      dist_buffer = 100
    ),
    "OrganizationIdentifier"
  )
})

testthat::test_that("TADA_FindNearbySites handles partial org_hierarchy", {
  testthat::expect_message(
    result <- TADA_FindNearbySites(
      nearby_fixture,
      catchment = FALSE,
      by_AU = FALSE,
      org_hierarchy = c("ORG1"),
      meta_select = "newest",
      dist_buffer = 100
    ),
    "missing from org_hierarchy"
  )
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("TADA.NearbySites.Flag" %in% names(result))
  testthat::expect_true("TADA.NearbySiteGroup" %in% names(result))
})

testthat::test_that("TADA_FindNearbySites supports meta_select = newest", {
  new <- TADA_FindNearbySites(
    nearby_fixture,
    catchment = FALSE,
    by_AU = FALSE,
    meta_select = "newest",
    dist_buffer = 100
  )
  
  testthat::expect_true(is.data.frame(new))
  testthat::expect_true("TADA.NearbySites.Flag" %in% names(new))
})

testthat::test_that("TADA_FindNearbySites handles duplicated observations", {
  dup_data <- dplyr::bind_rows(nearby_fixture, nearby_fixture[1, ])
  
  result <- TADA_FindNearbySites(
    dup_data,
    catchment = FALSE,
    by_AU = FALSE,
    by_org = FALSE,
    dist_buffer = 100
  )
  
  testthat::expect_equal(nrow(result), nrow(dup_data))
})

testthat::test_that("TADA_FindNearbySites respects by_AU = FALSE when AU column is present", {
  au_fixture <- nearby_fixture |>
    dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1", "AU2", "AU3"))
  
  result <- TADA_FindNearbySites(
    au_fixture,
    by_AU = FALSE,
    catchment = FALSE,
    dist_buffer = 100
  )
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("TADA.NearbySiteGroup" %in% names(result))
})

testthat::test_that("TADA_FindNearbySites respects by_AU = TRUE when AU column is present", {
  au_fixture <- nearby_fixture |>
    dplyr::mutate(ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1", "AU2", "AU3"))
  
  result <- TADA_FindNearbySites(
    au_fixture,
    by_AU = TRUE,
    catchment = FALSE,
    dist_buffer = 100
  )
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("TADA.NearbySiteGroup" %in% names(result))
})

testthat::test_that("TADA_FindNearbySites mocked Hi catchment filtering works", {
  testthat::skip_if_not_installed("sf")
  
  tiny_data <- nearby_fixture |> dplyr::slice(1:3)
  
  fake_catchments_hi <- sf::st_as_sf(
    tibble::tibble(
      NHD.nhdplusid = c("C1", "C2"),
      NHD.resolution = c("HR", "HR"),
      NHD.catchmentareasqkm = c(1, 1),
      wkt = c(
        "POLYGON((-112.001 45.999, -111.999 45.999, -111.999 46.001, -112.001 46.001, -112.001 45.999))",
        "POLYGON((-113.001 46.999, -112.999 46.999, -112.999 47.001, -113.001 47.001, -113.001 46.999))"
      )
    ),
    wkt = "wkt",
    crs = 4326
  )
  
  result <- testthat::with_mocked_bindings(
    fetchNHD = function(...) fake_catchments_hi,
    {
      TADA_FindNearbySites(
        tiny_data,
        dist_buffer = 20,
        catchment = TRUE,
        by_AU = FALSE,
        by_org = FALSE,
        nhd_res = "Hi"
      )
    }
  )
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("TADA.NearbySites.Flag" %in% names(result))
})

testthat::test_that("TADA_FindNearbySites mocked Med catchment filtering works", {
  testthat::skip_if_not_installed("sf")
  
  tiny_data <- nearby_fixture |> dplyr::slice(1:3)
  
  fake_catchments_med <- sf::st_as_sf(
    tibble::tibble(
      NHD.comid = c("C1", "C2"),
      NHD.resolution = c("nhdplusV2", "nhdplusV2"),
      NHD.catchmentareasqkm = c(1, 1),
      wkt = c(
        "POLYGON((-112.001 45.999, -111.999 45.999, -111.999 46.001, -112.001 46.001, -112.001 45.999))",
        "POLYGON((-113.001 46.999, -112.999 46.999, -112.999 47.001, -113.001 47.001, -113.001 46.999))"
      )
    ),
    wkt = "wkt",
    crs = 4326
  )
  
  result <- testthat::with_mocked_bindings(
    fetchNHD = function(...) fake_catchments_med,
    {
      TADA_FindNearbySites(
        tiny_data,
        dist_buffer = 20,
        catchment = TRUE,
        by_AU = FALSE,
        by_org = FALSE,
        nhd_res = "Med"
      )
    }
  )
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("TADA.NearbySites.Flag" %in% names(result))
})

testthat::test_that("TADA_FindNearbySites mocked catchment filtering can remove all groups", {
  testthat::skip_if_not_installed("sf")
  
  tiny_data <- nearby_fixture |> dplyr::slice(1:2)
  
  fake_catchments <- sf::st_as_sf(
    tibble::tibble(
      NHD.nhdplusid = c("C1", "C2"),
      NHD.resolution = c("HR", "HR"),
      NHD.catchmentareasqkm = c(1, 1),
      wkt = c(
        "POLYGON((-112.001 45.999, -111.999 45.999, -111.999 46.001, -112.001 46.001, -112.001 45.999))",
        "POLYGON((-113.001 46.999, -112.999 46.999, -112.999 47.001, -113.001 47.001, -113.001 46.999))"
      )
    ),
    wkt = "wkt",
    crs = 4326
  )
  
  result <- testthat::with_mocked_bindings(
    fetchNHD = function(...) fake_catchments,
    {
      TADA_FindNearbySites(
        tiny_data,
        dist_buffer = 50,
        catchment = TRUE,
        by_AU = FALSE,
        by_org = FALSE
      )
    }
  )
  
  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("TADA.NearbySites.Flag" %in% names(result))
})

# -------------------------
# TADA_GetUniqueNearbySites
# -------------------------

# Add tests here to cover TADA_GetUniqueNearbySites()

# =================
# INTEGRATION TESTS
# =================

# ------------
# fetchATTAINS
# ------------

testthat::test_that("fetchATTAINS fails with appropriate errors", {
  # Test with NULL data
  testthat::expect_error(
    EPATADA:::fetchATTAINS(.data = NULL),
    "The dataframe does not"
  )
})

testthat::test_that("fetchATTAINS handles small areas", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  # small_subset_Hill_MT_pH is subset of Hill_MT_pH fixture (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_all_features <- EPATADA:::fetchATTAINS(
      .data = small_subset_Hill_MT_pH
    )
  )
  testthat::expect_null(result_all_features$ATTAINS_points)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_lines), 2)
  testthat::expect_null(result_all_features$ATTAINS_polygons)
  testthat::expect_equal(
    nrow(result_all_features$ATTAINS_catchments),
    expect_cat_n_small
  )
})

testthat::test_that("fetchATTAINS handles large areas", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  # Hill_MT_pH from fixtures (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_all_features <- EPATADA:::fetchATTAINS(.data = Hill_MT_pH)
  )
  testthat::expect_null(result_all_features$ATTAINS_points)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_lines), 10)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_polygons), 1)
  testthat::expect_equal(nrow(result_all_features$ATTAINS_catchments), 45)
})

testthat::test_that("fetchATTAINS catchments_only parameter", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  testthat::expect_no_error(
    result_catchments_only <- EPATADA:::fetchATTAINS(
      .data = small_subset_Hill_MT_pH,
      catchments_only = TRUE
    )
  )
  testthat::expect_null(result_catchments_only$ATTAINS_points)
  testthat::expect_null(result_catchments_only$ATTAINS_lines)
  testthat::expect_null(result_catchments_only$ATTAINS_polygons)
  # Compare against catchments_only = FALSE (default)
  testthat::expect_equal(
    nrow(result_catchments_only$ATTAINS_catchments),
    expect_cat_n_small
  )
})

testthat::test_that("fetchATTAINS org_id parameter", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  org <- "RIDEM"
  
  testthat::expect_no_error(
    org_results <- EPATADA:::fetchATTAINS(
      .data = RI_CT_secchi,
      catchments_only = TRUE,
      org_id = org
    )
  )
  
  testthat::expect_true(all(org_results$ATTAINS_catchments$organizationid == org))
})

# --------
# fetchNHD
# --------

testthat::test_that("fetchNHD handles small areas with defaults", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  # small_subset_Hill_MT_pH subset of Hill_MT_pH fixture (testdata/Hill_MT_pH.Rd)
  testthat::expect_no_error(
    result_NHD_small <- EPATADA:::fetchNHD(.data = small_subset_Hill_MT_pH)
  )
  testthat::expect_s3_class(result_NHD_small, "sf")
  testthat::expect_gt(nrow(result_NHD_small), 0)
  testthat::expect_true("NHD.nhdplusid" %in% names(result_NHD_small))
  testthat::expect_true("NHD.resolution" %in% names(result_NHD_small))
  # Expect 2 catchments returned
  testthat::expect_equal(nrow(result_NHD_small), 2)
})

testthat::test_that("fetchNHD with valid non-default features params", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  testthat::expect_no_error(
    flines <- EPATADA:::fetchNHD(
      .data = small_subset_Hill_MT_pH,
      features = "flowlines"
    )
  )
  expect_equal(NROW(flines), 6) # Expected results
  
  testthat::expect_no_error(
    waterbodies <- EPATADA:::fetchNHD(
      .data = small_subset_Hill_MT_pH,
      features = "waterbodies"
    )
  )
  expect_equal(NROW(waterbodies), 0) # Expected results
})

testthat::test_that("fetchNHD with valid non-default resolution param Med", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  testthat::expect_no_error(
    med_cat <- EPATADA:::fetchNHD(
      .data = small_subset_Hill_MT_pH,
      resolution = "Med"
    )
  )
  expect_equal(nrow(med_cat), 2) # Expected results
})

testthat::test_that("fetchNHD error when invalid features param", {
  testthat::expect_error(
    EPATADA:::fetchNHD(.data = small_subset_Hill_MT_pH, features = "Hi"),
    "Please select between 'catchments', 'flowlines', 'waterbodies', or any combination for `feature` argument."
  )
})

testthat::test_that("fetchNHD error when invalid resolution param", {
  testthat::expect_error(
    EPATADA:::fetchNHD(.data = small_subset_Hill_MT_pH, resolution = "Lo"),
    'User-supplied resolution unavailable. Please select between "Med" or "Hi".'
  )
})

# ---------------------
# TADA_GetATTAINSByAUID
# ---------------------

testthat::test_that("Get ATTAINS by Assessment Unit ID", {
  # au_id_list <- test_au_ref_MTDEQ$ATTAINS.AssessmentUnitIdentifier
  
  # When run with defaults (no ExpertQuery fields)
  testthat::skip_on_cran()
  testthat::skip_if_offline("www.waterqualitydata.us")
  
  testthat::expect_no_error(
    actual_default <- TADA_GetATTAINSByAUID(
      Data_MT_MissoulaCounty,
      test_au_ref_MTDEQ
    )
  )
  # Check .data was updated by adding 83 cols (163+83=246)
  expect_equal(ncol(actual_default$TADA_with_ATTAINS), 246)
  # Check results based on number of rows
  expected_rows <- c(0, 5, 1)
  expect_equal(NROW(actual_default$ATTAINS_points), expected_rows[1])
  expect_equal(NROW(actual_default$ATTAINS_lines), expected_rows[2])
  expect_equal(NROW(actual_default$ATTAINS_polygons), expected_rows[3])
  # When default fill_ATTAINS_catch = FALSE, catchments are NULL
  expect_null(actual_default$ATTAINS_catchments)
  
  # Run with catchments
  testthat::expect_no_error(
    actual_catchments <- TADA_GetATTAINSByAUID(
      Data_MT_MissoulaCounty,
      test_au_ref_MTDEQ,
      fill_ATTAINS_catch = TRUE
    )
  )
  # Check results based on number of rows (only catchments change from default)
  expected_rows <- c(13, expected_rows)
  expect_equal(NROW(actual_catchments$ATTAINS_catchments), expected_rows[1])
  expect_equal(NROW(actual_catchments$ATTAINS_points), expected_rows[2])
  expect_equal(NROW(actual_catchments$ATTAINS_lines), expected_rows[3])
  expect_equal(NROW(actual_catchments$ATTAINS_polygons), expected_rows[4])
})

# -------------------------------
# TADA_CreateATTAINSAUMLCrosswalk
# -------------------------------

testthat::test_that("TADA_CreateATTAINSAUMLCrosswalk rejects pre-joined ATTAINS data", {
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
    TADA.MonitoringLocationIdentifier = character(0),
    TADA.MonitoringLocationName = character(0),
    TADA.LongitudeMeasure = numeric(0),
    TADA.LatitudeMeasure = numeric(0),
    HorizontalCoordinateReferenceSystemDatumName = character(0),
    OrganizationIdentifier = character(0),
    ActivityStartDate = as.Date(character(0)),
    TADA.ResultMeasureValue = numeric(0)
  )
  
  result <- TADA_CreateATTAINSAUMLCrosswalk(.data = empty_df, return_sf = FALSE)
  testthat::expect_true(NROW(result) == 0)
  testthat::expect_true("ResultIdentifier" %in% names(result))
  testthat::expect_true(any(grepl("^ATTAINS\\.", names(result))))
})

testthat::test_that("TADA_CreateATTAINSAUMLCrosswalk contains expected AU Ref Source values", {
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

# ----------------
# TADA_ViewATTAINS
# ----------------

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
