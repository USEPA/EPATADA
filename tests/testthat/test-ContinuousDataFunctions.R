# Tests for TADA_listNWIS
testthat::test_that("TADA_listNWIS returns correct structure when querying by sites", {
  # Test with known site numbers
  site_nums <- c("11530500", "11532500")
  sites_result <- TADA_listNWIS(sites = site_nums)
  
  # Check basic structure and content
  testthat::expect_s3_class(sites_result, "sf")
  testthat::expect_true("site_no" %in% colnames(sites_result))
  testthat::expect_true("parameter" %in% colnames(sites_result))
  testthat::expect_true(all(sites_result$site_no %in% site_nums))
})

testthat::test_that("TADA_listNWIS returns empty sf with correct structure when no data found", {
  # Test with non-existent site
  nonexistent_site <- "99999999"
  result <- TADA_listNWIS(sites = nonexistent_site)
  
  # Check structure of empty return
  testthat::expect_s3_class(result, "sf")
  testthat::expect_equal(nrow(result), 0)
  expected_cols <- c("site_no", "site_name", "site_type", "site_type_cd", 
                     "data_type", "data_type_cd", "parameter", "parameter_code", 
                     "n_obs", "begin_date", "end_date", "geometry")
  testthat::expect_true(all(expected_cols %in% colnames(result)))
})

testthat::test_that("TADA_listNWIS validates input parameters correctly", {
  # Test with multiple query types
  testthat::expect_error(
    TADA_listNWIS(sites = c("11530500"), states = "CA"),
    "Multiple data-querying arguments"
  )
  
  # Test with no query types
  testthat::expect_error(
    TADA_listNWIS(),
    "No data-querying argument"
  )
  
  # Test invalid state code
  testthat::expect_error(
    TADA_listNWIS(states = "ZZ"),
    "Valid state abbreviation not provided"
  )
})

testthat::test_that("TADA_listNWIS errors when aoi_sf is too large", {

  # Test big shapefiles (should error if larger than 118,078 square miles)
  
  # Create an artificially large polygon (covering most of the US)
  large_bbox <- c(
    xmin = -125, # West coast
    ymin = 24,   # Southern border
    xmax = -66,  # East coast
    ymax = 49    # Northern border
  )
  
  # Convert to bbox object, then to sfc
  large_poly <- sf::st_as_sfc(sf::st_bbox(large_bbox, crs = 4269))
  large_sf <- sf::st_sf(geometry = large_poly)
  
  # Test with artificial large AOI
  testthat::expect_error(
    TADA_listNWIS(aoi_sf = large_sf),
    "At least one of your user-supplied features in 'aoi_sf' is too large"
  )
  
  # Create a multi-feature sf object with one small and one large polygon
  small_bbox <- c(
    xmin = -77.1,
    ymin = 38.8,
    xmax = -76.9,
    ymax = 38.9
  )
  
  small_poly <- sf::st_as_sfc(sf::st_bbox(small_bbox, crs = 4269))
  
  combined_sf <- sf::st_sf(
    name = c("small", "large"),
    geometry = c(small_poly, large_poly)
  )
  
  # Test with combined small+large features
  testthat::expect_error(
    TADA_listNWIS(aoi_sf = combined_sf),
    "At least one of your user-supplied features in 'aoi_sf' is too large"
  )
})

# Tests for TADA_getNWIS
testthat::test_that("TADA_getNWIS returns correct structure with site query", {
  # Test with known site that has discharge data
  site_num <- "11530500"
  start_date <- "2020-01-01"
  end_date <- "2020-01-05"
  
  result <- TADA_getNWIS(
    sites = site_num, 
    parameter_codes = "00060", 
    start_date = start_date, 
    end_date = end_date
  )
  
  # Check structure and content
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("NWIS.site_no", "NWIS.date", "NWIS.parameter", "NWIS.value", "NWIS.status") %in% colnames(result)))
  testthat::expect_true(all(result$NWIS.site_no == site_num))
  testthat::expect_true(all(as.Date(result$NWIS.date) >= as.Date(start_date)))
  testthat::expect_true(all(as.Date(result$NWIS.date) <= as.Date(end_date)))
})

testthat::test_that("TADA_getNWIS validates input parameters correctly", {
  # Test with multiple query types
  testthat::expect_error(
    TADA_getNWIS(
      sites = "11530500", 
      states = "CA", 
      parameter_codes = "00060", 
      start_date = "2020-01-01", 
      end_date = "2020-01-05"
    ),
    "Multiple data-querying arguments"
  )
  
  # Test with no query types
  testthat::expect_error(
    TADA_getNWIS(
      parameter_codes = "00060", 
      start_date = "2020-01-01", 
      end_date = "2020-01-05"
    ),
    "No data-querying argument"
  )
  
  # Test with missing required parameters
  testthat::expect_error(
    TADA_getNWIS(
      sites = "11530500",
      start_date = "2020-01-01", 
      end_date = "2020-01-05"
    )
  )
})

