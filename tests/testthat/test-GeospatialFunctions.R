# Testing the Geospatial Functions ----
# testing `GeoSpatialFunctions.R` with the same data that was used in the
# `TADAModule2.Rmd` Vignette in chunk 6 (3/18/24).
#
# TADA_dataframe <- TADA_DataRetrieval(
#   startDate = "2020-01-01",
#   endDate = "2020-12-31",
#   characteristicName = "pH",
#   countycode = "US:08:069",
#   applyautoclean = TRUE
# )


# Read in sample data for tests
TADA_dataframe <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_dataframe.rds"))
TADA_spatial <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_spatial.rds"))
TADA_with_ATTAINS <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_with_ATTAINS.rds"))
TADA_with_ATTAINS_list <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_with_ATTAINS_list.rds"))

# TADA_MakeSpatial ----
testthat::test_that(
  desc = "`TADA_MakeSpatial()` converts the TADA_dataframe into an sf object",
  code = {
    test_df <- TADA_MakeSpatial(.data = TADA_dataframe)
    expect_true(inherits(test_df, "sf"))
  }
)

testthat::test_that(
  desc = "The CRS in the test dataframe is the same as the input",
  code = {
    test_df1 <- TADA_MakeSpatial(.data = TADA_dataframe, crs = 4326)
    test_df2 <- TADA_MakeSpatial(.data = TADA_dataframe, crs = 4269)

    expect_true(gsub("\\D", "", sf::st_crs(test_df1)$epsg) == as.character(4326))
    expect_true(gsub("\\D", "", sf::st_crs(test_df2)$epsg) == as.character(4269))
  }
)

testthat::test_that(
  desc = "The geometry types in the dataframe are points",
  code = {
    test_df <- TADA_MakeSpatial(.data = TADA_dataframe)
    geometry_types <- sf::st_geometry_type(test_df)
    valid_geometry_types <- "POINT"
    expect_true(all(geometry_types %in% valid_geometry_types))
  }
)

testthat::test_that(
  desc = "The number of rows are the same between TADA_dataframe and the output dataframe of TADA_MakeSpatial",
  code = {
    test_df <- TADA_MakeSpatial(.data = TADA_dataframe)
    expect_true(nrow(TADA_dataframe) == nrow(test_df))
  }
)

testthat::test_that(
  desc = "The columns between TADA_dataframe and the output dataframe of TADA_MakeSpatial are identical (sans 'geometry')",
  code = {
    test_df <- TADA_MakeSpatial(.data = TADA_dataframe)
    expect_true(all(names(TADA_dataframe) == names(test_df)[names(test_df) != "geometry"]))
  }
)

testthat::test_that(
  desc = "The structures of the TADA_dataframe and the output dataframe of TADA_MakeSpatial are identical (sans 'geometry')",
  code = {
    test_df <- TADA_MakeSpatial(.data = TADA_dataframe)
    expect_true(identical(str(TADA_dataframe), str(test_df[names(test_df) != "geometry"])))
  }
)

testthat::test_that(
  desc = "TADA_MakeSpatial() errors if the input dataframe does not contain WQP-style latitude and longitude data",
  code = {
    expect_error(TADA_MakeSpatial(.data = tibble::tibble(sample = NA, .rows = 0)))
  }
)

testthat::test_that(
  desc = "TADA_MakeSpatial() errors if the input dataframe is already a spatial object",
  code = {
    expect_error(TADA_MakeSpatial(TADA_spatial))
  }
)

testthat::test_that(
  desc = "The order of the rows are the same (sans 'geometry')",
  code = {
    test_df <- TADA_MakeSpatial(.data = TADA_dataframe)
    expect_true(identical(TADA_dataframe, sf::st_drop_geometry(test_df)))
  }
)

# fetchATTAINS ----
testthat::test_that(
  desc = "fetchATTAINS handles valid input data",
  code = {
    valid_data <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))), crs = 4326)
    result <- EPATADA:::fetchATTAINS(.data = valid_data)
    expect_false(is.null(result))
  }
)

testthat::test_that(
  desc = "fetchATTAINS handles missing input data",
  code = {
    expect_error(fetchATTAINS(.data = NULL))
  }
)

# fetchNHD ----
testthat::test_that(
  desc = "fetchNHD handles valid input data",
  code = {
    valid_data <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))), crs = 4326)
    result <- fetchNHD(.data = valid_data)
    expect_false(is.null(result))
  }
)

testthat::test_that(
  desc = "fetchATTAINS handles missing input data",
  code = {
    expect_error(fetchNHD(.data = NULL))
  }
)

# TADA_GetATTAINS ----
testthat::test_that(
  desc = "TADA_GetATTAINS addresses improper resolution",
  code = {
    expect_error(TADA_GetATTAINS(.data = TADA_dataframe, fill_catchments = TRUE, resolution = "NoNsEnSe", return_sf = FALSE))
  }
)

testthat::test_that(
  desc = "TADA_GetATTAINS can take in the TADA_dataframe and the sf object as inputs",
  code = {
    expect_no_error(TADA_GetATTAINS(.data = TADA_dataframe))
    expect_no_error(TADA_GetATTAINS(.data = TADA_spatial))

    expect_no_warning(TADA_GetATTAINS(.data = TADA_dataframe))
    expect_no_warning(TADA_GetATTAINS(.data = TADA_spatial))
  }
)

testthat::test_that(
  desc = "TADA_GetATTAINS does not run if the .data argument input has already been joined with ATTAINS data.",
  code = {
    expect_error(
      TADA_GetATTAINS(TADA_with_ATTAINS),
      "Your data has already been joined with ATTAINS data."
    )
  }
)

testthat::test_that(
  desc = "TADA_GetATTAINS(return = TRUE)[[1]] == TADA_GetATTAINS(return_sf = FALSE)",
  code = {
    expect_true(identical(TADA_with_ATTAINS_list[[1]], TADA_with_ATTAINS))
  }
)

testthat::test_that(
  desc = "An empty df gets returned if there are no observations in the input df, ",
  code = {
    test <- TADA_GetATTAINS(.data = tibble::tibble(sample = NA, .rows = 0), return_sf = FALSE)
    expect_true(nrow(test) == 0)
  }
)

# TADA_ViewATTAINS ----
testthat::test_that(
  desc = "An input that does not include all objects from `TADA_GetATTAINS(return_sf = TRUE)` gets rejected",
  code = {
    expect_error(TADA_ViewATTAINS(ATTAINS_list = TADA_with_ATTAINS_list[[1]]))
  }
)

testthat::test_that(
  desc = "An input with no observations gets rejected",
  code = {
    expect_error(TADA_ViewATTAINS(ATTAINS_list = tibble::tibble(sample = NA, .rows = 0)))
  }
)

# Remove everything from the environment
rm(list = c("TADA_dataframe", "TADA_spatial", "TADA_with_ATTAINS", "TADA_with_ATTAINS_list"))
