# Helper: minimal synthetic dataset with required columns + ResultIdentifier
make_df <- function() {
  data.frame(
    ResultIdentifier = paste0("R", seq_len(7)),
    ActivityMediaSubdivisionName = c(
      "Surface Water", # 1 -> SURFACE WATER
      "Groundwater", # 2 -> GROUNDWATER
      "Sediment", # 3 -> SEDIMENT
      NA, # 4 -> ActivityMediaName AIR -> OTHER
      NA, # 5 -> BIOLOGICAL -> OTHER
      NA, # 6 -> "" -> OTHER
      NA # 7 -> Tissue -> OTHER
    ),
    AquiferName = c(NA, "Aquifer", NA, NA, NA, NA, NA),
    MonitoringLocationTypeName = c(
      "River/Stream",
      "Well",
      "Wetland",
      "Station",
      "Site",
      "Site",
      "Site"
    ),
    ActivityMediaName = c(NA, NA, NA, "AIR", "BIOLOGICAL", "", "Tissue"),
    stringsAsFactors = FALSE
  )
}

test_that("TADA_MediaFilter errors on non-data frame", {
  expect_error(
    TADA_MediaFilter(list(a = 1)),
    "Input object must be a data frame"
  )
})

test_that("TADA_MediaFilter returns NULL with message on empty data frame", {
  empty_df <- data.frame(
    ResultIdentifier = character(),
    ActivityMediaSubdivisionName = character(),
    AquiferName = character(),
    MonitoringLocationTypeName = character(),
    stringsAsFactors = FALSE
  )
  expect_message(res <- TADA_MediaFilter(empty_df), "empty")
  expect_null(res)
})

test_that("TADA_MediaFilter errors when required columns are missing", {
  df_missing <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = "Surface Water",
    MonitoringLocationTypeName = "River/Stream",
    stringsAsFactors = FALSE
  )
  expect_error(
    TADA_MediaFilter(df_missing),
    "Missing required columns: AquiferName"
  )
})

test_that("clean = FALSE: adds TADA.Media.Flag and normalizes to core values", {
  df <- make_df()
  res <- TADA_MediaFilter(df, clean = FALSE)

  # Flag column present
  expect_true("TADA.Media.Flag" %in% names(res))
  # Helper column removed
  expect_false("gw_has_fields" %in% names(res))

  # Flags normalized to core set and OTHER for non-core
  flags <- sort(unique(res$TADA.Media.Flag))
  expect_setequal(flags, c("SURFACE WATER", "GROUNDWATER", "SEDIMENT", "OTHER"))

  # Toggles ignored when clean = FALSE
  res2 <- TADA_MediaFilter(
    df,
    clean = FALSE,
    ground_water = TRUE,
    sediment = TRUE,
    other = TRUE
  )
  expect_equal(nrow(res2), nrow(df))
})

test_that("clean = TRUE: removes selected media and drops flag column", {
  df <- make_df()

  # Determine expected rows remaining after removing SEDIMENT and OTHER
  flagged <- TADA_MediaFilter(df, clean = FALSE)
  flags <- flagged$TADA.Media.Flag
  expected_n <- sum(flags %in% c("SURFACE WATER", "GROUNDWATER"))

  # Remove sediment + other
  expect_message(
    res_clean <- TADA_MediaFilter(
      df,
      clean = TRUE,
      sediment = TRUE,
      other = TRUE
    ),
    regexp = "(Removed media types|set to TRUE).*SEDIMENT.*OTHER",
    all = FALSE
  )
  expect_equal(nrow(res_clean), expected_n)

  # Flag column removed in clean mode
  expect_false("TADA.Media.Flag" %in% names(res_clean))
  # Helper column removed
  expect_false("gw_has_fields" %in% names(res_clean))
})

test_that("clean = TRUE: warns when all media toggles are TRUE", {
  df <- make_df()
  # Warning that all media types are selected for removal
  expect_warning(
    TADA_MediaFilter(
      df,
      clean = TRUE,
      surface_water = TRUE,
      ground_water = TRUE,
      sediment = TRUE,
      other = TRUE
    ),
    regexp = "All media types are selected for removal"
  )
})

test_that("clean = TRUE: warns when filter removes all rows", {
  # Construct data that will all be removed by toggles
  df <- data.frame(
    ResultIdentifier = c("R1", "R2"),
    ActivityMediaSubdivisionName = c("Surface Water", "Groundwater"),
    AquiferName = c(NA, "Aquifer"),
    MonitoringLocationTypeName = c("River/Stream", "Well"),
    ActivityMediaName = c(NA, NA),
    stringsAsFactors = FALSE
  )
  expect_warning(
    TADA_MediaFilter(
      df,
      clean = TRUE,
      surface_water = TRUE,
      ground_water = TRUE,
      sediment = TRUE,
      other = TRUE
    ),
    regexp = "All rows were removed by the media filter"
  )
})

test_that("clean = TRUE: message when no toggles are set", {
  df <- make_df()
  expect_message(
    res_clean <- TADA_MediaFilter(df, clean = TRUE),
    regexp = "No media types selected for removal",
    all = FALSE
  )
  # No filtering performed, but flag column still removed in clean mode
  expect_equal(nrow(res_clean), nrow(df))
  expect_false("TADA.Media.Flag" %in% names(res_clean))
})

test_that("Normalization to OTHER for HABITAT, empty string, AIR, BIOLOGICAL, and non-core values", {
  df <- data.frame(
    ResultIdentifier = paste0("R", 1:5),
    ActivityMediaSubdivisionName = c(NA, NA, NA, NA, NA),
    AquiferName = c(NA, NA, NA, NA, NA),
    MonitoringLocationTypeName = c("Site", "Site", "Site", "Site", "Site"),
    ActivityMediaName = c("HABITAT", "", "AIR", "BIOLOGICAL", "Tissue"),
    stringsAsFactors = FALSE
  )

  res <- TADA_MediaFilter(df, clean = FALSE)
  expect_true(all(res$TADA.Media.Flag == "OTHER"))
})

test_that("Reference join coalesces media when available; otherwise falls back gracefully", {
  # This test assumes the package extdata contains WQXMonitoringLocationTypeNameRef.csv
  df <- data.frame(
    ResultIdentifier = paste0("R", 1:2),
    ActivityMediaSubdivisionName = c(NA, NA),
    AquiferName = c(NA, NA),
    # Use types likely present in the reference; if not, function still returns core flags
    MonitoringLocationTypeName = c("River/Stream", "Well"),
    ActivityMediaName = c(NA, NA),
    stringsAsFactors = FALSE
  )
  res <- TADA_MediaFilter(df, clean = FALSE)
  expect_true("TADA.Media.Flag" %in% names(res))
  expect_true(all(
    res$TADA.Media.Flag %in%
      c("SURFACE WATER", "GROUNDWATER", "SEDIMENT", "OTHER")
  ))
})

test_that("Clean-mode message lists exactly the removed media types", {
  df <- make_df()
  expect_message(
    TADA_MediaFilter(df, clean = TRUE, ground_water = TRUE),
    regexp = "Removed media types: GROUNDWATER"
  )
  expect_message(
    TADA_MediaFilter(df, clean = TRUE, surface_water = TRUE, other = TRUE),
    regexp = "Removed media types: SURFACE WATER, OTHER"
  )
})
