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
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  res <- TADA_MediaFilter(df, clean = FALSE)

  # Flag column present
  expect_true("TADA.Media.Flag" %in% names(res))
  # Helper column removed
  expect_false("gw_has_fields" %in% names(res))

  # Flags normalized to core set and OTHER for non-core
  flags <- sort(unique(res$TADA.Media.Flag))
  allowed <- c("SURFACE WATER", "GROUNDWATER", "SEDIMENT", "OTHER")
  expect_true(all(unique(flags) %in% allowed))

  # Toggles ignored when clean = FALSE
  expect_message(
    TADA_MediaFilter(df, clean = FALSE, ground_water = TRUE),
    regexp = "media toggles ignored because clean = FALSE"
  )

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
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))

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
    regexp = "(Removed media types|set to TRUE).*SEDIMENT.*OTHER"
  )
  expect_equal(nrow(res_clean), expected_n)

  # Flag column removed in clean mode
  expect_false("TADA.Media.Flag" %in% names(res_clean))
  # Helper column removed
  expect_false("gw_has_fields" %in% names(res_clean))
})

test_that("clean = TRUE: warns when all media toggles are TRUE", {
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
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
  # Construct data that will all be removed by toggles (but avoids 'all toggles TRUE' warning)
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
      surface_water = TRUE, # removes Surface Water
      ground_water = TRUE # removes Groundwater
    ),
    regexp = "All rows were removed by the media filter"
  )
})

test_that("clean = TRUE: message when no toggles are set", {
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  expect_message(
    res_clean <- TADA_MediaFilter(df, clean = TRUE),
    regexp = "No media types selected for removal"
  )
  # No filtering performed, but flag column still removed in clean mode
  expect_equal(nrow(res_clean), nrow(df))
  expect_false("TADA.Media.Flag" %in% names(res_clean))
})

test_that("clean = TRUE: emits 'Removed media types: none' when no toggles set", {
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  expect_message(
    TADA_MediaFilter(df, clean = TRUE),
    regexp = "Removed media types: none"
  )
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

test_that("Reference join helper column is removed from output", {
  df <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = NA_character_,
    AquiferName = NA_character_,
    MonitoringLocationTypeName = "River/Stream",
    stringsAsFactors = FALSE
  )
  res <- TADA_MediaFilter(df, clean = FALSE)
  expect_false("Ref.TADA.Media.Flag" %in% names(res))
})

test_that("Clean-mode message lists exactly the removed media types", {
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))

  expect_message(
    suppressWarnings(TADA_MediaFilter(df, clean = TRUE, ground_water = TRUE)),
    regexp = "Removed media types: GROUNDWATER"
  )

  expect_message(
    suppressWarnings(TADA_MediaFilter(
      df,
      clean = TRUE,
      surface_water = TRUE,
      other = TRUE
    )),
    regexp = "Removed media types: SURFACE WATER, OTHER"
  )
})

test_that("Classifies 'water' ActivityMediaName as SURFACE WATER unless groundwater fields present", {
  df_sw <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = NA_character_,
    ActivityMediaName = "water",
    AquiferName = NA_character_,
    MonitoringLocationTypeName = "River/Stream",
    stringsAsFactors = FALSE
  )
  res_sw <- TADA_MediaFilter(df_sw, clean = FALSE)
  expect_equal(res_sw$TADA.Media.Flag, "SURFACE WATER")

  df_gw <- df_sw
  df_gw$WellDepthMeasure.MeasureValue <- 10
  res_gw <- TADA_MediaFilter(df_gw, clean = FALSE)
  expect_equal(res_gw$TADA.Media.Flag, "GROUNDWATER")
})

test_that("Groundwater fields override Surface Water subdivision", {
  df <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = "Surface Water",
    AquiferName = "Some Aquifer",
    MonitoringLocationTypeName = "River/Stream",
    stringsAsFactors = FALSE
  )
  res <- TADA_MediaFilter(df, clean = FALSE)
  expect_equal(res$TADA.Media.Flag, "GROUNDWATER")
})

test_that("clean = FALSE: emits message indicating media toggles are ignored", {
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))
  expect_message(
    TADA_MediaFilter(df, clean = FALSE, ground_water = TRUE),
    regexp = "media toggles ignored because clean = FALSE"
  )
})

test_that("clean = TRUE: warns when all media toggles are TRUE (muffling post-filter warning)", {
  # Avoid noisy messages from data generation
  df <- suppressMessages(TADA_RandomTestingData(choose_random_state = TRUE))

  expect_warning(
    withCallingHandlers(
      TADA_MediaFilter(
        df,
        clean = TRUE,
        surface_water = TRUE,
        ground_water = TRUE,
        sediment = TRUE,
        other = TRUE
      ),
      warning = function(w) {
        # Muffle only the post-filter warning
        if (
          grepl(
            "All rows were removed by the media filter",
            conditionMessage(w)
          )
        ) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    regexp = "All media types are selected for removal"
  )
})
