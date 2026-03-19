# Compact dummy dataset covering SURFACE WATER, GROUNDWATER, SEDIMENT, OTHER, plus GW override.
dummy_media_df <- function() {
  df <- data.frame(
    ResultIdentifier = paste0("R", 1:6),
    ActivityMediaSubdivisionName = c(
      "Surface Water", # SURFACE WATER
      NA, # GROUNDWATER (via well/aquifer)
      NA, # SEDIMENT (via ActivityMediaName)
      NA, # OTHER (HABITAT)
      NA, # SURFACE WATER (via "water")
      NA # GROUNDWATER override (via GW field)
    ),
    AquiferName = c(NA, "Aquifer A", NA, NA, NA, NA),
    MonitoringLocationTypeName = c(
      "River/Stream",
      "Well",
      "River/Stream",
      "Site",
      "River/Stream",
      "Site"
    ),
    ActivityMediaName = c(
      NA,
      NA,
      "SEDIMENT", # ensure SEDIMENT classification
      "HABITAT", # OTHER normalization
      "water", # SURFACE WATER unless GW fields present
      "water" # overridden to GW via GW field below
    ),
    stringsAsFactors = FALSE
  )
  # Add a groundwater field to force GW override on last row
  df$WellDepthMeasure.MeasureValue <- c(NA, NA, NA, NA, NA, 10)
  df
}

test_that("clean = FALSE: adds flag and normalizes to core values", {
  df <- dummy_media_df()
  res <- suppressMessages(TADA_MediaFilter(df, clean = FALSE))
  flags <- sort(unique(res$TADA.Media.Flag))
  expect_true(all(
    flags %in% c("SURFACE WATER", "GROUNDWATER", "SEDIMENT", "OTHER")
  ))
  expect_true("SEDIMENT" %in% flags) # ensure dummy covers sediment
})

test_that("clean = TRUE: removes selected media and drops flag column", {
  df <- dummy_media_df()

  # Silence messages from the clean = FALSE pass
  flagged <- suppressMessages(TADA_MediaFilter(df, clean = FALSE))
  flags <- flagged$TADA.Media.Flag
  expected_n <- sum(flags %in% c("SURFACE WATER", "GROUNDWATER"))

  # Execute clean = TRUE removing SEDIMENT and OTHER; capture but don't assert on messages
  ev <- testthat::evaluate_promise(TADA_MediaFilter(
    df,
    clean = TRUE,
    sediment = TRUE,
    other = TRUE
  ))
  res_clean <- ev$result

  # Rows match expectation
  expect_equal(nrow(res_clean), expected_n)

  # Helper/flag columns are absent in clean mode
  expect_false("TADA.Media.Flag" %in% names(res_clean))
  expect_false("gw_has_fields" %in% names(res_clean))

  # Verify exactly which media were removed (behavioral check)
  kept_ids <- res_clean$ResultIdentifier
  removed_flags <- unique(flagged$TADA.Media.Flag[
    !flagged$ResultIdentifier %in% kept_ids
  ])
  expect_setequal(removed_flags, c("SEDIMENT", "OTHER"))
})

test_that("clean = TRUE: warns when all media toggles are TRUE", {
  df <- dummy_media_df()
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

test_that("clean = TRUE: message when no toggles are set", {
  df <- dummy_media_df()
  expect_message(
    res_clean <- TADA_MediaFilter(df, clean = TRUE),
    regexp = "No media types selected for removal"
  )
  expect_equal(nrow(res_clean), nrow(df))
  expect_false("TADA.Media.Flag" %in% names(res_clean))
})

test_that("clean = TRUE: emits a message when no toggles set", {
  df <- dummy_media_df()
  ev <- testthat::evaluate_promise(TADA_MediaFilter(df, clean = TRUE))
  combined <- c(ev$output, ev$messages, ev$warnings)
  expect_true(
    any(grepl(
      "No media types selected for removal",
      combined,
      ignore.case = TRUE
    )) ||
      any(grepl("Removed media types:\\s*none", combined, ignore.case = TRUE))
  )
  # And still verify behavior:
  expect_equal(nrow(ev$result), nrow(df))
  expect_false("TADA.Media.Flag" %in% names(ev$result))
})

test_that("Clean-mode output lists exactly the removed media types", {
  df <- dummy_media_df()

  # Groundwater only
  ev1 <- testthat::evaluate_promise(suppressWarnings(TADA_MediaFilter(
    df,
    clean = TRUE,
    ground_water = TRUE
  )))
  combined1 <- paste(c(ev1$output, ev1$messages), collapse = "\n")
  expect_true(grepl("media types:\\s*GROUNDWATER\\b", combined1))

  # Surface water + other
  ev2 <- testthat::evaluate_promise(suppressWarnings(TADA_MediaFilter(
    df,
    clean = TRUE,
    surface_water = TRUE,
    other = TRUE
  )))
  combined2 <- paste(c(ev2$output, ev2$messages), collapse = "\n")

  # Either order is acceptable
  expect_true(
    grepl("media types:\\s*SURFACE WATER, OTHER", combined2) ||
      grepl("media types:\\s*OTHER, SURFACE WATER", combined2)
  )

  # Or, alternatively, just ensure both tokens appear somewhere after "media types:"
  # expect_true(grepl("media types:", combined2) &&
  #             grepl("\\bSURFACE WATER\\b", combined2) &&
  #             grepl("\\bOTHER\\b", combined2))
})

test_that("clean = FALSE: emits message indicating media toggles are ignored", {
  df <- dummy_media_df()
  expect_message(
    TADA_MediaFilter(df, clean = FALSE, ground_water = TRUE),
    regexp = "media toggles ignored because clean = FALSE"
  )
})

test_that("clean = TRUE: warns when all media toggles are TRUE (muffling post-filter warning)", {
  df <- dummy_media_df()
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

test_that("AquiferName present without GW fields classifies as OTHER", {
  df <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = "Surface Water",
    AquiferName = "Some Aquifer",
    MonitoringLocationTypeName = "River/Stream",
    stringsAsFactors = FALSE
  )
  res <- suppressMessages(TADA_MediaFilter(df, clean = FALSE))
  expect_equal(res$TADA.Media.Flag, "OTHER")
})

test_that("Groundwater fields override Surface Water subdivision", {
  df <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = "Surface Water",
    AquiferName = "Some Aquifer",
    MonitoringLocationTypeName = "River/Stream",
    WellDepthMeasure.MeasureValue = 10,
    stringsAsFactors = FALSE
  )
  res <- suppressMessages(TADA_MediaFilter(df, clean = FALSE))
  expect_equal(res$TADA.Media.Flag, "GROUNDWATER")
})

test_that("Well location type classifies as GROUNDWATER regardless of AquiferName", {
  df <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = "Surface Water",
    AquiferName = "Some Aquifer",
    MonitoringLocationTypeName = "Well",
    stringsAsFactors = FALSE
  )
  res <- suppressMessages(TADA_MediaFilter(df, clean = FALSE))
  expect_equal(res$TADA.Media.Flag, "GROUNDWATER")
})

test_that("AquiferName present without GW fields classifies as OTHER", {
  df <- data.frame(
    ResultIdentifier = "R1",
    ActivityMediaSubdivisionName = "Surface Water",
    AquiferName = "Some Aquifer",
    MonitoringLocationTypeName = "River/Stream",
    stringsAsFactors = FALSE
  )
  res <- suppressMessages(TADA_MediaFilter(df, clean = FALSE))
  expect_equal(res$TADA.Media.Flag, "OTHER")
})
