# Combined testthat suite for depth helpers and functions

# -------------------------
# Internal helpers tests
# -------------------------

testthat::test_that(".depth_param_names returns expected vector", {
  dp <- .depth_param_names()
  testthat::expect_type(dp, "character")
  testthat::expect_true(length(dp) >= 3)
  testthat::expect_true(any(grepl("SECCHI", dp)))
})

testthat::test_that(".normalize_null_numeric handles inputs correctly", {
  testthat::expect_true(is.na(.normalize_null_numeric("null")))
  testthat::expect_true(is.na(.normalize_null_numeric(NULL)))
  testthat::expect_identical(.normalize_null_numeric(2), 2)
  testthat::expect_identical(.normalize_null_numeric(NA_real_), NA_real_)
  # Non-character non-NULL values are returned as-is
  testthat::expect_identical(.normalize_null_numeric("2"), "2")
})

testthat::test_that(".drop_avg_aggregates filters TADA- rows", {
  df <- data.frame(
    ResultIdentifier = c("A1", "TADA-A2", "B1", "TADA-B2"),
    val = 1:4,
    stringsAsFactors = FALSE
  )
  out <- .drop_avg_aggregates(df)
  testthat::expect_setequal(out$ResultIdentifier, c("A1", "B1"))
})

testthat::test_that(".ensure_depth_flag_columns runs FlagDepthCategory and can blank flags for NA thresholds", {
  # Minimal synthetic dataset (single temperature result with depth)
  df <- tibble::tibble(
    TADA.ActivityDepthHeightMeasure.MeasureValue = 1,
    TADA.ResultDepthHeightMeasure.MeasureValue = NA_real_,
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = NA_real_,
    ActivityRelativeDepthName = NA_character_,
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = "m",
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = "m",
    TADA.CharacteristicName = "TEMPERATURE",
    TADA.ResultMeasure.MeasureUnitCode = "DEG C",
    TADA.ResultMeasureValue = 10,
    ResultIdentifier = "R1",
    TADA.MonitoringLocationIdentifier = "LOC1",
    OrganizationIdentifier = "ORG1",
    ActivityStartDate = as.Date("2020-01-01"),
    # Extra fields used elsewhere
    TADA.MonitoringLocationName = "Loc 1",
    TADA.ActivityMediaName = "WATER",
    ActivityStartDateTime = "2020-01-01T08:00:00Z",
    ActivityMediaSubdivisionName = NA_character_,
    TADA.ComparableDataIdentifier = "TEMPERATURE_NONE_NONE_DEG C",
    TADA.MethodSpeciationName = "NONE",
    TADA.ResultSampleFractionText = "NONE",
    TADA.MonitoringLocationTypeName = "River/Stream"
  )

  out1 <- .ensure_depth_flag_columns(
    df,
    surfacevalue = 2,
    bottomvalue = 2,
    allow_na_thresholds = FALSE
  )
  testthat::expect_true(all(
    c(
      "TADA.ConsolidatedDepth",
      "TADA.ConsolidatedDepth.Unit",
      "TADA.ConsolidatedDepth.Bottom",
      "TADA.DepthCategory.Flag"
    ) %in%
      names(out1)
  ))
  testthat::expect_false(all(is.na(out1$TADA.DepthCategory.Flag))) # some flag assigned

  out2 <- .ensure_depth_flag_columns(
    df,
    surfacevalue = NA_real_,
    bottomvalue = NA_real_,
    allow_na_thresholds = TRUE
  )
  testthat::expect_true(all(is.na(out2$TADA.DepthCategory.Flag)))
})

# -------------------------
# Fixtures
# -------------------------

make_synth_profile_only_df <- function() {
  # Three temperature rows (profile only, no depth-parameter)
  tibble::tibble(
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 5, 9),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(
      NA_real_,
      NA_real_,
      NA_real_
    ),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(
      NA_real_,
      NA_real_,
      NA_real_
    ),
    ActivityRelativeDepthName = NA_character_,
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c("m", "m", "m"),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c("m", "m", "m"),
    TADA.CharacteristicName = c("TEMPERATURE", "TEMPERATURE", "TEMPERATURE"),
    TADA.ResultMeasure.MeasureUnitCode = c("DEG C", "DEG C", "DEG C"),
    TADA.ResultMeasureValue = c(10, 5, 1),
    ResultIdentifier = c("T1", "T2", "T3"),
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC1", "LOC1"),
    OrganizationIdentifier = c("ORG1", "ORG1", "ORG1"),
    ActivityStartDate = as.Date(rep("2020-01-01", 3)),
    TADA.MonitoringLocationName = "Loc 1",
    TADA.ActivityMediaName = "WATER",
    ActivityStartDateTime = "2020-01-01T08:00:00Z",
    ActivityMediaSubdivisionName = NA_character_,
    TADA.ComparableDataIdentifier = rep("TEMPERATURE_NONE_NONE_DEG C", 3),
    TADA.MethodSpeciationName = "NONE",
    TADA.ResultSampleFractionText = "NONE",
    TADA.MonitoringLocationTypeName = "River/Stream"
  )
}

make_synth_depth_df_meters <- function() {
  # Synthetic profile: 3 depths for temperature + 1 depth-param (secchi) row (in meters)
  tibble::tibble(
    # Use activity depth; leave result depth NA
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(0.5, 5, 9, NA),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_
    ),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_
    ),
    ActivityRelativeDepthName = NA_character_,
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c("m", "m", "m", "m"), # not used (result depth NA)
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c("m", "m", "m", "m"),
    TADA.CharacteristicName = c(
      "TEMPERATURE",
      "TEMPERATURE",
      "TEMPERATURE",
      "DEPTH, SECCHI DISK DEPTH"
    ),
    TADA.ResultMeasure.MeasureUnitCode = c("DEG C", "DEG C", "DEG C", "m"),
    TADA.ResultMeasureValue = c(10, 5, 1, 1.2), # secchi in meters
    ResultIdentifier = c("T1", "T2", "T3", "S1"),
    TADA.MonitoringLocationIdentifier = c("LOC1", "LOC1", "LOC1", "LOC1"),
    OrganizationIdentifier = c("ORG1", "ORG1", "ORG1", "ORG1"),
    ActivityStartDate = as.Date(rep("2020-01-01", 4)),
    # fields used elsewhere
    TADA.MonitoringLocationName = "Loc 1",
    TADA.ActivityMediaName = "WATER",
    ActivityStartDateTime = "2020-01-01T08:00:00Z",
    ActivityMediaSubdivisionName = NA_character_,
    TADA.ComparableDataIdentifier = c(
      "TEMPERATURE_NONE_NONE_DEG C",
      "TEMPERATURE_NONE_NONE_DEG C",
      "TEMPERATURE_NONE_NONE_DEG C",
      "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"
    ),
    TADA.MethodSpeciationName = "NONE",
    TADA.ResultSampleFractionText = "NONE",
    TADA.MonitoringLocationTypeName = "River/Stream"
  )
}

make_synth_depth_df_mixed_units_annotated <- function() {
  # Start with meters-only, annotate with FlagDepthCategory, then mutate the Secchi row to have ft unit
  df_m <- make_synth_depth_df_meters()
  df_ann <- TADA_FlagDepthCategory(df_m, dailyagg = "none")
  is_depth_param <- df_ann$TADA.CharacteristicName %in% .depth_param_names()
  # Convert the depth-parameter row "appearance" to feet for plotting conversion path
  df_ann$TADA.ResultMeasureValue[is_depth_param] <- 4
  df_ann$TADA.ResultMeasure.MeasureUnitCode[is_depth_param] <- "ft"
  df_ann$TADA.ConsolidatedDepth.Unit[is_depth_param] <- "ft" # force mismatch with figure unit ("m")
  df_ann$TADA.ConsolidatedDepth[is_depth_param] <- 4 # arbitrary ft value; plot will convert using ResultMeasureValue
  df_ann$TADA.ComparableDataIdentifier[
    is_depth_param
  ] <- "DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"
  df_ann
}

# -------------------------
# TADA_FlagDepthCategory tests
# -------------------------

testthat::test_that("TADA_FlagDepthCategory assigns Surface/Middle/Bottom with bycategory = 'no'", {
  df <- make_synth_depth_df_meters()
  out <- TADA_FlagDepthCategory(
    df,
    bycategory = "no",
    surfacevalue = 2,
    bottomvalue = 2,
    dailyagg = "none"
  )
  # Filter to temperature rows
  temp <- out[out$TADA.CharacteristicName == "TEMPERATURE", ]
  flags <- temp$TADA.DepthCategory.Flag
  testthat::expect_true(all(c("Surface", "Middle", "Bottom") %in% flags))
})

testthat::test_that("TADA_FlagDepthCategory filters categories with bycategory filters", {
  df <- make_synth_depth_df_meters()
  out_surface <- TADA_FlagDepthCategory(
    df,
    bycategory = "surface",
    dailyagg = "none"
  )
  testthat::expect_true(all(out_surface$TADA.DepthCategory.Flag == "Surface"))
  out_bottom <- TADA_FlagDepthCategory(
    df,
    bycategory = "bottom",
    dailyagg = "none"
  )
  testthat::expect_true(all(out_bottom$TADA.DepthCategory.Flag == "Bottom"))
})

testthat::test_that("TADA_FlagDepthCategory dailyagg = 'none' with aggregatedonly = TRUE errors", {
  df <- make_synth_depth_df_meters()
  testthat::expect_error(TADA_FlagDepthCategory(
    df,
    dailyagg = "none",
    aggregatedonly = TRUE
  ))
})

testthat::test_that("TADA_FlagDepthCategory dailyagg = 'avg' returns aggregate with prefix when aggregatedonly = TRUE", {
  df <- make_synth_depth_df_meters()
  out <- TADA_FlagDepthCategory(
    df,
    bycategory = "no",
    dailyagg = "avg",
    aggregatedonly = TRUE
  )
  testthat::expect_true(all(grepl("^TADA-", out$ResultIdentifier)))
  testthat::expect_equal(nrow(out), 1L) # single group aggregate (entire water column)
})

testthat::test_that("TADA_FlagDepthCategory dailyagg = 'min' and 'max' select one row each", {
  df <- make_synth_depth_df_meters()
  out_min <- TADA_FlagDepthCategory(
    df,
    bycategory = "no",
    dailyagg = "min",
    aggregatedonly = TRUE
  )
  out_max <- TADA_FlagDepthCategory(
    df,
    bycategory = "no",
    dailyagg = "max",
    aggregatedonly = TRUE
  )
  testthat::expect_equal(nrow(out_min), 1L)
  testthat::expect_equal(nrow(out_max), 1L)
  # min should pick the lowest temperature value (1 at bottom depth)
  testthat::expect_equal(out_min$TADA.ResultMeasureValue, 1)
  # max should pick the highest temperature value (10 at surface)
  testthat::expect_equal(out_max$TADA.ResultMeasureValue, 10)
})

testthat::test_that("TADA_FlagDepthCategory clean = TRUE keeps only depth categories", {
  df <- make_synth_depth_df_meters()
  out <- TADA_FlagDepthCategory(df, clean = TRUE)
  testthat::expect_true(all(
    out$TADA.DepthCategory.Flag %in% c("Surface", "Middle", "Bottom")
  ))
})

testthat::test_that("TADA_FlagDepthCategory stops on multiple depth units", {
  df <- make_synth_depth_df_meters()
  # Inject a second unit in the temperature rows by populating result depth with different unit
  df$TADA.ResultDepthHeightMeasure.MeasureValue <- df$TADA.ActivityDepthHeightMeasure.MeasureValue
  df$TADA.ResultDepthHeightMeasure.MeasureUnitCode <- c("m", "m", "ft", "ft")
  testthat::expect_error(TADA_FlagDepthCategory(df))
})

testthat::test_that("TADA_FlagDepthCategory handles data with no depth info", {
  df <- make_synth_depth_df_meters()
  # Wipe out all depth fields so depth.count == 0
  df$TADA.ActivityDepthHeightMeasure.MeasureValue <- NA_real_
  df$TADA.ResultDepthHeightMeasure.MeasureValue <- NA_real_
  out <- TADA_FlagDepthCategory(df)
  testthat::expect_true(all(is.na(out$TADA.ConsolidatedDepth)))
  testthat::expect_true(all(is.na(out$TADA.ConsolidatedDepth.Bottom)))
  testthat::expect_true(all(is.na(out$TADA.DepthCategory.Flag)))
})

# -------------------------
# TADA_IDDepthProfiles tests
# -------------------------

testthat::test_that("TADA_IDDepthProfiles lists characteristics with counts (default)", {
  df <- make_synth_depth_df_meters()
  out <- TADA_IDDepthProfiles(
    df,
    nresults = TRUE,
    nvalue = 2,
    aggregates = FALSE
  )
  testthat::expect_true(all(
    c(
      "TADA.MonitoringLocationIdentifier",
      "TADA.MonitoringLocationName",
      "OrganizationIdentifier",
      "ActivityStartDate",
      "TADA.CharacteristicsForDepthProfile"
    ) %in%
      names(out)
  ))
  # Should include temperature comparable ID with count "(3)"
  testthat::expect_true(any(grepl(
    "TEMPERATURE_NONE_NONE_DEG C \\(3\\)",
    out$TADA.CharacteristicsForDepthProfile
  )))
})

testthat::test_that("TADA_IDDepthProfiles without counts and higher threshold", {
  df <- make_synth_depth_df_meters()
  # With nvalue = 3, temperature group qualifies (3 depths)
  out <- TADA_IDDepthProfiles(
    df,
    nresults = FALSE,
    nvalue = 3,
    aggregates = FALSE
  )
  testthat::expect_true(any(grepl(
    "TEMPERATURE_NONE_NONE_DEG C",
    out$TADA.CharacteristicsForDepthProfile
  )))
  # With nvalue = 4, temperature is dropped; depth-parameter remains only if a profile is present
  out2 <- TADA_IDDepthProfiles(
    df,
    nresults = FALSE,
    nvalue = 4,
    aggregates = FALSE
  )
  # Because the function also requires MeanResults > 1 across the group,
  # and only secchi has 1 depth, the whole group will be filtered out.
  testthat::expect_equal(nrow(out2), 0)
})

testthat::test_that("TADA_IDDepthProfiles respects aggregates = FALSE by ignoring TADA- average rows", {
  df <- make_synth_depth_df_meters()

  # First, annotate the base data with consolidated depth/category columns
  df_annot <- TADA_FlagDepthCategory(df, dailyagg = "none")

  # Baseline: no TADA- rows present
  base_out <- TADA_IDDepthProfiles(
    df_annot,
    nresults = TRUE,
    aggregates = FALSE
  )

  # Create an averaged aggregate row from the annotated data and append
  avg_only <- TADA_FlagDepthCategory(
    df_annot,
    bycategory = "no",
    dailyagg = "avg",
    aggregatedonly = TRUE
  )
  df2 <- dplyr::bind_rows(df_annot, avg_only)

  # Now run IDDepthProfiles with aggregates = FALSE; TADA- row should be ignored
  out <- TADA_IDDepthProfiles(df2, nresults = TRUE, aggregates = FALSE)

  # Normalize for comparison:
  normalize_df <- function(x) {
    x |>
      dplyr::ungroup() |>
      dplyr::mutate(
        TADA.CharacteristicsForDepthProfile = stringr::str_squish(
          TADA.CharacteristicsForDepthProfile
        )
      ) |>
      dplyr::arrange(
        TADA.MonitoringLocationIdentifier,
        OrganizationIdentifier,
        ActivityStartDate,
        TADA.CharacteristicsForDepthProfile
      )
  }

  out_norm <- normalize_df(out)
  base_norm <- normalize_df(base_out)

  testthat::expect_equal(nrow(out_norm), nrow(base_norm))
  testthat::expect_equal(names(out_norm), names(base_norm))
  testthat::expect_equal(out_norm, base_norm, ignore_attr = TRUE)
})

testthat::test_that("TADA_DepthProfilePlot checks non-depth-parameter units against `unit`", {
  testthat::skip_if_not_installed("plotly")
  # Profile-only data in meters; asking for ft should error
  df <- make_synth_profile_only_df()
  testthat::expect_error(TADA_DepthProfilePlot(
    df,
    groups = c("TEMPERATURE_NONE_NONE_DEG C"),
    location = "LOC1",
    activity_date = as.Date("2020-01-01"),
    depthcat = FALSE,
    unit = "ft"
  ))
})

testthat::test_that("TADA_DepthProfilePlot argument validation for missing inputs", {
  testthat::skip_if_not_installed("plotly")
  df <- make_synth_profile_only_df()
  testthat::expect_error(TADA_DepthProfilePlot(df)) # missing location/date/groups
  testthat::expect_error(TADA_DepthProfilePlot(
    df,
    groups = c("TEMPERATURE_NONE_NONE_DEG C"),
    location = "NOT_IN_DATA",
    activity_date = as.Date("2020-01-01")
  ))
  testthat::expect_error(TADA_DepthProfilePlot(
    df,
    groups = c("NOT_A_GROUP"),
    location = "LOC1",
    activity_date = as.Date("2020-01-01")
  ))
})

testthat::test_that("TADA_DepthProfilePlot depthcat requires at least one threshold when TRUE", {
  testthat::skip_if_not_installed("plotly")
  df <- make_synth_profile_only_df()
  testthat::expect_error(TADA_DepthProfilePlot(
    df,
    groups = c("TEMPERATURE_NONE_NONE_DEG C"),
    location = "LOC1",
    activity_date = as.Date("2020-01-01"),
    depthcat = TRUE,
    surfacevalue = NA_real_,
    bottomvalue = NA_real_,
    unit = "m"
  ))
})
