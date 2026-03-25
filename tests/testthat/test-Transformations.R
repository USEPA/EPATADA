# Helpers: deterministic dummy data -----------------------------

make_dummy_nutrient_df <- function() {
  # A tiny dataset with 1 site, 2 days, and both N and P-related rows.
  # Includes all columns required by TADA_CalculateTotalNP and sets flags to "Pass"/"Non_QC"
  # so flagging functions need not be invoked during tests.

  data.frame(
    ActivityStartDate = as.Date(c(
      "2025-01-01",
      "2025-01-01",
      "2025-01-01",
      "2025-01-02"
    )),
    TADA.MonitoringLocationIdentifier = c(
      "SITE-001",
      "SITE-001",
      "SITE-001",
      "SITE-001"
    ),
    TADA.MonitoringLocationName = c("Site 1", "Site 1", "Site 1", "Site 1"),
    TADA.LongitudeMeasure = c(-111.9000, -111.9000, -111.9000, -111.9000),
    TADA.LatitudeMeasure = c(40.7000, 40.7000, 40.7000, 40.7000),
    TADA.ActivityMediaName = c("Water", "Water", "Water", "Water"),
    TADA.CharacteristicName = c(
      "Nitrate + Nitrite", # N species
      "Total Kjeldahl Nitrogen", # N species
      "Orthophosphate", # P form
      "Total Phosphorus" # P total form
    ),
    TADA.MethodSpeciationName = c("AS N", "AS N", "AS P", "AS P"),
    TADA.ResultSampleFractionText = c(
      "UNFILTERED",
      "UNFILTERED",
      "UNFILTERED",
      "UNFILTERED"
    ),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "UG/L", "UG/L"),
    TADA.ResultMeasureValue = c(0.2, 0.5, 10, 30),
    ActivityTypeCode = c(
      "Field Msr/Obs",
      "Field Msr/Obs",
      "Field Msr/Obs",
      "Field Msr/Obs"
    ),
    OrganizationIdentifier = c("ORG-1", "ORG-1", "ORG-1", "ORG-1"),
    OrganizationFormalName = c("Org One", "Org One", "Org One", "Org One"),
    CountryCode = c("US", "US", "US", "US"),
    StateCode = c("UT", "UT", "UT", "UT"),
    CountyCode = c("035", "035", "035", "035"),
    HUCEightDigitCode = c("16020204", "16020204", "16020204", "16020204"),
    MonitoringLocationTypeName = c(
      "River/Stream",
      "River/Stream",
      "River/Stream",
      "River/Stream"
    ),
    MonitoringLocationDescriptionText = c(
      "Upstream reach",
      "Upstream reach",
      "Upstream reach",
      "Upstream reach"
    ),
    ActivityRelativeDepthName = c(NA, NA, NA, NA),
    ActivityMediaSubdivisionName = c(NA, NA, NA, NA),
    TADA.ComparableDataIdentifier = c(NA, NA, NA, NA),
    TADA.ResultMeasureValueDataTypes.Flag = c(
      "Numeric",
      "Numeric",
      "Numeric",
      "Numeric"
    ),

    # Pre-set flags to avoid invoking flagging functions during tests
    TADA.ActivityType.Flag = c("Non_QC", "Non_QC", "Non_QC", "Non_QC"),
    TADA.ResultUnit.Flag = c("Pass", "Pass", "Pass", "Pass"),
    TADA.SampleFraction.Flag = c("Pass", "Pass", "Pass", "Pass"),
    TADA.MethodSpeciation.Flag = c("Pass", "Pass", "Pass", "Pass"),

    # Provide stable identifiers for integrity tests
    ResultIdentifier = paste0("RID-", seq_len(4)),
    stringsAsFactors = FALSE
  )
}

make_dummy_non_np_df <- function() {
  # Dataset with a non-nutrient characteristic to exercise the
  # "not applicable" branch (no TN/TP totals possible)

  data.frame(
    ActivityStartDate = as.Date(c("2025-03-13", "2025-03-14")),
    TADA.MonitoringLocationIdentifier = c("SITE-XYZ", "SITE-XYZ"),
    TADA.MonitoringLocationName = c("Site X", "Site X"),
    TADA.LongitudeMeasure = c(-80.0, -80.0),
    TADA.LatitudeMeasure = c(34.0, 34.0),
    TADA.ActivityMediaName = c("Water", "Water"),
    TADA.CharacteristicName = c(
      "Dissolved oxygen (DO)",
      "Dissolved oxygen (DO)"
    ),
    TADA.MethodSpeciationName = c(NA, NA),
    TADA.ResultSampleFractionText = c("WATER", "WATER"),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L"),
    TADA.ResultMeasureValue = c(8.5, 9.1),
    ActivityTypeCode = c("Field Msr/Obs", "Field Msr/Obs"),
    OrganizationIdentifier = c("ORG-2", "ORG-2"),
    OrganizationFormalName = c("Org Two", "Org Two"),
    CountryCode = c("US", "US"),
    StateCode = c("SC", "SC"),
    CountyCode = c("015", "015"),
    HUCEightDigitCode = c("03040201", "03040201"),
    MonitoringLocationTypeName = c("River/Stream", "River/Stream"),
    MonitoringLocationDescriptionText = c(
      "Downstream reach",
      "Downstream reach"
    ),
    ActivityRelativeDepthName = c(NA, NA),
    ActivityMediaSubdivisionName = c(NA, NA),
    TADA.ComparableDataIdentifier = c(NA, NA),
    TADA.ResultMeasureValueDataTypes.Flag = c("Numeric", "Numeric"),

    TADA.ActivityType.Flag = c("Non_QC", "Non_QC"),
    TADA.ResultUnit.Flag = c("Pass", "Pass"),
    TADA.SampleFraction.Flag = c("Pass", "Pass"),
    TADA.MethodSpeciation.Flag = c("Pass", "Pass"),

    ResultIdentifier = c("RID-A", "RID-B"),
    stringsAsFactors = FALSE
  )
}

# Tests ---------------------------------------------------------

test_that("harmonization works (dummy data)", {
  dat <- make_dummy_nutrient_df()

  # Use the package synonym reference
  ref <- TADA_GetSynonymRef(dat)
  dat1 <- TADA_HarmonizeSynonyms(dat, ref = ref)

  expect_equal(nrow(dat1), nrow(dat))
})

# If combinations are added to HarmonizationTemplate.csv, they must also
# be included in NPsummation_key.csv. If this test fails, you likely need
# to add missing rows to NPsummation_key.csv
test_that("np summation key matches nutrient harmonization ref", {
  # Load and restrict to N/P from the harmonization template
  harm <- TADA_GetSynonymRef()
  harm <- unique(subset(
    harm,
    harm$HarmonizationGroup %in% c("Phosphorus", "Nitrogen")
  )[, c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )])

  # Load summation ref keys
  np <- TADA_GetNutrientSummationRef()[, c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )]
  np$np <- 1L

  # NA-aware join to respect the package’s normalization rules
  check <- dplyr::left_join(
    harm,
    np,
    by = c(
      "TADA.CharacteristicName",
      "TADA.ResultSampleFractionText",
      "TADA.MethodSpeciationName"
    ),
    na_matches = "na"
  )

  # Assert all nutrient harmonization keys are covered by the summation ref
  expect_false(any(is.na(check$np)))

  # Optional: print any true mismatches for debugging
  missing <- check[is.na(check$np), , drop = FALSE]
  if (nrow(missing) > 0) {
    message("Missing from summation ref (showing up to 10 rows):")
    print(utils::head(missing, 10))
  }
})

test_that("TADA_CalculateTotalNP keeps values numeric and non-NA (dummy)", {
  testdat <- make_dummy_nutrient_df()

  # Convert special chars in case helper evolves; should remain numeric
  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  testdat <- TADA_CalculateTotalNP(testdat, daily_agg = "max")

  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))
  expect_false(any(is.na(testdat$TADA.ResultMeasureValue)))
})

test_that("TADA package functions maintain ResultIdentifier integrity (dummy)", {
  df2 <- make_dummy_nutrient_df()

  # Harmonize synonyms (optional for this integrity check)
  df2 <- TADA_HarmonizeSynonyms(df2)

  # Calculate total NP with daily aggregation
  df3 <- TADA_CalculateTotalNP(df2, daily_agg = "max")

  # Original identifiers should be preserved
  original_identifiers <- unique(df2$ResultIdentifier)
  combined_identifiers <- unique(df3$ResultIdentifier)

  missing_identifiers <- setdiff(original_identifiers, combined_identifiers)
  expect_equal(
    length(missing_identifiers),
    0L,
    info = paste(
      "Missing identifiers:",
      paste(missing_identifiers, collapse = ", ")
    )
  )

  # No duplicate ResultIdentifier values in df3
  expect_false(any(duplicated(df3$ResultIdentifier)))
})

test_that("TADA_CalculateTotalNP maintains ResultIdentifier integrity when not applicable (dummy)", {
  df2 <- make_dummy_non_np_df()

  # Harmonize synonyms (no effect on non-N/P, but keeps pipeline similar)
  df2 <- TADA_HarmonizeSynonyms(df2)

  # Calculate total NP with daily aggregation (no totals expected)
  df3 <- TADA_CalculateTotalNP(df2, daily_agg = "max")

  original_identifiers <- unique(df2$ResultIdentifier)
  combined_identifiers <- unique(df3$ResultIdentifier)

  missing_identifiers <- setdiff(original_identifiers, combined_identifiers)
  expect_equal(
    length(missing_identifiers),
    0L,
    info = paste(
      "Missing identifiers:",
      paste(missing_identifiers, collapse = ", ")
    )
  )

  expect_false(any(duplicated(df3$ResultIdentifier)))
})

test_that("daily_agg blank (NULL) treated as 'max' (dummy, full path)", {
  df <- make_dummy_nutrient_df()

  # Ensure keys match the summation reference
  df <- TADA_HarmonizeSynonyms(df)

  # Create a duplicate measurement to force aggregation flags to exist
  dup <- df[1, ]
  dup$ResultIdentifier <- "RID-1DUP"
  dup$TADA.ResultMeasureValue <- 0.1
  df <- rbind(df, dup)

  out_max <- TADA_CalculateTotalNP(df, daily_agg = "max")
  out_null <- TADA_CalculateTotalNP(df, daily_agg = NULL)

  # Same sorting and column comparison as your original test
  keycols <- c(
    "ActivityStartDate",
    "TADA.MonitoringLocationIdentifier",
    "TADA.CharacteristicName",
    "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText",
    "ResultIdentifier"
  )
  out_max_ord <- dplyr::arrange(out_max, dplyr::across(dplyr::all_of(keycols)))
  out_null_ord <- dplyr::arrange(
    out_null,
    dplyr::across(dplyr::all_of(keycols))
  )

  comp_cols <- c(
    keycols,
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.NutrientSummation.Flag"
  )
  expect_equal(out_null_ord[, comp_cols], out_max_ord[, comp_cols])
})

test_that("daily aggregation preserves considered-but-not-selected rows (dummy)", {
  df <- make_dummy_nutrient_df()

  # Duplicate a row to create two same-day, same-metadata measurements that require aggregation
  dup <- df[1, ]
  dup$ResultIdentifier <- "RID-1DUP"
  dup$TADA.ResultMeasureValue <- 0.1 # lower than existing 0.2 so max should select the original

  agg_df <- rbind(df, dup)

  out <- TADA_CalculateTotalNP(agg_df, daily_agg = "max")

  # The lower value should be present and flagged as considered-but-not-selected
  expect_true(any(
    out$ResultIdentifier == "RID-1DUP" &
      out$TADA.ResultValueAggregation.Flag ==
        "Considered in max aggregation function but not selected" &
      out$TADA.NutrientSummation.Flag == "Not used to calculate Total N or P."
  ))

  # The higher value should be selected
  expect_true(any(
    out$ResultIdentifier == "RID-1" &
      out$TADA.ResultValueAggregation.Flag == "Selected as max aggregate value"
  ))
})
