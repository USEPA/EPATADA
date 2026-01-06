test_that("harmonization works", {
  dat <- TADA_RandomTestingData(choose_random_state = TRUE)

  # Check if the required data frame is empty or null
  if (is.null(dat) || nrow(dat) == 0) {
    skip("Skipping test because dat is empty or null")
  }

  dat <- subset(dat, !is.na(dat$TADA.ResultMeasureValue))
  dat <- TADA_FlagFraction(dat, clean = TRUE)
  dat <- TADA_FlagResultUnit(dat, clean = "suspect_only")
  dat <- TADA_FlagSpeciation(dat, clean = "suspect_only")
  dat <- TADA_FlagMethod(dat, clean = TRUE)

  ref <- TADA_GetSynonymRef(dat)
  dat1 <- TADA_HarmonizeSynonyms(dat, ref = ref)

  expect_true(dim(dat)[1] == dim(dat1)[1])
})


test_that("np summation key matches nutrient harmonization ref", {
  harm <- TADA_GetSynonymRef()
  harm <- unique(subset(
    harm,
    harm$HarmonizationGroup %in% c("Phosphorus", "Nitrogen")
  )[, c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )])
  np <- TADA_GetNutrientSummationRef()[, c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName"
  )]
  np$np <- 1
  check <- merge(harm, np, all.x = TRUE)
  expect_false(any(is.na(check$np)))
})


test_that("TADA_CalculateTotalNP does not introduce duplicates or NAs in result cols", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)

  # Check if the required data frame is empty or null
  if (is.null(testdat) || nrow(testdat) == 0) {
    skip("Skipping test because testdat is empty or null")
  }

  testdat <- TADA_ConvertSpecialChars(
    testdat,
    col = "TADA.ResultMeasureValue",
    clean = TRUE
  )

  testdat <- TADA_CalculateTotalNP(testdat, daily_agg = "max")

  # na_rows <- testdat |> filter(is.na(TADA.ResultMeasureValue))

  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))

  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))

  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))
})

test_that("TADA package functions maintain ResultIdentifier integrity", {
  # Generate random testing data
  df <- TADA_RandomTestingData(choose_random_state = TRUE)

  # Check if the required data frame is empty or null
  if (is.null(df) || nrow(df) == 0) {
    skip("Skipping test because df is empty or null")
  }

  # Apply simple censored methods
  df2 <- TADA_SimpleCensoredMethods(
    df,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )

  # Check if df2 is empty or null
  if (is.null(df2) || nrow(df2) == 0) {
    skip("Skipping test because df2 is empty or null")
  }

  # Run key flag functions
  df2 <- TADA_RunKeyFlagFunctions(df2, clean = TRUE)

  # Harmonize synonyms
  df2 <- TADA_HarmonizeSynonyms(df2)

  # Calculate total NP with daily aggregation
  df3 <- TADA_CalculateTotalNP(df2, daily_agg = "max")

  # Check that all ResultIdentifier values from the original df2 are in df3
  original_identifiers <- unique(df2$ResultIdentifier)
  combined_identifiers <- unique(df3$ResultIdentifier)

  # Test that no identifiers are missing
  missing_identifiers <- setdiff(original_identifiers, combined_identifiers)
  expect_true(
    length(missing_identifiers) == 0,
    info = paste(
      "Missing identifiers:",
      paste(missing_identifiers, collapse = ", ")
    )
  )

  # Test for duplicate ResultIdentifier values in df3
  duplicate_ids <- df3$ResultIdentifier[duplicated(df3$ResultIdentifier)]
  expect_false(
    any(duplicated(df3$ResultIdentifier)),
    info = paste(
      "Duplicate ResultIdentifier values found:",
      paste(duplicate_ids, collapse = ", ")
    )
  )

  # Optionally verify column names
  # print(names(df2))  # Uncomment to print column names for verification

  # Optionally subset df2 to include only rows with missing identifiers
  # filtered_df2 <- df2[df2$ResultIdentifier %in% missing_identifiers, ]
})

test_that("TADA_CalculateTotalNP maintains ResultIdentifier integrity when not applicable", {
  # Generate random testing data
  df <- TADA_DataRetrieval(
    statecode = "SC",
    startDate = "2025-03-13",
    endDate = "2025-03-14",
    ask = FALSE
  )

  # Check if the required data frame is empty or null
  if (is.null(df) || nrow(df) == 0) {
    skip("Skipping test because df is empty or null")
  }

  # Apply simple censored methods
  df2 <- TADA_SimpleCensoredMethods(
    df,
    nd_method = "multiplier",
    nd_multiplier = 0.5,
    od_method = "as-is",
    od_multiplier = "null"
  )

  # Check if df2 is empty or null
  if (is.null(df2) || nrow(df2) == 0) {
    skip("Skipping test because df2 is empty or null")
  }

  # Run key flag functions
  df2 <- TADA_RunKeyFlagFunctions(df2, clean = TRUE)

  # Harmonize synonyms
  df2 <- TADA_HarmonizeSynonyms(df2)

  # Calculate total NP with daily aggregation
  df3 <- TADA_CalculateTotalNP(df2, daily_agg = "max")

  # Check that all ResultIdentifier values from the original df2 are in df3
  original_identifiers <- unique(df2$ResultIdentifier)
  combined_identifiers <- unique(df3$ResultIdentifier)

  # Test that no identifiers are missing
  missing_identifiers <- setdiff(original_identifiers, combined_identifiers)
  expect_true(
    length(missing_identifiers) == 0,
    info = paste(
      "Missing identifiers:",
      paste(missing_identifiers, collapse = ", ")
    )
  )

  # Test for duplicate ResultIdentifier values in df3
  duplicate_ids <- df3$ResultIdentifier[duplicated(df3$ResultIdentifier)]
  expect_false(
    any(duplicated(df3$ResultIdentifier)),
    info = paste(
      "Duplicate ResultIdentifier values found:",
      paste(duplicate_ids, collapse = ", ")
    )
  )

  # Optionally verify column names
  # print(names(df2))  # Uncomment to print column names for verification

  # Optionally subset df2 to include only rows with missing identifiers
  # filtered_df2 <- df2[df2$ResultIdentifier %in% missing_identifiers, ]
})
