test_that("harmonization works", {
  dat <- TADA_RandomTestingData(choose_random_state = TRUE)
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
  harm <- unique(subset(harm, harm$HarmonizationGroup %in% c("Phosphorus", "Nitrogen"))[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")])
  np <- TADA_GetNutrientSummationRef()[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")]
  np$np <- 1
  check <- merge(harm, np, all.x = TRUE)
  expect_false(any(is.na(check$np)))
})



test_that("TADA_CalculateTotalNP does not introduce duplicates or NAs in result cols", {
  testdat <- TADA_RandomTestingData(choose_random_state = TRUE)
  
  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  testdat <- TADA_CalculateTotalNP(testdat, daily_agg = "max")
  
  # na_rows <- testdat %>% filter(is.na(TADA.ResultMeasureValue))
  
  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))
  
  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))
  
  # # Test to ensure unit column does not contain any NA values
  # expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))
})

test_that("TADA_CalculateTotalNP exclude data logic is not missing results", {
  df_original <- TADA_RandomTestingData(choose_random_state = TRUE)
  
  # Check if QC flag function ran and message warning if not
  if (!"TADA.ActivityType.Flag" %in% names(df_original)) {
    message("TADA_CalculateTotalNP: Your input dataset was missing the TADA.ActivityType.Flag column, suggesting that QC replicates have not been addressed or reviewed. Running the TADA_FindQCActivities function with the clean = FALSE option before executing this function. This function will not include QC results when aggregating to a daily maximum and total nutrient value.")
    df_review <- TADA_FindQCActivities(df_original, clean = FALSE)
  } else {
    df_review <- df_original
  }
  
  # Check if unit flag function ran and message warning if not
  if (!"TADA.ResultUnit.Flag" %in% names(df_review)) {
    message("TADA_CalculateTotalNP: Your input dataset was missing the TADA.ResultUnit.Flag column, suggesting that unit and characteristic combinations have not been addressed or reviewed. Running the TADA_FlagResultUnit function with the clean = FALSE option before executing this function. This function will not include results with invalid or suspect units when aggregating to a daily maximum and total nutrient value.")
    df_review <- TADA_FlagResultUnit(df_review, clean = "none")
  }
  
  # Check if fraction flag function ran and message if not
  if (!"TADA.SampleFraction.Flag" %in% names(df_review)) {
    message("TADA_CalculateTotalNP: Your input dataset was missing the TADA.SampleFraction.Flag column, suggesting that fraction and characteristic combinations have not been addressed or reviewed. Running the TADA_FlagFraction function with the clean = FALSE option before executing this function. This function will not include results with invalid or suspect fractions when aggregating to a daily maximum and total nutrient value.")
    df_review <- TADA_FlagFraction(df_review, clean = FALSE)
  }
  
  # Check if speciation flag function ran and message warning if not
  if (!"TADA.MethodSpeciation.Flag" %in% names(df_review)) {
    message("TADA_CalculateTotalNP: Your input dataset was missing the TADA.MethodSpeciation.Flag column, suggesting that speciation and characteristic combinations have not been addressed or reviewed. Running the TADA_FlagSpeciation function with the clean = FALSE option before executing this function. This function will not include results with invalid or suspect speciations when aggregating to a daily maximum and total nutrient value.")
    df_review <- TADA_FlagSpeciation(df_review, clean = "none")
  }
  
  # Create the include and exclude data frames
  include_df <- df_review[df_review$TADA.ActivityType.Flag == "Non_QC" &
                            (df_review$TADA.ResultMeasureValueDataTypes.Flag %in% 
                               c("Numeric", 
                                 "Result Value/Unit Estimated from Detection Limit", 
                                 "Less Than",
                                 "Percentage",
                                 "Approximate Value",
                                 "Greater Than",
                                 "Comma-Separated Numeric",
                                 "Numeric Range - Averaged",
                                 "Percentage Range - Averaged",
                                 "Approximate Value")) &
                            (df_review$TADA.ResultUnit.Flag %in% 
                               c("Pass", 
                                 "Not Reviewed")) &
                            (df_review$TADA.SampleFraction.Flag %in% 
                               c("Pass", 
                                 "Not Reviewed")) &
                            (df_review$TADA.MethodSpeciation.Flag %in% 
                               c("Pass", 
                                 "Not Reviewed")), ]
  
  exclude_df <- df_review[df_review$TADA.ActivityType.Flag != "Non_QC" |
                            is.na(df_review$TADA.ResultMeasureValueDataTypes.Flag) |
                            (df_review$TADA.ResultMeasureValueDataTypes.Flag %in%
                               c("NA - Not Available",
                                 "Text",
                                 "Non-ASCII Character(s)",
                                 "Result Value/Unit Cannot Be Estimated From Detection Limit")) |
                            !(df_review$TADA.ResultUnit.Flag %in% 
                                c("Pass",
                                  "Not Reviewed")) |
                            !(df_review$TADA.SampleFraction.Flag %in% 
                                c("Pass",
                                  "Not Reviewed")) |
                            !(df_review$TADA.MethodSpeciation.Flag %in%
                                c("Pass",
                                  "Not Reviewed")), ]
  
  # Calculate the number of rows in each data frame
  total_rows_data <- nrow(df_review)
  total_rows_include <- nrow(include_df)
  total_rows_exclude <- nrow(exclude_df)
  
  # Check if the sum of rows in include_df and exclude_df equals the total rows in .data
  expect_equal(total_rows_include + total_rows_exclude, total_rows_data, 
               info = "The sum of rows in include_df and exclude_df should equal the total rows in .data.")
})

test_that("TADA package functions maintain ResultIdentifier integrity", {
  # Generate random testing data
  df <- TADA_RandomTestingData(choose_random_state = TRUE)
  
  # Apply simple censored methods
  df2 <- TADA_SimpleCensoredMethods(df, nd_method = "multiplier",
                                    nd_multiplier = 0.5, od_method = "as-is", 
                                    od_multiplier = "null")
  
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
  expect_true(length(missing_identifiers) == 0, 
              info = paste("Missing identifiers:", paste(missing_identifiers, collapse = ", ")))
  
  # Test for duplicate ResultIdentifier values in df3
  duplicate_ids <- df3$ResultIdentifier[duplicated(df3$ResultIdentifier)]
  expect_false(any(duplicated(df3$ResultIdentifier)), 
               info = paste("Duplicate ResultIdentifier values found:", paste(duplicate_ids, collapse = ", ")))
  
  # Optionally verify column names
  # print(names(df2))  # Uncomment to print column names for verification
  
  # Optionally subset df2 to include only rows with missing identifiers
  # filtered_df2 <- df2[df2$ResultIdentifier %in% missing_identifiers, ]
})
