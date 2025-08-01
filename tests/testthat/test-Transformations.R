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

test_that("TADA_CalculateTotalNP does not introduce NAs in cols", {
  today <- Sys.Date()
  oneyearago <- as.character(today - 1 * 365)
  testdat <- TADA_DataRetrieval(statecode = "UT", 
                                startDate = oneyearago, 
                                characteristicType = "Nutrient",
                                ask = FALSE)
  
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", "NA - Not Available", "Text", "Percentage", "Less Than")))
  
  testdat <- TADA_SimpleCensoredMethods(testdat,
                                        nd_method = "multiplier",
                                        nd_multiplier = 0.5,
                                        od_method = "as-is",
                                        od_multiplier = "null")
  
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", "NA - Not Available", "Text", "Result Value/Unit Cannot Be Estimated From Detection Limit", "Result Value/Unit Estimated from Detection Limit", "Less Than")))
  
  testdat <- TADA_CalculateTotalNP(testdat, daily_agg = "max")

  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in%
                    c("Numeric", "Percentage", "NA - Not Available", "Text", "Less Than", "Result Value/Unit Estimated from Detection Limit", 
                      "Result Value/Unit Cannot Be Estimated From Detection Limit", "TP estimated from one or more subspecies.", "TN estimated from one or more subspecies.")))

  testdat <- TADA_ConvertSpecialChars(testdat, 
                                      col = "TADA.ResultMeasureValue",
                                      clean = TRUE)
  
  # Test to ensure the column is entirely numeric
  expect_true(is.numeric(testdat$TADA.ResultMeasureValue))
  
  # Test to ensure value column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasureValue)))
  
  # Test to ensure unit column does not contain any NA values
  expect_true(!any(is.na(testdat$TADA.ResultMeasure.MeasureUnitCode)))
  
  # Test to make sure remaining result value data types are expected
  # "Result Value/Unit Copied from Detection Limit" should no longer be there
  # NA should not be there... 
  expect_true(all(unique(testdat$TADA.ResultMeasureValueDataTypes.Flag) %in% 
                    c("Numeric", "Result Value/Unit Estimated from Detection Limit", "Less Than", "TP estimated from one or more subspecies.", "TN estimated from one or more subspecies.")))  
})
