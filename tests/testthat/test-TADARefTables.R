# test_that("There aren't any char-frac-spec-unit combos not present in synonym reference",{
#   test <- TADA_RandomTestingData(choose_random_state = TRUE)
#   test1 <- TADA_RunKeyFlagFunctions(test)
#   ref <- TADA_GetSynonymRef()
#   ref_chars <- unique(ref$TADA.CharacteristicName)
#   test_chars <- unique(subset(test1, test1$TADA.CharacteristicName%in%ref_chars)[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName","TADA.ResultMeasure.MeasureUnitCode")])
#   test_chars_ref <- merge(test_chars, ref, all.x = TRUE)
#   new_combos <- subset(test_chars_ref, is.na(test_chars_ref$HarmonizationGroup))[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName","TADA.ResultMeasure.MeasureUnitCode")]
#   if(dim(new_combos)[1]>0){
#     print("New combinations found in random dataset test:")
#     print(new_combos)
#   }
#   expect_true(dim(new_combos)[1]==0)
# })

test_that("No combos were missed in NP key from harmonization table", {
  np <- unique(TADA_GetNutrientSummationRef()[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")])
  np$np <- 1
  harm <- unique(TADA_GetSynonymRef()[, c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName")])
  harm <- subset(harm, harm$TADA.CharacteristicName %in% np$TADA.CharacteristicName)

  combine <- merge(harm, np, all = TRUE)
  orphs <- subset(combine, is.na(combine$np))
  if (dim(orphs)[1] > 0) {
    print("Missing NP combos:")
    print(orphs)
  }
  expect_true(dim(orphs)[1] == 0)
})
