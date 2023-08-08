# Test that new combinations of detection condition have not been added to domain tables
test_that("Is TADA_GetDetCondRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old = utils::read.csv(system.file("extdata", "WQXResultDetectionConditionRef.csv", package = "TADA"))
  old_latedate = max(as.Date(old$Last.Change.Date,"%m/%d/%Y"))
  ref = TADA_GetDetCondRef()
  new_latedate = max(as.Date(ref$Last.Change.Date,"%m/%d/%Y"))
  
  expect_true(old_latedate==new_latedate)
})

# Test that new combinations of detection condition have not been added to domain tables
test_that("Is TADA_GetDetLimitRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old = utils::read.csv(system.file("extdata", "WQXDetectionQuantitationLimitTypeRef.csv", package = "TADA"))
  old_latedate = max(as.Date(old$Last.Change.Date,"%m/%d/%Y"))
  ref = TADA_GetDetLimitRef()
  new_latedate = max(as.Date(ref$Last.Change.Date,"%m/%d/%Y"))
  
  expect_true(old_latedate==new_latedate)
})

# Test that new QC conditions have not been added to ActivityType domain table
test_that("Is TADA_GetActivityTypeRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old = utils::read.csv(system.file("extdata", "WQXActivityTypeRef.csv", package = "TADA"))
  old_latedate = max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref = TADA_GetActivityTypeRef()
  new_latedate = max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))
  
  expect_true(old_latedate==new_latedate)
})



# Test that new codes have not been added to ResultMeasureQualifier domain table
test_that("Is TADA_GetMeasureQualifierCodeRef up to date?", {
  # Check for any new domain values that may not follow above logic and warn.
  old = utils::read.csv(system.file("extdata", "WQXMeasureQualifierCodeRef.csv", package = "TADA"))
  old_latedate = max(as.Date(old$Last.Change.Date, "%m/%d/%Y"))
  ref = TADA_GetMeasureQualifierCodeRef()
  new_latedate = max(as.Date(ref$Last.Change.Date, "%m/%d/%Y"))
  
  expect_true(old_latedate==new_latedate)
  
})  

# test_that("There aren't any char-frac-spec-unit combos not present in synonym reference",{
#   test = TADA_RandomTestingSet()
#   test1 = TADA_RunKeyFlagFunctions(test)
#   ref = TADA_GetSynonymRef()
#   ref_chars = unique(ref$TADA.CharacteristicName)
#   test_chars = unique(subset(test1, test1$TADA.CharacteristicName%in%ref_chars)[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpecificationName","TADA.ResultMeasure.MeasureUnitCode")])
#   test_chars_ref = merge(test_chars, ref, all.x = TRUE)
#   new_combos = subset(test_chars_ref, is.na(test_chars_ref$HarmonizationGroup))[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpecificationName","TADA.ResultMeasure.MeasureUnitCode")]
#   if(dim(new_combos)[1]>0){
#     print("New combinations found in random dataset test:")
#     print(new_combos)
#   }
#   expect_true(dim(new_combos)[1]==0)
# })

test_that("No combos were missed in NP key from harmonization table",{
  np = unique(TADA_GetNutrientSummationRef()[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpecificationName")])
  np$np = 1
  harm = unique(TADA_GetSynonymRef()[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpecificationName")])
  harm = subset(harm, harm$TADA.CharacteristicName%in%np$TADA.CharacteristicName)

  combine = merge(harm, np, all = TRUE)
  orphs = subset(combine, is.na(combine$np))
  if(dim(orphs)[1]>0){
    print("Missing NP combos:")
    print(orphs)
  }
  expect_true(dim(orphs)[1]==0)

})
