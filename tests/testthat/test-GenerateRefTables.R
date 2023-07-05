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
