test_that("No NA's in dependent flag columns", {
  today = Sys.Date()
  twoago = as.character(today-2*365)
  testdat = TADA_DataRetrieval(statecode = "UT", startDate = twoago, characteristicName = c("Nitrate","Copper"), sampleMedia = "Water")
  testdat = TADA_ConvertResultUnits(testdat, transform = TRUE)
  
  testdat = suppressWarnings(TADA_FlagFraction(testdat, clean = FALSE, flaggedonly=FALSE))
  expect_false(any(is.na(testdat$TADA.SampleFraction.Flag)))
  
  testdat = suppressWarnings(TADA_FlagSpeciation(testdat, clean = "none", flaggedonly=FALSE))
  expect_false(any(is.na(testdat$TADA.MethodSpeciation.Flag)))
  
  testdat = suppressWarnings(TADA_FlagResultUnit(testdat, clean = "none", flaggedonly=FALSE))
  expect_false(any(is.na(testdat$TADA.ResultUnit.Flag)))
  })

