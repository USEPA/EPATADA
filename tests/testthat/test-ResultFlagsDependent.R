test_that("No NA's in dependent flag columns", {
  today = Sys.Date()
  twoago = as.character(today-2*365)
  testdat = TADAdataRetrieval(statecode = "UT", startDate = twoago, characteristicName = c("Nitrate","Copper"), sampleMedia = "Water")
  testdat = TADA_ConvertResultUnits(testdat, transform = TRUE)
  
  testdat = suppressWarnings(InvalidFraction(testdat, clean = FALSE, errorsonly=FALSE))
  expect_false(any(is.na(testdat$TADA.SampleFraction.Flag)))
  
  testdat = suppressWarnings(InvalidSpeciation(testdat, clean = "none", errorsonly=FALSE))
  expect_false(any(is.na(testdat$TADA.MethodSpeciation.Flag)))
  
  testdat = suppressWarnings(InvalidResultUnit(testdat, clean = "none", errorsonly=FALSE))
  expect_false(any(is.na(testdat$TADA.ResultUnit.Flag)))
  })



