test_that("No NA's in dependent flag columns", {
  today = Sys.Date()
  twoago = as.character(today-2*365)
  testdat = TADAdataRetrieval(statecode = "UT", startDate = twoago, characteristicName = c("Nitrate","Copper"), sampleMedia = "Water")
  testdat = ConvertResultUnits(testdat, transform = TRUE)
  
  testdat = InvalidFraction(testdat, clean = FALSE, errorsonly=FALSE)
  expect_false(any(is.na(testdat$TADA.SampleFraction.Flag)))
  
  testdat = InvalidSpeciation(testdat, clean = "none", errorsonly=FALSE)
  expect_false(any(is.na(testdat$TADA.MethodSpeciation.Flag)))
  
  testdat = InvalidResultUnit(testdat, clean = "none", errorsonly=FALSE)
  expect_false(any(is.na(testdat$TADA.ResultUnit.Flag)))
  })



