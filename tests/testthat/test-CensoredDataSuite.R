test_that("idCensoredData orphans", {
  cens.check = TADA_DataRetrieval(statecode = "CO",startDate = "2021-01-01",endDate = "2022-01-01",characteristicName = c("Phosphorus","Nitrate"))
  expect_true(all(!is.na(cens.check$TADA.CensoredData.Flag)))
})

test_that("simpleCensoredMethods doesn't drop data", {
  testdat = TADA_DataRetrieval(statecode = "KS",startDate = "2021-01-01",endDate = "2022-01-01",characteristicName = c("Phosphorus","Nitrate"))
  cens.check = simpleCensoredMethods(testdat)
  expect_equal(dim(testdat)[1], dim(cens.check)[1])
})

test_that("summarizeCensoredData suggestions complete", {
  testdat = TADA_DataRetrieval(statecode = "KS",startDate = "2021-01-01",endDate = "2022-01-01",characteristicName = c("Phosphorus","Nitrate"))
  cens.check = summarizeCensoredData(testdat)
  expect_true(all(!is.na(cens.check$TADA.Censored.Note)))
})