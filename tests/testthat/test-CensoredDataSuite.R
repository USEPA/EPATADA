test_that("idCensoredData orphans", {
  testdat = TADAdataRetrieval(statecode = "CO",startDate = "2021-01-01",endDate = "2022-01-01",characteristicName = c("Phosphorus","Nitrate"))
  cens.check = idCensoredData(testdat)
  expect_true(all(!is.na(cens.check$TADA.CensoredData.Flag)))
})

test_that("simpleCensoredMethods doesn't drop data", {
  testdat = TADAdataRetrieval(statecode = "KS",startDate = "2021-01-01",endDate = "2022-01-01",characteristicName = c("Phosphorus","Nitrate"))
  cens.check = simpleCensoredMethods(testdat)
  expect_equal(dim(testdat)[1], dim(cens.check)[1])
})

test_that("summarizeCensoredData suggestions complete", {
  testdat = TADAdataRetrieval(statecode = "KS",startDate = "2021-01-01",endDate = "2022-01-01",characteristicName = c("Phosphorus","Nitrate"))
  cens.check = summarizeCensoredData(testdat)
  expect_true(all(!is.na(cens.check$TADA.Censored.Note)))
})