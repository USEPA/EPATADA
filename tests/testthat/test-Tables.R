test_that("TADA_Stats suggestions complete", {
  testdat <- TADA_DataRetrieval(statecode = "KS", startDate = "2021-01-01", endDate = "2022-01-01", characteristicName = c("Phosphorus", "Nitrate"))
  check <- TADA_Stats(testdat)
  expect_true(all(!is.na(check$ND_Estimation_Method)))
})
