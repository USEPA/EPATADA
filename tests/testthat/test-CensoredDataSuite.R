test_that("TADA_IDCensoredData orphans", {
  cens.check <- TADA_DataRetrieval(statecode = "CO", startDate = "2021-01-01", endDate = "2022-01-01", characteristicName = c("Phosphorus", "Nitrate"))
  expect_true(all(!is.na(cens.check$TADA.CensoredData.Flag)))
})

test_that("TADA_SimpleCensoredMethods doesn't drop data", {
  testdat <- TADA_DataRetrieval(statecode = "KS", startDate = "2021-01-01", endDate = "2022-01-01", characteristicName = c("Phosphorus", "Nitrate"))
  cens.check <- TADA_SimpleCensoredMethods(testdat)
  expect_equal(dim(testdat)[1], dim(cens.check)[1])
})


test = TADA_RandomNationalTestingSet()
test1 = TADA_IDCensoredData(test)
test2 = TADA_SimpleCensoredMethods(test1)
test3 = dplyr::select(test2, 
                      ActivityTypeCode, 
                      TADA.ActivityType.Flag,
                      ResultDetectionConditionText,
                      CharacteristicName,
                      TADA.CharacteristicName,
                      ResultMeasureValue,
                      TADA.ResultMeasureValue,
                      ResultMeasure.MeasureUnitCode,
                      TADA.WQXResultUnitConversion,
                      TADA.ResultMeasureValueDataTypes.Flag,
                      TADA.ResultMeasure.MeasureUnitCode,
                      MeasureQualifierCode,
                      TADA.MeasureQualifierCode.Flag, 
                      TADA.MeasureQualifierCode.Def,
                      DetectionQuantitationLimitTypeName,
                      DetectionQuantitationLimitMeasure.MeasureValue,
                      DetectionQuantitationLimitMeasure.MeasureUnitCode,
                      TADA.DetectionQuantitationLimitMeasure.MeasureValue,
                      TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
                      TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag,
                      TADA.CensoredData.Flag,
                      TADA.CensoredMethod,
                      ProviderName
)

write.csv(test3, "test3.csv")
