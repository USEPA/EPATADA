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

# Copy detection limit value and unit to TADA Result Measure Value and Unit columns
# this first row copies all over when TADA.DetectionQuantitationLimitMeasure.MeasureValue is not NA and the 
# TADA.ResultMeasureValueDataTypes.Flag is "NA - Not Available"
# Imp note: TADA result values are NA for text and other values (coerced) even though they are not 
# NA in the original result value
test$TADA.ResultMeasureValue <- ifelse(
  !is.na(test$TADA.DetectionQuantitationLimitMeasure.MeasureValue)
  & test$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available",
  test$TADA.DetectionQuantitationLimitMeasure.MeasureValue, 
  test$TADA.ResultMeasureValue)

# this does the same as above for the units
test$TADA.ResultMeasure.MeasureUnitCode <- ifelse(
  !is.na(test$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
  & test$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available", 
  test$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode, 
  test$TADA.ResultMeasure.MeasureUnitCode)

# this updates the TADA.ResultMeasureValueDataTypes.Flag 
test$TADA.ResultMeasureValueDataTypes.Flag <- ifelse(
  test$TADA.ResultMeasureValueDataTypes.Flag == "NA - Not Available"
  & !is.na(test$TADA.DetectionQuantitationLimitMeasure.MeasureValue),
  "Result Value/Unit Copied from Detection Limit",
  test$TADA.ResultMeasureValueDataTypes.Flag)

# this copies det lim result value and unit over to TADA result value and unit 
# when the result value is TEXT but there is a specific text value that indicates 
# the result is censored (BPQL, BDL, ND)
# and the TADA.DetectionQuantitationLimitMeasure.MeasureValue provided
# if more are added, they need to be included below as well (line 194)
test$TADA.ResultMeasureValue <- ifelse(
  is.na(test$TADA.ResultMeasureValue)
  & !is.na(test$TADA.DetectionQuantitationLimitMeasure.MeasureValue)
  & (test$ResultMeasureValue == "BPQL" |
    test$ResultMeasureValue == "BDL" |
    test$ResultMeasureValue == "ND") ,
  test$TADA.DetectionQuantitationLimitMeasure.MeasureValue, 
  test$TADA.ResultMeasureValue)

# this does the same as above for the units
test$TADA.ResultMeasure.MeasureUnitCode <- ifelse(
  !is.na(test$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)
  & (test$ResultMeasureValue == "BPQL" |
    test$ResultMeasureValue == "BDL" |
    test$ResultMeasureValue == "ND") , 
  test$TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode, 
  test$TADA.ResultMeasure.MeasureUnitCode)

test$TADA.ResultMeasureValueDataTypes.Flag <- ifelse(
  test$TADA.ResultMeasureValueDataTypes.Flag == "Text" &
    (test$ResultMeasureValue == "BPQL" |
    test$ResultMeasureValue == "BDL" |
    test$ResultMeasureValue == "ND") & 
    !is.na(test$TADA.DetectionQuantitationLimitMeasure.MeasureValue),
  "Result Value/Unit Copied from Detection Limit", 
  test$TADA.ResultMeasureValueDataTypes.Flag)

# Result Measure Qualifier codes to identify censored samples. 
if (!"TADA.MeasureQualifierCode.Flag" %in% names(test)) {
  data_mq_flag <- TADA_FlagMeasureQualifierCode(test)
} else {
  data_mq_flag <- test
}

## Identify censored data using TADA.ResultMeasureValueDataTypes.Flag and TADA.MeasureQualifierCode.Flag
cens_rm_flag <- data_mq_flag %>% dplyr::filter(TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Copied from Detection Limit")
cens_mq_flag <- data_mq_flag %>% dplyr::filter(TADA.MeasureQualifierCode.Flag %in% c("Non-Detect", "Over-Detect")) %>%
  dplyr::filter(!ResultIdentifier %in% cens_rm_flag$ResultIdentifier)
cens <- cens_rm_flag %>%
  rbind(cens_mq_flag)
not_cens <- data_mq_flag %>% dplyr::filter(!ResultIdentifier %in% cens$ResultIdentifier)
not_cens$TADA.CensoredData.Flag <- "Uncensored"

rm(cens_rm_flag, cens_mq_flag, data_mq_flag)


blah = dplyr::select(test, ActivityTypeCode, TADA.ActivityType.Flag,ResultDetectionConditionText,
                     CharacteristicName,
                     TADA.CharacteristicName,
                     ResultMeasureValue,
                     TADA.ResultMeasureValue,
                     ResultMeasure.MeasureUnitCode,
                     TADA.WQXResultUnitConversion,
                     TADA.ResultMeasureValueDataTypes.Flag,
                     TADA.ResultMeasure.MeasureUnitCode,
                     DetectionQuantitationLimitTypeName,
                     DetectionQuantitationLimitMeasure.MeasureValue,
                     DetectionQuantitationLimitMeasure.MeasureUnitCode,
                     TADA.DetectionQuantitationLimitMeasure.MeasureValue,
                     TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
                     #TADA.MeasureQualifierCode.Flag,
                     ProviderName)
write.csv(blah, "blah.csv")

test1 = TADA_IDCensoredData(test)
blah2 = dplyr::select(test1, 
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
                      ProviderName
)
write.csv(blah2, "blah2.csv")

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
