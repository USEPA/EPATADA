TADA_dataframe2 <- TADA_PairForCriteriaCalc(TADA_dataframe)
TADA_dataframe3 <- TADA_FlagContinuousData(TADA_dataframe2)

# does it matter the order in which you run TADA_FlagContinuousData versus TADA_FindPotentialDuplicatesSingleOrg? when flagging and cleaning.
# ex. PH TOTAL NA 2020-09-01 16:45:00 nwisco.01.02002963 USGS-402245105302300
TADA_dataframe4 <- TADA_FindPotentialDuplicatesSingleOrg(TADA_dataframe3)

TADA_dataframe5 <- TADA_FindQCActivities(TADA_dataframe, clean = TRUE)

# Handling of replicate samples with differing resultmeasurevalue when they are not labeled as a duplicate.
TADA_dataframe_test <- TADA_dataframe5 %>%
  dplyr::select(TADA.CharacteristicName,TADA.ResultSampleFractionText,TADA.MethodSpeciationName, 
                ActivityStartDateTime, ActivityIdentifier, MonitoringLocationIdentifier, 
                TADA.ResultMeasureValue, TADA.ResultMeasure.MeasureUnitCode, 
                setdiff(colnames(TADA_dataframe5), colnames(TADA_dataframe))
                ) %>%
  dplyr::filter(TADA.SingleOrgDup.Flag == "Unique") %>%
  dplyr::filter()

TADA_FieldValuesTable(TADA_dataframe_test, field = "MonitoringLocationIdentifier", characteristicName = "ZINC")

.data$agg <- .data[, c("TADA.CharacteristicName", "ActivityIdentifier", "MonitoringLocationIdentifier"]

TADA_ParamUseStandardsRef$TADA.MethodSpeciationName <- as.character(TADA_ParamUseStandardsRef$TADA.MethodSpeciationName)

# TADA_StandardsExceedance <- TADA_dataframe_test %>%
#   #dplyr::left_join(TADA_dataframe_test, by = c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName")) %>%
#   dplyr::group_by(TADA_dataframe_test[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName", "MonitoringLocationIdentifier")]) %>%
#   dplyr::summarise(
#     n_records = length(TADA.ResultMeasureValue),
#     #n_continuous_records = length(TADA.ResultMeasureValue)
#     .groups = "drop"
#   )

TADA_data_duration_aggregated <- TADA_dataframe_test %>%
  #dplyr::left_join(TADA_dataframe_test, by = c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName")) %>%
  dplyr::group_by(TADA_dataframe_test[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName", "MonitoringLocationIdentifier", "ActivityStartDateTime", "ActivityIdentifier", "TADA.ContinuousData.Flag")]) %>%
  dplyr::summarise(
    n_discrete_records = length(TADA.ResultMeasureValue),
    #n_aggegated_records = length(unique(ActivityIdentifier)),
    average_ResultMeasureValue_records = mean(TADA.ResultMeasureValue),
    #n_continuous_records = length(TADA.ResultMeasureValue)
    .groups = "drop"
  )

TADA_StandardsExceedance <- TADA_dataframe_test %>%
  #dplyr::left_join(TADA_dataframe_test, by = c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName")) %>%
  dplyr::group_by(TADA_dataframe_test[,c("TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpeciationName", "MonitoringLocationIdentifier")]) %>%
  dplyr::summarise(
    n_discrete_records = length(TADA.ResultMeasureValue),
    n_aggregated_records = length(unique(ActivityIdentifier)), # this will include discrete records
    .groups = "drop"
  ) %>%
  dplyr::mutate(n_discrete_count = sum(.$n_discrete_records)) %>%
  dplyr::mutate(n_aggregated_count = sum(.$n_aggregated_records))

TADA
