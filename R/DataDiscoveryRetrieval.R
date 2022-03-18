#' TADA Profile Check
#' 
#' This function checks if the column names in a dataframe include the TADA
#' profile fields. It is used at the beginning of TADA functions to ensure the
#' input data frame is suitable (i.e. is either the full physical/chemical
#' results profile downloaded from WQP or the TADA profile template downloaded
#' from the EPA TADA webpage.)
#'
#' @param .data A dataframe
#'
#' @return Boolean result indicating whether or not the input dataframe contains
#' all of the TADA profile fields.
#'

TADAprofileCheck <- function(.data){
  
  TADA.fields <- c("OrganizationIdentifier", "OrganizationFormalName",
                   "ActivityIdentifier", "ActivityTypeCode",
                   "ActivityMediaName", "ActivityMediaSubdivisionName", 
                   "ActivityStartDate", "ActivityStartTime.Time", 
                   "ActivityStartTime.TimeZoneCode", "ActivityEndDate", 
                   "ActivityEndTime.Time", "ActivityEndTime.TimeZoneCode", 
                   "ActivityDepthHeightMeasure.MeasureValue", "ActivityDepthHeightMeasure.MeasureUnitCode", 
                   "ActivityDepthAltitudeReferencePointText", "ActivityTopDepthHeightMeasure.MeasureValue", 
                   "ActivityTopDepthHeightMeasure.MeasureUnitCode", "ActivityBottomDepthHeightMeasure.MeasureValue",
                   "ActivityBottomDepthHeightMeasure.MeasureUnitCode", "ProjectIdentifier", 
                   "ActivityConductingOrganizationText", "MonitoringLocationIdentifier", 
                   "ActivityCommentText", "SampleAquifer", 
                   "HydrologicCondition", "HydrologicEvent", 
                   "SampleCollectionMethod.MethodIdentifier", "SampleCollectionMethod.MethodIdentifierContext",
                   "SampleCollectionMethod.MethodName", "SampleCollectionEquipmentName",
                   "ResultDetectionConditionText", "CharacteristicName", 
                   "ResultSampleFractionText", "ResultMeasureValue", 
                   "ResultMeasure.MeasureUnitCode", "MeasureQualifierCode", 
                   "ResultStatusIdentifier", "StatisticalBaseCode", 
                   "ResultValueTypeName", "ResultWeightBasisText", 
                   "ResultTimeBasisText", "ResultTemperatureBasisText", 
                   "ResultParticleSizeBasisText", "PrecisionValue",
                   "ResultCommentText", "USGSPCode", 
                   "ResultDepthHeightMeasure.MeasureValue", "ResultDepthHeightMeasure.MeasureUnitCode",
                   "ResultDepthAltitudeReferencePointText", "SubjectTaxonomicName",
                   "SampleTissueAnatomyName", "ResultAnalyticalMethod.MethodIdentifier",
                   "ResultAnalyticalMethod.MethodIdentifierContext", "ResultAnalyticalMethod.MethodName",
                   "MethodDescriptionText", "LaboratoryName",
                   "AnalysisStartDate", "ResultLaboratoryCommentText",
                   "DetectionQuantitationLimitTypeName", "DetectionQuantitationLimitMeasure.MeasureValue",
                   "DetectionQuantitationLimitMeasure.MeasureUnitCode", "PreparationStartDate",
                   "ProviderName", "ActivityStartDateTime", "ActivityEndDateTime")
  
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  
  if(all(TADA.fields %in% colnames(.data)) == TRUE) {
    TRUE
  } else {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
}

#' Read in WQP data
#' 
#' *placeholder for function description
#'
#'
#' @return 
#' @export
#' 

readWQPdataTADA <- function() {
  # change class of ResultMeasureValue to numeric (it's read into R as character)
}