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

#' Read in WQP data using WQP web services directly 
#' 
#' Go to the WQP website (https://www.waterqualitydata.us/) and fill out the 
#' advanced query form. When finished, do not download the data. Then choose
#' the Full Physical Chemical Data Profile, all data sources, and the file 
#' format Comma-Separated. Do hit the download button. Instead, copy the 
#' web service URL located at the bottom of the page (under result). Use that 
#' Result web service URL as the input for this function to download data 
#' directly into R.  
#' 
#' Note: It may be useful to save the Query URL as well as a comment within 
#' your code. This URL let's you return to the WQP query page with all your 
#' original data filters.
#'
#' @return WQP Full Physical Chemical Results Data Profile 
#' @export
#' 

readWQPWebService <- function(WebService) {
  #read in csv from WQP web service
  if(grepl("zip=yes", WebService)){
    WebService=str_replace(WebService, "zip=yes", "zip=no")
    data.table::fread(toString(WebService))
    } else {
    data.table::fread(WebService)
  }
}


