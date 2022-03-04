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
  
  `%!in%` <- Negate(`%in%`)
  
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  
  if(all(TADA.fields %in% colnames(.data)) == TRUE) {
    TRUE
  } else {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
}


#' TADA Auto Clean
#' 
#' **Placeholder text for function description
#'
#' @param .data TADA dataset
#' @param FlaggedData Boolean argument indicating whether output will have
#' columns appended to flag data or the output will be a cleaned dataset.
#'
#' @return Full TADA dataset with duplicate records and continuous data
#' @export

TADAautoClean <- function(.data, FlaggedData = TRUE){
  
  field.names <- colnames(.data)
  excluded.fields <- c("ActivityIdentifier", "ActivityConductingOrganizationText",
                       "OrganizationFormalName", "OrganizationIdentifier",
                       "ProjectIdentifier", "ResultCommentText", "ActivityCommentText")
  dupe.fields <- field.names[!field.names %in% excluded.fields]
  
  if(TADAprofileCheck(.data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(TADAprofileCheck(.data) == TRUE) {
    
    if(FlaggedData == FALSE) {
      # Remove type 1 duplicate
      clean.data <- .data[!duplicated(.data),]   
      # Remove type 2 duplicate
      clean.data <- clean.data[!duplicated(clean.data[dupe.fields]),]
      # Remove continuous data
      clean.data <- dplyr::filter(clean.data, 
                           ResultDetectionConditionText != "Reported in Raw Data (attached)" |
                             is.na(ResultDetectionConditionText))
      
      return(clean.data)
    }
    
    if(FlaggedData == TRUE) {
      # Remove type 1 duplicate
      flag.data <- .data[!duplicated(.data),]   
      # Flag type 2 duplicate
      flag.data$Duplicate.2 <- as.integer(duplicated(flag.data[dupe.fields]) |
                                            duplicated(flag.data[dupe.fields],
                                                       fromLast = TRUE))
      # Flag continuous data
      # make cont.data data frame
      cont.data <- dplyr::filter(flag.data, 
                          ResultDetectionConditionText == "Reported in Raw Data (attached)")
      # append ContDataFlag column
      cont.data$ContDataFlag <- 1
      # join cont.data to flag.data
      flag.data <- merge(flag.data, cont.data, all.x = TRUE) 
      
      return(flag.data)
    } else {
      stop("FlaggedData argument must be Boolean (TRUE or FALSE)")
    }
  }
}