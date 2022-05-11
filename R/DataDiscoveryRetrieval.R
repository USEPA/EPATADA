#' Generate TADA-Compatible Dataset with WQP Data
#' 
#' Retrieve data from Water Quality Portal (WQP) and output a TADA-compatible 
#' dataset.
#' 
#' @param stateCode Code that identifies a state
#' @param siteType Type of waterbody 
#' @param sampleMedia Type of media
#' @param characteristicName Name of characteristic
#' @param startDate Start Date
#' @param endDate End Date
#' 
#' @return TADA-compatible dataframe
#' @export
#' 

TADAdataRetrieval <- function(stateCode = "US:24",
                              siteType = c("Lake, Reservoir, Impoundment", "Stream"),
                              sampleMedia = c("water", "Water"),
                              characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
                              startDate = "01-01-2019",
                              endDate = "01-01-2022") {
  
  # Set query parameters
  WQPquery <- list(statecode = stateCode,
                   Sitetype = siteType, 
                   Samplemedia = sampleMedia,
                   characteristicName = characteristicName,
                   startDate = startDate, 
                   endDate = endDate)
  # Retrieve all 3 profiles
  results.DR <- dataRetrieval::readWQPdata(WQPquery)
  
  narrow.DR <- dataRetrieval::readWQPdata(WQPquery, dataProfile = "narrowResult")
  
  sites.DR <- dataRetrieval::whatWQPsites(WQPquery)
  
  #projects.DR <- dataRetrieval::readWQPdata(WQPquery, service = "Project")
  
  # Join station data to full phys/chem (results.DR)
  join1 <- results.DR %>%
    # join stations to results
    dplyr::left_join(sites.DR, by = "MonitoringLocationIdentifier") %>% 
    # remove ".x" suffix from column names
    dplyr::rename_at(dplyr::vars(ends_with(".x")), ~stringr::str_replace(., "\\..$","")) %>%
    # remove columns with ".y" suffix
    dplyr::select_at(dplyr::vars(-ends_with(".y")))
  
  # Join Speciation column from narrow to full profile
  TADAprofile <- join1 %>%
    dplyr::left_join(dplyr::select(narrow.DR, ActivityIdentifier, MonitoringLocationIdentifier,
                     CharacteristicName, ResultMeasureValue, 
                     MethodSpecificationName), 
              by = c("ActivityIdentifier", "MonitoringLocationIdentifier",
                     "CharacteristicName", "ResultMeasureValue"))
  
  # run autoclean function
  TADAprofile.clean <- autoclean(TADAprofile)
  
  return(TADAprofile.clean)
}



#' Read in WQP data using WQP web services directly 
#' 
#' Go to the WQP website (https://www.waterqualitydata.us/) and fill out the 
#' advanced query form. Choose the Full Physical Chemical Data Profile, all data
#' sources, and the file format Comma-Separated. When finished, do not hit the 
#' download button. Instead, copy the web service URL located at the bottom of 
#' the page under the header "Result". Use that "Result" web service URL as the
#' input for this function to download data directly into R.  
#' 
#' Note: It may be useful to save the Query URL as well as a comment within 
#' your code. This URL let's you return to the WQP query page with all your 
#' original data filters.
#' 
#' @param webservice WQP Web Service URL
#' 
#' @return WQP Full Physical Chemical Results Data Profile 
#' 
#' @export
#' 

readWQPwebservice <- function(webservice) {
  #read in csv from WQP web service
  if(grepl("zip=yes", webservice)){
    webservice=stringr::str_replace(webservice, "zip=yes", "zip=no")
    return (data.table::fread(toString(webservice)))
    } else {
    return (data.table::fread(webservice))
  }
}

#' autoclean 
#' 
#' Removes complex biological data. Removes non-water media samples. 
#' Removes rows of data that are true duplicates. Capitalizes fields to harmonize
#' data.
#' 
#' Within "BiologicalIntentName", only the allowable values "tissue", "toxicity", 
#' and "NA" apply to non-biological data (the function removes all others).
#' Toxicity and fish tissue data will be kept, but other types of biological 
#' monitoring data will not.
#' 
#' We decided to make some fields uppercase that way they're more compatible 
#' with the WQX validation reference tables and to avoid any issues with 
#' case-sensitivity when joining data. Therefore, we might need to tack on any
#' immediate QA steps (removing true duplicates, converting result values to numeric,
#' capitalizing letters, etc.) to this function, as well as the other retrieval functions.
#' 
#' @param .data TADA dataframe
#' 
#' @return autocleaned TADA data profile 
#' 

autoclean <- function(.data, clean=TRUE){
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("ActivityMediaName", "ResultMeasure.MeasureUnitCode",
           "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use this TADA function. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("ActivityMediaName", "ResultMeasure.MeasureUnitCode",
           "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName") %in% colnames(.data)) == TRUE)
    # capitalize fields (just those used w/ ref tables for now)
    .data$CharacteristicName = toupper(.data$CharacteristicName)
    .data$ResultSampleFractionText = toupper(.data$ResultSampleFractionText)
    .data$MethodSpecificationName = toupper(.data$MethodSpecificationName)
    .data$ResultMeasure.MeasureUnitCode = toupper(.data$ResultMeasure.MeasureUnitCode)
    .data$ActivityMediaName = toupper(.data$ActivityMediaName)
    .data$DetectionQuantitationLimitMeasure.MeasureUnitCode <- 
      toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
    #.data$BiologicalIntentName = toupper(.data$BiologicalIntentName)
    
    # Remove duplicate rows
    .data = .data[!duplicated(.data),]
    
    # Remove complex biological data
    .data = dplyr::filter(.data, ActivityMediaName == "WATER")
    #.data = dplyr::filter(.data, BiologicalIntentName != "TISSUE" | "TOXICITY" | is.na(InvalidCoordinates)== TRUE)
    
    return (.data)
}


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
