#' Generate TADA-Compatible Dataset with WQP Data
#'
#' Retrieve data from Water Quality Portal (WQP) and output a TADA-compatible
#' dataset.
#' 
#' Keep in mind that all the query filters for the WQP work as an AND 
#' but within the fields there are ORs. So for example, 
#' characteristics – if you choose pH & DO – it’s an OR. Similarly, if you
#' choose VA and IL, it’s an OR. But the combo of fields are ANDs. 
#' Such as State/VA AND Characteristic/DO". 
#' "Characteristic" and "Characteristic Group" also work as an AND. 
#'
#' @param statecode Code that identifies a state
#' @param startDate Start Date
#' @param countycode Code that identifies a county 
#' @param siteid Unique monitoring station identifier
#' @param siteType Type of waterbody
#' @param characteristicName Name of parameter
#' @param ActivityMediaName Sampling substrate such as water, air, or sediment
#' @param endDate End Date
#'
#' @return TADA-compatible dataframe
#' 
#' @export
#'

TADAdataRetrieval <- function(statecode = "null",
                              startDate = "null",
                              countycode = "null", 
                              siteid = "null",
                              siteType = "null",
                              characteristicName = "null",
                              ActivityMediaName = "null", 
                              endDate = "null"
                              ) {

  # Set query parameters
  WQPquery <- list()
  if (length(statecode)>1) {
        WQPquery <- c(WQPquery, statecode = list(statecode)) 
  } else if (statecode != "null") {
        WQPquery <- c(WQPquery, statecode = statecode)
  }
  
  if (length(startDate)>1) {
    WQPquery <- c(WQPquery, startDate = list(startDate)) 
  } else if (startDate != "null") {
    WQPquery <- c(WQPquery, startDate = startDate)
  }
  
  if (length(countycode)>1) {
    WQPquery <- c(WQPquery, countycode = list(countycode)) 
  } else if (countycode != "null") {
    WQPquery <- c(WQPquery, countycode = countycode)
  }
  
  if (length(siteid)>1) {
    WQPquery <- c(WQPquery, siteid = list(siteid)) 
  } else if (siteid != "null") {
    WQPquery <- c(WQPquery, siteid = siteid)
  }
  
  if (length(siteType)>1) {
    WQPquery <- c(WQPquery, siteType = list(siteType)) 
  } else if (siteType != "null") {
    WQPquery <- c(WQPquery, siteType = siteType)
  }
  
  if (length(characteristicName)>1) {
    WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
  } else if (characteristicName != "null") {
    WQPquery <- c(WQPquery, characteristicName = characteristicName)
  }
  
  if (length(ActivityMediaName)>1) {
    WQPquery <- c(WQPquery, ActivityMediaName = list(ActivityMediaName)) 
  } else if (ActivityMediaName != "null") {
    WQPquery <- c(WQPquery, ActivityMediaName = ActivityMediaName)
  }
  
  if (length(endDate)>1) {
    WQPquery <- c(WQPquery, endDate = list(endDate)) 
  } else if (endDate != "null") {
    WQPquery <- c(WQPquery, endDate = endDate)
  }

  # Retrieve all 3 profiles
  results.DR <- dataRetrieval::readWQPdata(WQPquery)

  narrow.DR <- dataRetrieval::readWQPdata(WQPquery, dataProfile = "narrowResult")

  sites.DR <- dataRetrieval::whatWQPsites(WQPquery)

  projects.DR <- dataRetrieval::readWQPdata(WQPquery, service = "Project")

  # Join station data to full phys/chem (results.DR)
  join1 <- results.DR %>%
    # join stations to results
    dplyr::left_join(sites.DR, by = "MonitoringLocationIdentifier") %>%
    # remove ".x" suffix from column names
    dplyr::rename_at(dplyr::vars(ends_with(".x")), ~ stringr::str_replace(., "\\..$", "")) %>%
    # remove columns with ".y" suffix
    dplyr::select_at(dplyr::vars(-ends_with(".y")))

  # Join Speciation column from narrow to full profile
  join2 <- join1 %>%
    dplyr::left_join(dplyr::select(
      narrow.DR, ActivityIdentifier, MonitoringLocationIdentifier,
      CharacteristicName, ResultMeasureValue,
      MethodSpecificationName
    ),
    by = c(
      "ActivityIdentifier", "MonitoringLocationIdentifier",
      "CharacteristicName", "ResultMeasureValue"
    )
    )
  
  # Join QAPP columns from project to full profile
  TADAprofile <- join2 %>%
    dplyr::left_join(dplyr::select(
      projects.DR, OrganizationIdentifier, OrganizationFormalName,
      ProjectIdentifier, ProjectName, ProjectDescriptionText, 
      SamplingDesignTypeCode, QAPPApprovedIndicator, QAPPApprovalAgencyName, 
      ProjectFileUrl, ProjectMonitoringLocationWeightingUrl
    ),
    by = c(
      "OrganizationIdentifier", "OrganizationFormalName",
      "ProjectIdentifier"
    )
    )

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
#' @param webservice WQP Web Service URL, entered within quotes "url"
#'
#' @return WQP Full Physical Chemical Results Data Profile
#'
#' @export
#'

readWQPwebservice <- function(webservice) {
  #consider function dataRetrieval::getWebServiceData
  # read in csv from WQP web service
  if (grepl("zip=yes", webservice)) {
    webservice <- stringr::str_replace(webservice, "zip=yes", "zip=no")
    return(data.table::fread(toString(webservice)))
    #update when we switch to WQX 3.0
    #return(autoclean(data.table::fread(toString(webservice))))
  } else {
    return(data.table::fread(webservice))
    #update when we switch to WQX 3.0
    #return(autoclean(data.table::fread(webservice)))
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

autoclean <- function(.data) {
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if ("data.frame" %in% class(.data) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if (all(c(
    "ActivityMediaName", "ResultMeasure.MeasureUnitCode",
    "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName"
  ) %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use this TADA function. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if (all(c(
    "ActivityMediaName", "ResultMeasure.MeasureUnitCode",
    "CharacteristicName", "ResultSampleFractionText", "MethodSpecificationName"
  ) %in% colnames(.data)) == TRUE) {
    # capitalize fields (just those used w/ ref tables for now)
    .data$CharacteristicName <- toupper(.data$CharacteristicName)
  }
  .data$ResultSampleFractionText <- toupper(.data$ResultSampleFractionText)
  .data$MethodSpecificationName <- toupper(.data$MethodSpecificationName)
  .data$ResultMeasure.MeasureUnitCode <- toupper(.data$ResultMeasure.MeasureUnitCode)
  .data$ActivityMediaName <- toupper(.data$ActivityMediaName)
  .data$DetectionQuantitationLimitMeasure.MeasureUnitCode <-
    toupper(.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
  # .data$BiologicalIntentName = toupper(.data$BiologicalIntentName)

  # Remove duplicate rows
  .data <- .data[!duplicated(.data), ]

  # Remove complex biological data
  .data <- dplyr::filter(.data, ActivityMediaName == "WATER")
  # .data = dplyr::filter(.data, BiologicalIntentName != "TISSUE" | "TOXICITY" | is.na(InvalidCoordinates)== TRUE)

  # run MeasureValueSpecialCharacters function
  .data <- MeasureValueSpecialCharacters(.data)

  return(.data)
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

TADAprofileCheck <- function(.data) {
  TADA.fields <- c(
    "OrganizationIdentifier", "OrganizationFormalName",
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
    "ProviderName", "ActivityStartDateTime", "ActivityEndDateTime"
  )

  if (("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }

  if (all(TADA.fields %in% colnames(.data)) == TRUE) {
    TRUE
  } else {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
}



#' Large WQP data pulls using dataRetrieval for all data from all sites in the contiguous United States.
#'  
#' This function uses the WQP summary service to limit the amount
#' downloaded to only relevant data. For large data sets, that can 
#' save a lot of time and ultimately reduce the complexity of subsequent
#' data processing. 
#' 
#' This function will join data from multiple WQP profiles and output a 
#' TADA-compatible dataset.
#'
#' @param startDate Start Date YYYY-MM-DD format, for example, "1995-01-01"
#' @param endDate end date in YYYY-MM-DD format, for example, "2020-12-31"
#' @param characteristicName Name of water quality parameter
#' @param siteType Name of water body type (e.g., "Stream", "Lake, Reservoir, Impoundment")
#' 
#' @return TADA-compatible dataframe
#' 
#' @export
#'

TADABigdataRetrieval <- function(startDate = "null",
                              endDate = "null",
                              characteristicName = "null", 
                              siteType = "null"
) {
  
  startDate_Low = lubridate::ymd(startDate)
  startYearLo = lubridate::year(startDate_Low)
  
  endDate_High = lubridate::ymd(endDate)
  startYearHi = lubridate::year(endDate_High)
  
  if (length(characteristicName)>1) {
    characteristicName = list(characteristicName) 
  } else if (characteristicName != "null") {
    characteristicName = characteristicName
  }

  if (length(siteType)>1) {
    siteType = list(siteType)
  } else if (siteType != "null") {
    siteType = siteType
  }
  
  state_cd_cont = utils::read.csv(file = "inst/extdata/statecode.csv")
  
  for(i in seq_len(nrow(state_cd_cont))){
    
    state_cd = as.numeric(state_cd_cont$STATE[i])
    state_nm = state_cd_cont$STUSAB[i]
    
    df_summary = dataRetrieval::readWQPsummary(statecode = state_cd,
                     characteristicName = characteristicName, 
                     siteType = siteType, 
                     startDate = startDate)
    
    sites = df_summary %>%
      dplyr::filter(YearSummarized >= startYearLo,
                    YearSummarized <= startYearHi)
    
    siteid_all = unique(sites$MonitoringLocationIdentifier)
    
    if(length(siteid_all) > 0) {
      
      l=length(siteid_all)  #len(sites)
      g=250   #grouping size
      nl=ceiling(l/g) #number of queries
      
      i=0
      j=0
      k=0
      
      while (i < nl) {
        
        j=i*g
        k=j+g-1
        
        if (k>l){k=l}
        sites=siteid_all[j:k]
        
        results.DR <- dataRetrieval::readWQPdata(siteid = sites,
                                               characteristicName = characteristicName) 
                                               #startDate = startDate)
        
        narrow.DR <- dataRetrieval::readWQPdata(siteid = sites,
                                                characteristicName = characteristicName,
                                                dataProfile = "narrowResult")
        
        sites.DR <- dataRetrieval::whatWQPsites(siteid = sites,
                                                characteristicName = characteristicName)

        #projects.DR <- dataRetrieval::readWQPdata(siteid = siteid,
                                                  #characteristicName = characteristicName,
                                                  #service = "Project")
        
        #})
        
        # Join station data to full phys/chem (results.DR)
        join1 <- results.DR %>%
        # join stations to results
        dplyr::left_join(sites.DR, by = "MonitoringLocationIdentifier") %>%
        # remove ".x" suffix from column names
        dplyr::rename_at(dplyr::vars(ends_with(".x")), ~ stringr::str_replace(., "\\..$", "")) %>%
        # remove columns with ".y" suffix
        dplyr::select_at(dplyr::vars(-ends_with(".y")))
    
        # Join Speciation column from narrow to full profile
        join2 <- join1 %>%
        dplyr::left_join(dplyr::select(
        narrow.DR, ActivityIdentifier, MonitoringLocationIdentifier,
        CharacteristicName, ResultMeasureValue,
        MethodSpecificationName
        ),
        by = c(
          "ActivityIdentifier", "MonitoringLocationIdentifier",
          "CharacteristicName", "ResultMeasureValue"
        )
        )
        
        join2$ResultMeasureValue = as.character(join2$ResultMeasureValue)
        
        if (i==0){
          df = join2 }  
        else {
          join2 = rbind(df, join2)
          }
        print(j)
        print(k)
        
        i = i+1
      }
    }
    
    if(nrow(join2) > 0){
      
      #####
      #need to edit below if temporary rds files do not go away
      #may be able to delete below
      #https://stackoverflow.com/questions/47626331/saving-and-retrieving-temp-files-in-r-packages
      #####
      
      #original
      #saveRDS(df_state, file = paste0(state_nm, "_raw_data.rds"))
      
      tempfilename = paste0(state_nm, "_raw_data.rds")
      file.path(tempdir(), saveRDS(join2, file = paste0("inst/tempdata/", tempfilename)))

    }
  }
    all_data <- data.frame()
    for(state in state_cd_cont$STUSAB){
      allstates_df <- tryCatch({
        #####
        #need to edit line below if rds files do not go away
        #####
      
        #original below
       #readRDS(paste0(state, "_raw_data.rds"))
      
        readRDS(paste0("inst/tempdata/", tempfilename))
      })
    
      if(nrow(allstates_df) > 0){
        all_data <- bind_rows(all_data, allstates_df)
      }
    
    } 
    
    finalprofile = all_data %>%
      dplyr::filter(ActivityStartDate <= endDate, 
                    ActivityStartDate >= startDate)
    
    finalprofile2 = autoclean(finalprofile)
    #not sure if above is working correctly, thousands of "duplicated" rows are removed
    # you will still need to filter on activity media subdivision now
    
  return(finalprofile2)
}
