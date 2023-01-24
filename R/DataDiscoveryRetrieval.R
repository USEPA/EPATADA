#' Generate TADA-compatible dataframe from WQP Data
#'
#' Retrieve data from Water Quality Portal (WQP) and generate a TADA-compatible
#' dataframe.
#' 
#' This function will create and/or edit the following columns:
#' TADA.DetectionLimitMeasureValue.Flag
#' DetectionQuantitationLimitMeasure.MeasureValue
#' DetectionLimitMeasureValue.Original
#' ResultMeasureValue.Original
#' TADA.ResultMeasureValue.Flag
#' ResultMeasureValue
#' 
#' Keep in mind that all the query filters for the WQP work as an AND 
#' but within the fields there are ORs. So for example, 
#' characteristics – if you choose pH & DO – it’s an OR. Similarly, if you
#' choose VA and IL, it’s an OR. But the combo of fields are ANDs. 
#' Such as State/VA AND Characteristic/DO". 
#' "Characteristic" and "Characteristic Group" also work as an AND. 
#'  
#' All data cleaning and transformations are done directly to the
#' "ResultMeasureValue" and "DetectionLimitMeasureValue" columns, 
#' however the original "ResultMeasureValue" and "DetectionLimitMeasureValue"
#' columns and values from the WQP are preserved in these new fields, 
#' "ResultMeasureValue.Original" and "DetectionLimitMeasureValue.Original". 
#' Additionally, "TADA.ResultMeasureValue.Flag" and 
#' "TADA.DetectionLimitMeasureValue.Flag" are created to track and changes made
#' to the "ResultMeasureValue" and "DetectionLimitMeasureValue" columns; 
#' and to provide information about the result values that is needed to address
#' censored data later on (i.e., nondetections)
#'  
#' See ?MeasureValueSpecialCharacters and ?autoclean documentation for more information.
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
#' @examples 
#' \dontrun{
#' tada1 <- TADAdataRetrieval(statecode = "WI",
#'                            countycode = "Dane",
#'                            characteristicName = "Phosphorus")
#' }
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
  results.DR <- dataRetrieval::readWQPdata(WQPquery,
                                           dataProfile = "resultPhysChem",
                                           ignore_attributes = TRUE)
  #check if any results are available
  if ((nrow(results.DR) > 0) == FALSE) {
    stop("Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
  }

  narrow.DR <- dataRetrieval::readWQPdata(WQPquery, 
                                          dataProfile = "narrowResult", 
                                          ignore_attributes = TRUE)

  sites.DR <- dataRetrieval::whatWQPsites(WQPquery)

  projects.DR <- dataRetrieval::readWQPdata(WQPquery, 
                                            ignore_attributes = TRUE, 
                                            service = "Project")
  
  TADAprofile = JoinWQPProfiles(FullPhysChem = results.DR,
                          Sites = sites.DR,
                          Narrow = narrow.DR,
                          Projects = projects.DR)
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


#' Large WQP data pulls using dataRetrieval
#' 
#' This function does multiple synchronous data calls to the WQP 
#' (waterqualitydata.us). It uses the WQP summary service to limit the amount 
#' downloaded to only relevant data, and pulls back data from 100 stations at a 
#' time and then joins the data back together and produces a single TADA 
#' compatible dataframe as the output. For large data sets, that can save a lot 
#' of time and ultimately reduce the complexity of subsequent data processing. 
#' Using this function, you will be able to download all data available from all
#' sites in the contiguous United States that is available for the time period,
#' characteristicName, and siteType requested.
#' 
#' Similarly to the TADAdataRetrieval function, this function will create 
#' and/or edit the following columns:
#' TADA.DetectionLimitMeasureValue.Flag
#' DetectionQuantitationLimitMeasure.MeasureValue
#' DetectionLimitMeasureValue.Original
#' ResultMeasureValue.Original
#' TADA.ResultMeasureValue.Flag
#' ResultMeasureValue
#' 
#' All data cleaning and transformations are done directly to the
#' "ResultMeasureValue" and "DetectionLimitMeasureValue" columns, 
#' however the original "ResultMeasureValue" and "DetectionLimitMeasureValue"
#' columns and values from the WQP are preserved in these new fields, 
#' "ResultMeasureValue.Original" and "DetectionLimitMeasureValue.Original". 
#' Additionally, "TADA.ResultMeasureValue.Flag" and 
#' "TADA.DetectionLimitMeasureValue.Flag" are created to track and changes made
#' to the "ResultMeasureValue" and "DetectionLimitMeasureValue" columns; 
#' and to provide information about the result values that is needed to address
#'  censored data later on (i.e., nondetections)
#'  
#' See ?MeasureValueSpecialCharacters and ?autoclean documentation for more information.
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
#' @examples 
#' \dontrun{
#' tada2 <- TADABigdataRetrieval(startDate = "2019-01-01", e
#' ndDate = "2021-12-31", characteristicName = "Temperature, water", 
#' siteType = "Stream")
#' }
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
    ## NOTE: if query brings back no results, function returns empty 
    # dataRetrieval profile, not empty summary
    df_summary = dataRetrieval::readWQPsummary(statecode = state_cd,
                     characteristicName = characteristicName, 
                     siteType = siteType)
    if(nrow(df_summary)>0){
      sites = df_summary %>%
        dplyr::filter(YearSummarized >= startYearLo,
                    YearSummarized <= startYearHi)
      siteid_all = unique(sites$MonitoringLocationIdentifier)
      
      if(length(siteid_all) > 0) {
        print(paste0("Grabbing ",state_nm," data from ",length(siteid_all)," sites."))
        l=length(siteid_all)  #len(sites)
        g=100   #max number of sites pulled per WQP query
        #may want to consider using the total number of records in a given 
        #download group instead, e.g., records must not exceed some maximum 
        #threshold (e.g. USGS uses 250,000 records per group for their pipelines)
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
                                                characteristicName = characteristicName, 
                                                dataProfile = "resultPhysChem",
                                                ignore_attributes = TRUE, 
                                                startDateLo = startDate,
                                                endDateHi = endDate)
        
          narrow.DR <- dataRetrieval::readWQPdata(siteid = sites,
                                                characteristicName = characteristicName,
                                                dataProfile = "narrowResult", 
                                                ignore_attributes = TRUE,
                                                startDateLo = startDate,
                                                endDateHi = endDate)
        
          sites.DR <- dataRetrieval::whatWQPsites(siteid = sites,
                                                characteristicName = characteristicName,
                                                startDateLo = startDate,
                                                endDateHi = endDate)
          
          projects.DR <- dataRetrieval::readWQPdata(siteid = sites,
                                                characteristicName = characteristicName,
                                                service = "Project",
                                                startDateLo = startDate,
                                                endDateHi = endDate)

          joins = JoinWQPProfiles(FullPhysChem = results.DR,
                                  Sites = sites.DR,
                                  Narrow = narrow.DR,
                                  Projects = projects.DR)
          
          # need to specify this or throws error when trying to bind rows. Temporary fix for larger
          # issue where data structure for all columns should be specified.
          joins$ResultMeasureValue = as.character(joins$ResultMeasureValue)
          joins$HorizontalAccuracyMeasure.MeasureValue = as.character(joins$HorizontalAccuracyMeasure.MeasureValue)
          joins$ActivityDepthHeightMeasure.MeasureValue = as.character(joins$ActivityDepthHeightMeasure.MeasureValue)
          joins$DetectionQuantitationLimitMeasure.MeasureValue = as.character(joins$DetectionQuantitationLimitMeasure.MeasureValue)
          if (i==0){
            df = joins }  
          else {
            joins = dplyr::bind_rows(df, joins)
            }
          # print(j)
          # print(k)
        
          i = i+1
        }
      }else{
        joins = data.frame()
        print(paste0(state_nm, " had no data."))}
    
      if(nrow(joins) > 0){
      
      #####
      #need to edit below if temporary rds files do not go away
      #may be able to delete below
      #https://stackoverflow.com/questions/47626331/saving-and-retrieving-temp-files-in-r-packages
      #####
      
      #original
      #saveRDS(df_state, file = paste0(state_nm, "_raw_data.rds"))
      
        tempfilename = paste0(state_nm, "_raw_data.rds")
        file.path(tempdir(), saveRDS(joins, file = paste0("inst/tempdata/", tempfilename)))

        }
      }else{print(paste0(state_nm, " had no data."))}
  }
    all_data <- data.frame()
    stdir = list.files("inst/tempdata/")
    
    for(m in 1:length(stdir)){
      path = paste0("inst/tempdata/",stdir[m])
      allstates_df <- tryCatch({
        #####
        #need to edit line below if rds files do not go away
        #####
      
        #original below
       #readRDS(paste0(state, "_raw_data.rds"))
      
        readRDS(path)
      })
      unlink(path)
    
      if(nrow(allstates_df) > 0){
        all_data <- dplyr::bind_rows(all_data, allstates_df)
      }
    
    } 
    
    # Do not need if date input works in dataRetrieval functions
    finalprofile = all_data %>%
      dplyr::filter(ActivityStartDate <= endDate,
                    ActivityStartDate >= startDate)
    
    finalprofile2 = autoclean(finalprofile)
    #not sure if above is working correctly, thousands of "duplicated" rows are removed
    # you will still need to filter on activity media subdivision now
    
  return(finalprofile2)
}



#' Join WQP Profiles
#'
#' After retrieving multiple result and metadata profiles from the WQP, you 
#' can use this function to join those profiles together into one dataframe.
#' The FullPhysChem data input is required to run this function.
#' 
#' @param FullPhysChem Full physical chemical data profile
#' @param Sites Sites data profile
#' @param Narrow Full biological data profile
#' @param Projects Projects data profile
#'
#' @return TADA-compatible dataframe
#' 
#' @export
#' 
#' @examples 
#' join = TADA::JoinWQPProfiles(FullPhysChem = resultphyschem, Sites = station, Narrow = narrow)
#' 

JoinWQPProfiles <- function(FullPhysChem = "null",
                            Sites = "null",
                            Narrow = "null",
                            Projects = "null"
) {
  
  FullPhysChem.df <- FullPhysChem 
  
  Sites.df <- Sites
  
  Narrow.df <- Narrow

  Projects.df <- Projects
  
  # Join station data to full phys/chem (FullPhysChem.df)
  if(length(Sites.df>1)){
    if(nrow(Sites.df)>0){
      join1 <- FullPhysChem.df %>%
        # join stations to results
        dplyr::left_join(Sites.df, by = "MonitoringLocationIdentifier") %>%
        # remove ".x" suffix from column names
        dplyr::rename_at(dplyr::vars(dplyr::ends_with(".x")), ~ stringr::str_replace(., "\\..$", "")) %>%
        # remove columns with ".y" suffix
        dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    }else{join1 = FullPhysChem.df}
  }else{join1 = FullPhysChem.df}
 
  # Add Speciation column from narrow
  if (length(Narrow.df)>1){
    if(nrow(Narrow.df)>0){
      join2 <- join1 %>%
        dplyr::left_join(dplyr::select(
          Narrow.df, ActivityIdentifier, MonitoringLocationIdentifier,
          CharacteristicName, ResultMeasureValue,
          MethodSpecificationName, OrganizationIdentifier, ResultIdentifier
        ),
        by = c(
          "ActivityIdentifier", "MonitoringLocationIdentifier",
          "CharacteristicName", "ResultMeasureValue", "OrganizationIdentifier",
          "ResultIdentifier"
        )
        )
    }else{join2 = join1}
    
  }else{join2 <- join1}
  
  # Add QAPP columns from project
  if (length(Projects.df)>1){
    if(nrow(Projects.df)>0){
      join3 <- join2 %>%
        dplyr::left_join(dplyr::select(
          Projects.df, OrganizationIdentifier, OrganizationFormalName,
          ProjectIdentifier, ProjectName, ProjectDescriptionText, 
          SamplingDesignTypeCode, QAPPApprovedIndicator, QAPPApprovalAgencyName, 
          ProjectFileUrl, ProjectMonitoringLocationWeightingUrl
        ),
        by = c(
          "OrganizationIdentifier", "OrganizationFormalName",
          "ProjectIdentifier","ProjectName"
        )
        )
    }else{join3 = join2}
    
  }else{join3 <- join2}
  return(join3)
}