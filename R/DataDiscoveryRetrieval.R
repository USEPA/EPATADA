#' Generate TADA-compatible dataframe from WQP Data
#'
#' Retrieve data from Water Quality Portal (WQP) and generate a TADA-compatible
#' dataframe. Note that the inputs (e.g. project, organization, siteType) with the 
#' exceptions of endDate and startDate match the web service call format from the
#' online WQP GUI. endDate and startDate match the format suggested in USGS's 
#' dataRetrieval package (endDate = "YYYY-MM-DD"), which is a more familiar date 
#' format for R users than the WQP GUI's endDateHi = "MM-DD-YYYY".
#'  
#' 
#' This function will create and/or edit the following columns:
#' TADA.DetectionLimitMeasureValue.Flag
#' DetectionQuantitationLimitMeasure.MeasureValue
#' DetectionLimitMeasureValue.Original
#' ResultMeasureValue.Original
#' TADA.ResultMeasureValue.Flag
#' ResultMeasureValue
#' 
#' Keep in mind that all the query filters for the WQP work as an AND, 
#' but within the fields there are ORs. For example, within
#' characteristicName – if you enter, c("pH", "Dissolved oxygen (DO)), – it’s an OR. 
#' Meaning that you will be back "pH" or "Dissolved oxygen (DO)" data. Similarly, if you
#' enter c("Virginia", "Illinois"), it’s an OR. But the combo of fields are ANDs. 
#' Such as State/Virginia AND Characteristic/Dissolved oxygen (DO)". Meaning, you 
#' will get get back only Dissolved oxygen (DO) from Virginia.
#' characteristicName and Characteristic Group also work as an AND, therefore the 
#' characteristicName must fall within the Characteristic Group when both are entered.
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
#' Users can reference the \href{https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads}{WQX domain tables}
#' to find allowable values for queries, e.g., reference the WQX domain table to find countycode and statecode: https://cdx.epa.gov/wqx/download/DomainValues/County_CSV.zip
#' Alternatively, you can use the WQP services to find areas where data is available in the US: https://www.waterqualitydata.us/Codes/countycode
#'  
#' See ?MeasureValueSpecialCharacters and ?autoclean documentation for more information.
#' 
#' @param statecode Code that identifies a state
#' @param startDate Start Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param countycode Code that identifies a county 
#' @param siteid Unique monitoring station identifier
#' @param siteType Type of waterbody
#' @param characteristicName Name of parameter
#' @param sampleMedia Sampling substrate such as water, air, or sediment
#' @param project A string of letters and/or numbers (some additional characters also possible) used to signify a project with data in the Water Quality Portal
#' @param organization A string of letters and/or numbers (some additional characters also possible) used to signify an organization with data in the Water Quality Portal
#' @param endDate End Date string in the format YYYY-MM-DD
#' @param applyautoclean Logical, defaults to TRUE. Applies TADA's autoclean function on the returned data profile.
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
#' 
#' tada2 <- TADAdataRetrieval(project = "Anchorage Bacteria 20-21")
#' 
#' tada3 <- TADAdataRetrieval(statecode = "UT", 
#'                            characteristicName = c("Ammonia", "Nitrate", "Nitrogen"), 
#'                            startDate = "2020-10-01")
#' 
#' tada4 <- TADAdataRetrieval(statecode = "SC", countycode  = "Abbeville")
#' # countycode queries require a statecode
#' tada5 <- TADAdataRetrieval(countycode = "US:02:020")
#' }
#'

TADAdataRetrieval <- function(statecode = "null",
                              startDate = "null",
                              countycode = "null", 
                              siteid = "null",
                              siteType = "null",
                              characteristicName = "null",
                              sampleMedia = "null",
                              project = "null",
                              organization = "null",
                              endDate = "null",
                              applyautoclean = TRUE
                              ) {

  # Set query parameters
  WQPquery <- list()
  if (length(statecode)>1) {
        WQPquery <- c(WQPquery, statecode = list(statecode)) 
  } else if (statecode != "null") {
        WQPquery <- c(WQPquery, statecode = statecode)
  }
  
  if (length(startDate)>1) {
    if(is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], orders = "ymd")))){
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, startDate = list(startDate)) 
  } else if (startDate != "null") {
    if(is.na(suppressWarnings(lubridate::parse_date_time(startDate, orders = "ymd")))){
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
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
  
  if (length(sampleMedia)>1) {
    WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia)) 
  } else if (sampleMedia != "null") {
    WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
  }
  
  if (length(project)>1) {
    WQPquery <- c(WQPquery, project = list(project)) 
  } else if (project != "null") {
    WQPquery <- c(WQPquery, project = project)
  }
  
  if (length(organization)>1) {
    WQPquery <- c(WQPquery, organization = list(organization)) 
  } else if (organization != "null") {
    WQPquery <- c(WQPquery, organization = organization)
  }
  
  if (length(endDate)>1) {
    if(is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], orders = "ymd")))){
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, endDate = list(endDate)) 
  } else if (endDate != "null") {
    if(is.na(suppressWarnings(lubridate::parse_date_time(endDate, orders = "ymd")))){
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
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
  if(applyautoclean==TRUE){
    
    TADAprofile.clean <- autoclean(TADAprofile)
    
  }else{
    
    TADAprofile.clean = TADAprofile
  }
  
  return(TADAprofile.clean)
}



#' Read in WQP data using the Water Quality Portal (WQP) web services
#'
#' Go to the WQP website (https://www.waterqualitydata.us/) and fill out the
#' advanced query form. Choose the file format Comma-Separated. 
#' Then, choose a data profile. When finished, do not hit the download button.
#' Instead, copy the web service URL located at the bottom of the page under 
#' the header "Station" or "Result". This is the url in the second box from the 
#' top. Use that web service URL as the input for this function to download 
#' data directly into R.
#' 
#' We recommend retrieving data for all the following profiles 
#' (you can run this function four separate times to bring in all four profiles):
#' 1. Sample Results (physical/chemical metadata)
#' 2. Sample Results (narrow)
#' 3. Project Data
#' 4. Site Data Only
#' 
#' After you retrieve all four profiles, you can use TADA::JoinWQPProfiles to 
#' joining the four dataframes into a single dataframe.
#' 
#' Note: It may be useful to save the Query URL from the WQP as well as a 
#' comment within your code. This URL let's you return to the WQP query page 
#' with all your selected data filters. For example, this is the query used
#' in the examples for this function: 
#' https://www.waterqualitydata.us/#statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&dataProfile=biological&providers=NWIS&providers=STEWARDS&providers=STORET
#'
#' @param webservice WQP Web Service URL, entered within quotes, i.e., "webserviceurl"
#'
#' @return WQP Data Profile
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' physchemresults1 <- TADAReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&dataProfile=biological&providers=NWIS&providers=STEWARDS&providers=STORET")
#' sites1 <- TADAReadWQPWebServices("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' projects1 <- TADAReadWQPWebServices("https://www.waterqualitydata.us/data/Project/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' narrow1 <- TADAReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&dataProfile=narrowResult&providers=NWIS&providers=STEWARDS&providers=STORET")
#' }
#'

TADAReadWQPWebServices <- function(webservice) {
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
#' characteristicName, and siteType requested. Computer memory may limit the
#' size of datasets that your R console will be able to hold in one session.
#' Function requires a characteristicName, siteType, statecode, huc, or start/
#' end date input. The recommendation is to be as specific as you can with your
#' large data call.
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
#' censored data later on (i.e., nondetections)
#' 
#' Some code for this function was adapted from this USGS Blog (Author: Aliesha Krall)
#' \href{https://waterdata.usgs.gov/blog/large_sample_pull/}{Large Sample Pull}  
#' 
#' See ?MeasureValueSpecialCharacters and ?autoclean documentation for more information.
#' 
#' @param startDate Start Date YYYY-MM-DD format, for example, "1995-01-01"
#' @param endDate end date in YYYY-MM-DD format, for example, "2020-12-31"
#' @param statecode Character/character vector. State/territory abbreviations from FIPS codes consist of two letters 
#' @param huc A numeric code denoting a hydrologic unit. Example: "04030202". Different size hucs can be entered.
#' @param characteristicName Name of water quality parameter
#' @param siteType Name of water body type (e.g., "Stream", "Lake, Reservoir, Impoundment")
#' @param sampleMedia Defaults to "Water". Refer to WQP domain tables for other options.
#' @param applyautoclean Defaults to FALSE. If TRUE, runs TADA's autoclean function on final combined dataset.
#' 
#' @return TADA-compatible dataframe
#' 
#' @export
#'
#' @examples 
#' \dontrun{
#' tada1 <- TADABigdataRetrieval(startDate = "2019-01-01", endDate = "2021-12-31", characteristicName = "Temperature, water", statecode = c("AK","AL"))
#' tada2 <- TADABigdataRetrieval(startDate = "2016-10-01",endDate = "2022-09-30", statecode = "UT")
#' tada3 = TADABigdataRetrieval(huc = "04030202", characteristicName = "Escherichia coli")
#' tada4 = TADABigdataRetrieval(huc = c("04030202","04030201"), characteristicName = "Temperature, water")
#' }
#' 

TADABigdataRetrieval <- function(startDate = "null",
                              endDate = "null",
                              statecode = "null",
                              huc = "null",
                              characteristicName = "null", 
                              siteType = "null",
                              sampleMedia = "Water",
                              applyautoclean = FALSE
) {
  
  start_T = Sys.time()
  
  if(!"null"%in%statecode&!"null"%in%huc){stop("Please provide either state code(s) OR huc(s) to proceed.")}

  if(!startDate=="null"){
    startDate = lubridate::ymd(startDate)
    startYearLo = lubridate::year(startDate)
  }else{ # else: pick a date before which any data are unlikely to be in WQP
    startDate = lubridate::ymd("1800-01-01")
    startYearLo = lubridate::year(startDate)
  } 
  
# Logic: if the input endDate is not null, convert to date and obtain year
  # for summary
  if(!endDate=="null"){
    endDate = lubridate::ymd(endDate)
    endYearHi = lubridate::year(endDate)
  }else{ # else: if not populated, default to using today's date/year for summary
    endDate = Sys.Date()
    endDate = lubridate::ymd(endDate)
    endYearHi = lubridate::year(endDate)
  }
  
  # Create readWQPsummary query
  WQPquery <- list()
  if (length(characteristicName)>1) {
    WQPquery = c(WQPquery,characteristicName = list(characteristicName)) 
  } else if (characteristicName != "null") {
    WQPquery = c(WQPquery,characteristicName = characteristicName)
  }
  if (length(siteType)>1) {
    WQPquery = c(WQPquery,siteType = list(siteType)) 
  } else if (siteType != "null") {
    WQPquery = c(WQPquery,siteType = siteType)
  }

  if (!"null"%in%statecode) {
    # state_cd_cont = utils::read.csv(file = "inst/extdata/statecode.csv",colClasses=c("STATE"="character"))
    load("inst/extdata/statecodes_df.Rdata")
    statecode = as.character(statecode)
    statecodes_sub = statecodes_df%>%dplyr::filter(STUSAB%in%statecode)
    statecd = paste0("US:",statecodes_sub$STATE)
    if(nrow(statecodes_sub)==0){stop("State code is not valid. Check FIPS state/territory abbreviations.")}
    if(length(statecode)>1){
      for(i in 1:length(statecode)){
        WQPquery = c(WQPquery, statecode=list(statecd))
      }
      WQPquery = c(WQPquery, statecode=list(statecd))
    }else{WQPquery = c(WQPquery, statecode=statecd)}
  }
  
  if (length(huc)>1) {
    WQPquery = c(WQPquery,huc = list(huc)) 
  } else if (huc != "null") {
    WQPquery = c(WQPquery,huc = huc)
  }
  
  print("Building site summary table for chunking result downloads...")
  df_summary = dataRetrieval::readWQPsummary(WQPquery)
  
  # Create readWQPdata query
  WQPquery2 = list(startDate = startDate, endDate = endDate)
  if (length(characteristicName)>1) {
    WQPquery2 = c(WQPquery2,characteristicName = list(characteristicName)) 
  } else if (characteristicName != "null") {
    WQPquery2 = c(WQPquery2,characteristicName = characteristicName)
  }
  
  if (length(sampleMedia)>1) {
    WQPquery2 <- c(WQPquery2, sampleMedia = list(sampleMedia)) 
  } else if (sampleMedia != "null") {
    WQPquery2 <- c(WQPquery2, sampleMedia = sampleMedia)
  }
  
    ## NOTE: if query brings back no results, function returns empty 
    # dataRetrieval profile, not empty summary
    if(nrow(df_summary)>0){
      sites = df_summary %>%
        dplyr::filter(YearSummarized >= startYearLo,
                      YearSummarized <= endYearHi)
      
      siteid_all = unique(sites$MonitoringLocationIdentifier)
      rm(df_summary) # save some space
      
      if(length(siteid_all) > 0) {
        rm(sites) # save some space
        l=length(siteid_all)  #len(sites)
        maxsites=100   #max number of sites pulled per WQP query
        #may want to consider using the total number of records in a given 
        #download group instead, e.g., records must not exceed some maximum 
        #threshold (e.g. USGS uses 250,000 records per group for their pipelines)
        site_groups = split(siteid_all, ceiling(seq_along(siteid_all)/maxsites))

        df = data.frame()
        print("Starting result downloads...")
        for(j in 1:length(site_groups)){
          sites = site_groups[[j]]

          results.DR <- dataRetrieval::readWQPdata(siteid = sites,
                                                WQPquery2, 
                                                dataProfile = "resultPhysChem",
                                                ignore_attributes = TRUE)
        
          narrow.DR <- dataRetrieval::readWQPdata(siteid = sites,
                                                WQPquery2,
                                                dataProfile = "narrowResult", 
                                                ignore_attributes = TRUE)
        
          sites.DR <- dataRetrieval::whatWQPsites(siteid = sites,
                                                WQPquery2)
          
          projects.DR <- dataRetrieval::readWQPdata(siteid = sites,
                                                WQPquery2,
                                                service = "Project")

          joins = JoinWQPProfiles(FullPhysChem = results.DR,
                                  Sites = sites.DR,
                                  Narrow = narrow.DR,
                                  Projects = projects.DR)
          
          # need to specify this or throws error when trying to bind rows. Temporary fix for larger
          # issue where data structure for all columns should be specified.
          joins = joins%>%dplyr::mutate_at(c("ActivityDepthHeightMeasure.MeasureValue",
                                      "ActivityTopDepthHeightMeasure.MeasureValue",
                                      "ActivityBottomDepthHeightMeasure.MeasureValue",
                                      "ResultMeasureValue",
                                      "ResultDepthHeightMeasure.MeasureValue",
                                      "DetectionQuantitationLimitMeasure.MeasureValue",
                                      "DrainageAreaMeasure.MeasureValue",
                                      "ContributingDrainageAreaMeasure.MeasureValue",
                                      "HorizontalAccuracyMeasure.MeasureValue",
                                      "VerticalMeasure.MeasureValue",
                                      "VerticalAccuracyMeasure.MeasureValue",
                                      "WellDepthMeasure.MeasureValue",
                                      "WellHoleDepthMeasure.MeasureValue"), as.character)
          
          df = dplyr::bind_rows(df, joins)
          # status of download relative to total number of sites queried.
          perc = round(j/length(site_groups)*100)
          print(paste0(perc,"% of sites run through web services and their data successfully combined."))
        }
      }else{
        warning("Query returned no data. Function returns an empty dataframe.")
        return(sites)
      }
    }else{
      warning("Query returned no data. Function returns an empty dataframe.")
      return(df_summary)
    }
  if(applyautoclean == TRUE){
    print("Applying TADA autoclean function...")
    df = autoclean(df)
  }
  
  # timing function for efficiency tests.
  difference = difftime(Sys.time(), start_T, units = "mins")
  print(difference)
  
  return(df)
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
        dplyr::left_join(Sites.df, by = "MonitoringLocationIdentifier", multiple = "all") %>%
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
          "ProjectIdentifier","ProjectName"),
        multiple = "all"
        )
    }else{join3 = join2}
    
  }else{join3 <- join2}
  return(join3)
}
