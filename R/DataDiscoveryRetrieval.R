#' Generate TADA-compatible dataframe from WQP Data
#'
#' Retrieve data from Water Quality Portal (WQP) and generate a TADA-compatible
#' dataframe. Note that the inputs (e.g. project, organization, siteType) with the
#' exceptions of endDate and startDate match the web service call format from the
#' online WQP GUI. endDate and startDate match the format suggested in USGS's
#' dataRetrieval package (endDate = "YYYY-MM-DD"), which is a more familiar date
#' format for R users than the WQP GUI's endDateHi = "MM-DD-YYYY".
#'
#' Multiple fields are queried together using AND logic, but multiple values within
#' one field are queried together using OR logic. For example, within
#' characteristicName, if you enter, c("pH", "Dissolved oxygen (DO)), the
#' function will return all results that are "pH" OR "Dissolved oxygen (DO)". Similarly,
#' if you enter c("VA", "IL"), the function will return results from Virginia OR Illinois.
#' But the combo of these fields are ANDs: The function will return any pH and DO data
#' from only Virginia or Illinois; the data must fit into one of the values from BOTH
#' of the query fields.
#' characteristicName and Characteristic Group also work as an AND, therefore the
#' characteristicName must fall within the Characteristic Group when both are entered.
#'
#'
#' Users can reference the \href{https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads}{WQX domain tables}
#' to find allowable values for queries, e.g., reference the WQX domain table to find countycode and statecode: https://cdx.epa.gov/wqx/download/DomainValues/County_CSV.zip
#' Alternatively, you can use the WQP services to find areas where data is available in the US: https://www.waterqualitydata.us/Codes/countycode
#'
#' TADA_DataRetrieval automatically runs TADA_AutoClean on the incoming dataset. TADA_AutoClean
#' is important for categorizing result value and detection limit data, as well as
#' harmonizing key columns used in TADA. See ?TADA_AutoClean for more information.
#'
#' Note: TADA_DataRetrieval (by leveraging dataRetrieval),  automatically converts
#' the date times to UTC. It also automatically converts the data to dates,
#' datetimes, numerics based on a standard algorithm. See: ?dataRetrieval::readWQPdata
#'
#' @param startDate Start Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param endDate End Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param countrycode Code that identifies a country or ocean (e.g. countrycode = "CA" for Canada, countrycode = "OA" for Atlantic Ocean). See https://www.waterqualitydata.us/Codes/countrycode for options.
#' @param statecode FIPS state alpha code that identifies a state (e.g. statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param countycode FIPS county name. Note that a state code must also be supplied (e.g. statecode = "AL", countycode = "Chilton"). See https://www.waterqualitydata.us/Codes/countycode for options.
#' @param huc A numeric code denoting a hydrologic unit. Example: "04030202". Different size hucs can be entered. See https://epa.maps.arcgis.com/home/item.html?id=796992f4588c401fabec7446ecc7a5a3 for a map with HUCS. Click on a HUC to find the associated code.
#' @param siteid Unique monitoring location identifier.
#' @param siteType Type of waterbody. See https://www.waterqualitydata.us/Codes/sitetype for options.
#' @param characteristicName Name of parameter. See https://www.waterqualitydata.us/Codes/characteristicName for options.
#' @param characteristicType Groups of environmental measurements/parameters. See https://www.waterqualitydata.us/Codes/characteristicType for options.
#' @param sampleMedia Sampling substrate such as water, air, or sediment. See https://www.waterqualitydata.us/Codes/sampleMedia for options.
#' @param organization A string of letters and/or numbers (some additional characters also possible) used to signify an organization with data in the Water Quality Portal. See https://www.waterqualitydata.us/Codes/organization for options.
#' @param project A string of letters and/or numbers (some additional characters also possible) used to signify a project with data in the Water Quality Portal. See https://www.waterqualitydata.us/Codes/project for options.
#' @param providers Leave blank to include all, or specify "STEWARDS", "STORET" (i.e., WQX), and/or "NWIS". See https://www.waterqualitydata.us/Codes/providers for options.
#' @param applyautoclean Logical, defaults to TRUE. Applies TADA_AutoClean function on the returned data profile.
#'
#' @return TADA-compatible dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # example for WI
#' tada1 <- TADA_DataRetrieval(statecode = "WI", countycode = "Dane", characteristicName = "Phosphorus")
#'
#' # example for UT
#' tada2 <- TADA_DataRetrieval(statecode = "UT", characteristicName = c("Ammonia", "Nitrate", "Nitrogen"))
#'
#' # example for SC
#' tada3 <- TADA_DataRetrieval(statecode = "SC", countycode = "Abbeville")
#'
#' # example for CT
#' tada4 <- TADA_DataRetrieval(statecode = "CT", startDate = "2020-10-01")
#'
#'
#' # note that countycode queries require a statecode (see example below)
#' tada5 <- TADA_DataRetrieval(countycode = "US:02:020")
#'
#' # example for NM
#' tada6 <- TADA_DataRetrieval(
#'   statecode = "NM",
#'   characteristicName = c(
#'     "Ammonia",
#'     "Nitrate",
#'     "Nitrogen"
#'   ),
#'   startDate = "2020-05-01"
#' )
#'
#' # example for AK project
#' tada7 <- TADA_DataRetrieval(project = "Anchorage Bacteria 20-21")
#'
#' # another example for AK
#' tada8 <- TADA_DataRetrieval(
#'   statecode = "AK",
#'   characteristicName = c(
#'     "Fecal Coliform",
#'     "Escherichia coli",
#'     "Enterococcus",
#'     "Ammonia",
#'     "Nitrate",
#'     "Nitrogen"
#'   ),
#'   startDate = "2018-05-01"
#' )
#'
#' # example for tribes
#' # Download data for many of the ATTAINS participating tribes
#' # Note this query may take about a half hour to run
#' # https://www.itecmembers.org/attains/
#' # ATTAINS participating tribes also have tribal pages in EPA's
#' # How's My Waterway Application
#' # Example: https://mywaterway.epa.gov/tribe/SFNOES
#' #
#' # Sac & Fox Nation, Oklahoma "SFNOES_WQX"
#' # Citizen Potawatomi Nation, Oklahoma "CPNWATER"
#' # Delaware Nation, Oklahoma "DELAWARENATION"
#' # Hoopa Valley Tribe, California "HVTEPA_WQX"
#' # Otoe Missouria Tribe of Oklahoma "O_MTRIBE_WQX"
#' # Minnesota Chippewa Tribe, Minnesota (Fond du Lac Band) "FONDULAC_WQX"
#' # Pueblo of San Ildefonso, New Mexico "SANILDEFONSODECP"
#' # Pueblo of Santa Ana, New Mexico "PUEBLO_SANTAANA"
#' # Pueblo of Tesuque, New Mexico "PUEBLOOFTESUQUE"
#' # Red Lake Band of Chippewa Indians, Minnesota "REDLAKE_WQX"
#' # Seneca-Cayuga Nation "SCEQ"
#' # The Chickasaw Nation "CNENVSER"
#' # The Choctaw Nation of Oklahoma "CHOCNATWQX"
#' # Wyandotte Nation "WNENVDPT_WQX"
#' # Pueblo of Pojoaque "PUEBLO_POJOAQUE"
#'
#' tada9 <- TADA_DataRetrieval(organization = c(
#'   "SFNOES_WQX",
#'   "CPNWATER",
#'   "DELAWARENATION",
#'   "HVTEPA_WQX",
#'   "O_MTRIBE_WQX",
#'   "FONDULAC_WQX",
#'   "SANILDEFONSODECP",
#'   "PUEBLO_SANTAANA",
#'   "PUEBLOOFTESUQUE",
#'   "REDLAKE_WQX",
#'   "SCEQ",
#'   "CNENVSER",
#'   "CHOCNATWQX",
#'   "WNENVDPT_WQX",
#'   "PUEBLO_POJOAQUE"
#' ))
#'
#' # query only NWIS data for a 10 year period in CT
#' tada10 <- TADA_DataRetrieval(
#'   startDate = "2013-01-01",
#'   endDate = "2022-12-31",
#'   sampleMedia = c("Water", "water"),
#'   statecode = "CT", # consider downloading only 1 state at a time
#'   providers = "NWIS",
#'   applyautoclean = FALSE
#' )
#'
#' # query by country code (e.g. Canada, countrycode = "CA")
#' tada11 <- TADA_DataRetrieval(
#'   startDate = "2015-01-01",
#'   countrycode = "CA"
#' )
#' }
#'
TADA_DataRetrieval <- function(startDate = "null",
                               endDate = "null",
                               countrycode = "null",
                               countycode = "null",
                               huc = "null",
                               siteid = "null",
                               siteType = "null",
                               characteristicName = "null",
                               characteristicType = "null",
                               sampleMedia = "null",
                               statecode = "null",
                               organization = "null",
                               project = "null",
                               providers = "null",
                               applyautoclean = TRUE) {
  # Set query parameters
  WQPquery <- list()

  if (!"null" %in% statecode) {
    load(system.file("extdata", "statecodes_df.Rdata", package = "TADA"))
    statecode <- as.character(statecode)
    statecodes_sub <- statecodes_df %>% dplyr::filter(STUSAB %in% statecode)
    statecd <- paste0("US:", statecodes_sub$STATE)
    if (nrow(statecodes_sub) == 0) {
      stop("State code is not valid. Check FIPS state/territory abbreviations.")
    }
    if (length(statecode) >= 1) {
      WQPquery <- c(WQPquery, statecode = list(statecd))
    }
  }

  if (length(huc) > 1) {
    WQPquery <- c(WQPquery, huc = list(huc))
  } else if (huc != "null") {
    WQPquery <- c(WQPquery, huc = huc)
  }

  if (length(startDate) > 1) {
    if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, startDate = list(startDate))
  } else if (startDate != "null") {
    if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, startDate = startDate)
  }

  if (length(countrycode) > 1) {
    WQPquery <- c(WQPquery, countrycode = list(countrycode))
  } else if (countrycode != "null") {
    WQPquery <- c(WQPquery, countrycode = countrycode)
  }

  if (length(countycode) > 1) {
    WQPquery <- c(WQPquery, countycode = list(countycode))
  } else if (countycode != "null") {
    WQPquery <- c(WQPquery, countycode = countycode)
  }

  if (length(siteid) > 1) {
    WQPquery <- c(WQPquery, siteid = list(siteid))
  } else if (siteid != "null") {
    WQPquery <- c(WQPquery, siteid = siteid)
  }

  if (length(siteType) > 1) {
    WQPquery <- c(WQPquery, siteType = list(siteType))
  } else if (siteType != "null") {
    WQPquery <- c(WQPquery, siteType = siteType)
  }

  if (length(characteristicName) > 1) {
    WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
  } else if (characteristicName != "null") {
    WQPquery <- c(WQPquery, characteristicName = characteristicName)
  }

  if (length(characteristicType) > 1) {
    WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
  } else if (characteristicType != "null") {
    WQPquery <- c(WQPquery, characteristicType = characteristicType)
  }

  if (length(sampleMedia) > 1) {
    WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
  } else if (sampleMedia != "null") {
    WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
  }

  if (length(project) > 1) {
    WQPquery <- c(WQPquery, project = list(project))
  } else if (project != "null") {
    WQPquery <- c(WQPquery, project = project)
  }

  if (length(providers) > 1) {
    WQPquery <- c(WQPquery, providers = list(providers))
  } else if (providers != "null") {
    WQPquery <- c(WQPquery, providers = providers)
  }

  if (length(organization) > 1) {
    WQPquery <- c(WQPquery, organization = list(organization))
  } else if (organization != "null") {
    WQPquery <- c(WQPquery, organization = organization)
  }

  if (length(endDate) > 1) {
    if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, endDate = list(endDate))
  } else if (endDate != "null") {
    if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, endDate = endDate)
  }

  # Retrieve all 3 profiles
  print("Downloading WQP query results. This may take some time depending upon the query size.")
  print(WQPquery)
  results.DR <- dataRetrieval::readWQPdata(WQPquery,
    dataProfile = "resultPhysChem",
    ignore_attributes = TRUE
  )
  # check if any results are available
  if ((nrow(results.DR) > 0) == FALSE) {
    print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
    TADAprofile.clean <- results.DR
  } else {
    sites.DR <- dataRetrieval::whatWQPsites(WQPquery)

    projects.DR <- dataRetrieval::readWQPdata(WQPquery,
      ignore_attributes = TRUE,
      service = "Project"
    )

    TADAprofile <- TADA_JoinWQPProfiles(
      FullPhysChem = results.DR,
      Sites = sites.DR,
      Projects = projects.DR
    )

    # need to specify this or throws error when trying to bind rows. Temporary fix for larger
    # issue where data structure for all columns should be specified.
    cols <- names(TADAprofile)

    TADAprofile <- TADAprofile %>% dplyr::mutate_at(cols, as.character)

    # run TADA_AutoClean function
    if (applyautoclean == TRUE) {
      print("Data successfully downloaded. Running TADA_AutoClean function.")

      TADAprofile.clean <- TADA_AutoClean(TADAprofile)
    } else {
      TADAprofile.clean <- TADAprofile
    }
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
#' (you can run this function three separate times to bring in all three profiles):
#' 1. Sample Results (physical/chemical metadata)
#' 2. Project Data
#' 3. Site Data Only
#'
#' After you retrieve all three profiles, you can use TADA::TADA_JoinWQPProfiles to
#' join the three dataframes into a single dataframe.
#'
#' Note: It may be useful to save the Query URL from the WQP as well as a
#' comment within your code. This URL let's you return to the WQP query page
#' with all your selected data filters. For example, this is the query used
#' in the examples for this function:
#' https://www.waterqualitydata.us/#statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&dataProfile=biological&providers=NWIS&providers=STEWARDS&providers=STORET
#'
#' **Extra tip:** Note that the web service call built using the Water
#' Quality Portal uses the inputs startDateLo and startDateHi rather than
#' startDate and endDate, and dates are in the format MM-DD-YYYY rather
#' than the TADA_DataRetrieval and dataRetrieval format of YYYY-MM-DD. The
#' functions use the latter format rather than the web service call date
#' format because YYYY-MM-DD is a more easily utilized format in the R
#' coding environment. However, users of USGS's dataRetrieval may use the
#' date format MM-DD-YYYY *only if* they specify with "startDateLo" and
#' "startDateHi" inputs. For coding consistency, it is recommended users
#' stick with YYYY-MM-DD.
#'
#' @param webservice WQP Web Service URL, entered within quotes, i.e., "webserviceurl"
#'
#' @return WQP Data Profile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' physchemresults1 <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&dataProfile=biological&providers=NWIS&providers=STEWARDS&providers=STORET")
#' sites1 <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' projects1 <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Project/search?statecode=US%3A09&sampleMedia=water&sampleMedia=Water&startDateLo=01-01-2021&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' }
#'
TADA_ReadWQPWebServices <- function(webservice) {
  # consider function dataRetrieval::getWebServiceData
  # read in csv from WQP web service
  if (grepl("zip=yes", webservice)) {
    webservice <- stringr::str_replace(webservice, "zip=yes", "zip=no")

    # download data
    webservice <- data.table::fread(toString(webservice))

    # if input df was not downloaded using USGS's dataRetrieval, then the
    # column names will include / separators instead of . and TADA uses .
    # (e.g. ResultMeasure/MeasureUnitCode vs. ResultMeasure.MeasureUnitCode)
    colnames(webservice) <- gsub("/", ".", colnames(webservice))

    return(webservice)
  } else {
    # download data
    webservice <- data.table::fread(toString(webservice))

    # if input df was not downloaded using USGS's dataRetrieval, then the
    # column names will include / separators instead of . and TADA uses .
    # (e.g. ResultMeasure/MeasureUnitCode vs. ResultMeasure.MeasureUnitCode)
    colnames(webservice) <- gsub("/", ".", colnames(webservice))

    return(webservice)
  }
}



#' Large WQP data pulls using dataRetrieval
#'
#' This function does multiple synchronous data calls to the WQP
#' (waterqualitydata.us). It uses the WQP summary service to limit the amount
#' downloaded to only relevant data (based on user query), pulls back data for
#' 250000 records at a time, and then joins the data back together to produce a
#' single TADA compatible dataframe as the output. For large data sets, that can save a lot
#' of time and ultimately reduce the complexity of subsequent data processing.
#' Using this function, you will be able to download all data available from all
#' sites in the contiguous United States available for the time period,
#' characteristicName, and siteType requested. Computer memory may limit the
#' size of datasets that your R console will be able to hold in one session.
#' Function requires a characteristicName, siteType, statecode, huc, or start/
#' end date input. The recommendation is to be as specific as you can with your
#' large data call. The function allows the user to run TADA_AutoClean on the dataset,
#' but this is not the default as checking large dataframes for exact duplicate
#' rows can be time consuming and is better performed on its own once the query is
#' completed.
#'
#' Some code for this function was adapted from this USGS Blog (Author: Aliesha Krall)
#' \href{https://waterdata.usgs.gov/blog/large_sample_pull/}{Large Sample Pull}
#'
#' See ?TADA_AutoClean documentation for more information on this optional input.
#'
#' Note: TADA_BigDataRetrieval (by leveraging USGS's dataRetrieval),  automatically converts
#' the date times to UTC. It also automatically converts the data to dates,
#' datetimes, numerics based on a standard algorithm. See: ?dataRetrieval::readWQPdata
#'
#' @param startDate Start Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param endDate End Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param countrycode Code that identifies a country or ocean (e.g. countrycode = "CA" for Canada, countrycode = "OA" for Atlantic Ocean). See https://www.waterqualitydata.us/Codes/countrycode for options.
#' @param statecode FIPS state alpha code that identifies a state (e.g. statecode = "DE" for Delaware). See https://www.waterqualitydata.us/Codes/statecode for options.
#' @param countycode FIPS county name. Note that a state code must also be supplied (e.g. statecode = "AL", countycode = "Chilton"). See https://www.waterqualitydata.us/Codes/countycode for options.
#' @param huc A numeric code denoting a hydrologic unit. Example: "04030202". Different size hucs can be entered. See https://epa.maps.arcgis.com/home/item.html?id=796992f4588c401fabec7446ecc7a5a3 for a map with HUCS. Click on a HUC to find the associated code.
#' @param siteid Unique monitoring location identifier.
#' @param siteType Type of waterbody. See https://www.waterqualitydata.us/Codes/sitetype for options.
#' @param characteristicName Name of parameter. See https://www.waterqualitydata.us/Codes/characteristicName for options.
#' @param characteristicType Groups of environmental measurements/parameters. See https://www.waterqualitydata.us/Codes/characteristicType for options.
#' @param sampleMedia Sampling substrate such as water, air, or sediment. See https://www.waterqualitydata.us/Codes/sampleMedia for options.
#' @param organization A string of letters and/or numbers (some additional characters also possible) used to signify an organization with data in the Water Quality Portal. See https://www.waterqualitydata.us/Codes/organization for options.
#' @param maxrecs The maximum number of results queried within one call to dataRetrieval.
#' @param applyautoclean Logical, defaults to FALSE. If TRUE, runs TADA_AutoClean function on the returned data profile.
#'
#' @return TADA-compatible dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # takes approx 3 mins to run
#' tada1 <- TADA_BigDataRetrieval(startDate = "2019-01-01", endDate = "2021-12-31", characteristicName = "Temperature, water", statecode = c("AK", "AL"))
#'
#' # takes approx 21 mins
#' tada2 <- TADA_BigDataRetrieval(startDate = "2016-10-01", endDate = "2022-09-30", statecode = "UT")
#'
#' # takes seconds to run
#' tada3 <- TADA_BigDataRetrieval(huc = "04030202", characteristicName = "Escherichia coli")
#'
#' # takes approx 3 mins to run
#' tada4 <- TADA_BigDataRetrieval(startDate = "2004-01-01", countrycode = "CA")
#'
#' # takes seconds to run
#' tada5 <- TADA_BigDataRetrieval(startDate = "2018-01-01", statecode = "AL", countycode = "Chilton")
#'
#' # takes seconds to run
#' tada6 <- TADA_BigDataRetrieval(organization = "PUEBLOOFTESUQUE")
#' }
#'
TADA_BigDataRetrieval <- function(startDate = "null",
                                  endDate = "null",
                                  countrycode = "null",
                                  statecode = "null",
                                  countycode = "null",
                                  huc = "null",
                                  siteid = "null",
                                  siteType = "null",
                                  characteristicName = "null",
                                  characteristicType = "null",
                                  sampleMedia = "null",
                                  organization = "null",
                                  maxrecs = 250000,
                                  applyautoclean = FALSE) {
  start_T <- Sys.time()

  if (!"null" %in% statecode & !"null" %in% huc) {
    stop("Please provide either state code(s) OR huc(s) to proceed.")
  }

  if (!startDate == "null") {
    startDat <- lubridate::ymd(startDate)
    startYearLo <- lubridate::year(startDat)
  } else { # else: pick a date before which any data are unlikely to be in WQP
    startDate <- "1800-01-01"
    startDat <- lubridate::ymd(startDate)
    startYearLo <- lubridate::year(startDat)
  }

  # Logic: if the input endDate is not null, convert to date and obtain year
  # for summary
  if (!endDate == "null") {
    endDat <- lubridate::ymd(endDate)
    endYearHi <- lubridate::year(endDat)
  } else { # else: if not populated, default to using today's date/year for summary
    endDate <- as.character(Sys.Date())
    endDat <- lubridate::ymd(endDate)
    endYearHi <- lubridate::year(endDat)
  }

  # Create readWQPsummary query
  WQPquery <- list()
  if (length(characteristicName) > 1) {
    WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
  } else if (characteristicName != "null") {
    WQPquery <- c(WQPquery, characteristicName = characteristicName)
  }
  if (length(characteristicType) > 1) {
    WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
  } else if (characteristicType != "null") {
    WQPquery <- c(WQPquery, characteristicType = characteristicType)
  }
  if (length(siteType) > 1) {
    WQPquery <- c(WQPquery, siteType = list(siteType))
  } else if (siteType != "null") {
    WQPquery <- c(WQPquery, siteType = siteType)
  }

  if (!"null" %in% statecode) {
    load(system.file("extdata", "statecodes_df.Rdata", package = "TADA"))
    statecode <- as.character(statecode)
    statecodes_sub <- statecodes_df %>% dplyr::filter(STUSAB %in% statecode)
    statecd <- paste0("US:", statecodes_sub$STATE)
    if (nrow(statecodes_sub) == 0) {
      stop("State code is not valid. Check FIPS state/territory abbreviations.")
    }
    if (length(statecode) > 1) {
      for (i in 1:length(statecode)) {
        WQPquery <- c(WQPquery, statecode = list(statecd))
      }
      WQPquery <- c(WQPquery, statecode = list(statecd))
    } else {
      WQPquery <- c(WQPquery, statecode = statecd)
    }
  }

  if (length(huc) > 1) {
    WQPquery <- c(WQPquery, huc = list(huc))
  } else if (huc != "null") {
    WQPquery <- c(WQPquery, huc = huc)
  }

  if (length(countrycode) > 1) {
    WQPquery <- c(WQPquery, countrycode = list(countrycode))
  } else if (countrycode != "null") {
    WQPquery <- c(WQPquery, countrycode = countrycode)
  }

  if (length(countycode) > 1) {
    WQPquery <- c(WQPquery, countycode = list(countycode))
  } else if (countycode != "null") {
    WQPquery <- c(WQPquery, countycode = countycode)
  }

  if (length(organization) > 1) {
    WQPquery <- c(WQPquery, organization = list(organization))
  } else if (organization != "null") {
    WQPquery <- c(WQPquery, organization = organization)
  }

  # cut down on summary query time if possible based on big data query
  diffdat <- lubridate::time_length(difftime(Sys.Date(), startDat), "years")

  if (diffdat <= 1) {
    WQPquery <- c(WQPquery, summaryYears = 1)
  }

  if (diffdat > 1 & diffdat <= 5) {
    WQPquery <- c(WQPquery, summaryYears = 5)
  }

  print("Building site summary table for chunking result downloads...")
  df_summary <- dataRetrieval::readWQPsummary(WQPquery)

  ## NOTE: if query brings back no results, function returns empty
  # dataRetrieval profile, not empty summary
  if (nrow(df_summary) > 0) {
    # narrow down to years of interest from summary
    sites <- df_summary %>%
      dplyr::filter(
        YearSummarized >= startYearLo,
        YearSummarized <= endYearHi
      )

    rm(df_summary)
    # if there are still site records when filtered to years of interest....
    if (dim(sites)[1] > 0) {
      # function for chunking by records
      make_groups <- function(x, maxrecs) {
        if (sum(x$tot_n) <= maxrecs | dim(x)[1] == 1) { # WARNING: if there's only one row and it's more than maxrecs, it will try to run the query anyway
          groupings <- x
          groupings$group <- 1
        } else {
          groupings <- data.frame()
          group <- data.frame()
          i <- 1
          while (nrow(x) > nrow(group)) {
            x$csum <- cumsum(x$tot_n)
            brk <- which(x$csum > maxrecs)[1]
            group <- x[1:(brk - 1), ]
            group$group <- i
            if (brk > 1) {
              x <- x[brk:length(x$tot_n), ]
            } else {
              x <- x[2:length(x$tot_n), ]
            }
            i <- i + 1
            groupings <- plyr::rbind.fill(groupings, group)
          }

          x$group <- i

          groupings <- plyr::rbind.fill(groupings, x)
        }
        return(groupings)
      }


      # get total number of results per site and separate out sites with >250000 results
      tot_sites <- sites %>%
        dplyr::group_by(MonitoringLocationIdentifier) %>%
        dplyr::summarise(tot_n = sum(ResultCount)) %>%
        dplyr::arrange(tot_n)
      smallsites <- tot_sites %>% dplyr::filter(tot_n <= maxrecs)
      bigsites <- tot_sites %>% dplyr::filter(tot_n > maxrecs)

      df <- data.frame()

      if (dim(smallsites)[1] > 0) {
        smallsitesgrp <- make_groups(smallsites, maxrecs)

        print(paste0("Downloading data from sites with fewer than ", maxrecs, " results by grouping them together."))

        for (i in 1:max(smallsitesgrp$group)) {
          site_chunk <- subset(smallsitesgrp$MonitoringLocationIdentifier, smallsitesgrp$group == i)
          joins <- TADA::TADA_DataRetrieval(
            startDate = startDate,
            endDate = endDate,
            siteid = site_chunk,
            characteristicName = characteristicName,
            characteristicType = characteristicType,
            sampleMedia = sampleMedia,
            applyautoclean = FALSE
          )
          if (dim(joins)[1] > 0) {
            df <- dplyr::bind_rows(df, joins)
          }
        }

        rm(smallsites, smallsitesgrp)
      }

      if (dim(bigsites)[1] > 0) {
        print(paste0("Downloading data from sites with greater than ", maxrecs, " results, chunking queries by shorter time intervals..."))

        bsitesvec <- unique(bigsites$MonitoringLocationIdentifier)

        for (i in 1:length(bsitesvec)) {
          mlidsum <- subset(sites, sites$MonitoringLocationIdentifier == bsitesvec[i])
          mlidsum <- mlidsum %>%
            dplyr::group_by(MonitoringLocationIdentifier, YearSummarized) %>%
            dplyr::summarise(tot_n = sum(ResultCount))
          site_chunk <- unique(mlidsum$MonitoringLocationIdentifier)

          bigsitegrps <- make_groups(mlidsum, maxrecs)

          for (i in 1:max(bigsitegrps$group)) {
            yearchunk <- subset(bigsitegrps$YearSummarized, bigsitegrps$group == i)
            startD <- paste0(min(yearchunk), "-01-01")
            endD <- paste0(max(yearchunk), "-12-31")

            joins <- TADA::TADA_DataRetrieval(
              startDate = startD,
              endDate = endD,
              siteid = site_chunk,
              characteristicName = characteristicName,
              characteristicType = characteristicType,
              sampleMedia = sampleMedia,
              applyautoclean = FALSE
            )

            if (dim(joins)[1] > 0) {
              df <- dplyr::bind_rows(df, joins)
            }
          }
        }
        rm(bigsites, bigsitegrps)
      }
    } else {
      warning("Query returned no data. Function returns an empty dataframe.")
      return(sites)
    }
  } else {
    warning("Query returned no data. Function returns an empty dataframe.")
    return(df_summary)
  }

  df <- subset(df, as.Date(df$ActivityStartDate, "%Y-%m-%d") >= startDat & as.Date(df$ActivityStartDate, "%Y-%m-%d") <= endDat)

  if (applyautoclean == TRUE) {
    print("Applying TADA_AutoClean function...")
    df <- TADA_AutoClean(df)
  }

  # timing function for efficiency tests.
  difference <- difftime(Sys.time(), start_T, units = "mins")
  print(difference)

  return(df)
}



#' Join WQP Profiles
#'
#' After retrieving multiple result and metadata profiles from the WQP, this
#' function joins those profiles together into one dataframe.
#' The FullPhysChem data input is required to run this function.
#'
#' @param FullPhysChem Full physical chemical data profile
#' @param Sites Sites data profile
#' @param Projects Projects data profile
#'
#' @return TADA-compatible dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load WQP data
#' # WQP URL: https://www.waterqualitydata.us/#statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&providers=NWIS&providers=STEWARDS&providers=STORET
#' # Use TADA_ReadWQPWebServices to load each profile
#' stationProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Station/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#' physchemProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Result/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&dataProfile=resultPhysChem&providers=NWIS&providers=STEWARDS&providers=STORET")
#' projectProfile <- TADA_ReadWQPWebServices("https://www.waterqualitydata.us/data/Project/search?statecode=US%3A09&characteristicType=Nutrient&startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&zip=yes&providers=NWIS&providers=STEWARDS&providers=STORET")
#'
#' # Join all three profiles using TADA_JoinWQPProfiles
#' TADAProfile <- TADA_JoinWQPProfiles(FullPhysChem = physchemProfile, Sites = stationProfile, Projects = projectProfile)
#' }
#'
TADA_JoinWQPProfiles <- function(FullPhysChem = "null",
                                 Sites = "null",
                                 Projects = "null") {
  FullPhysChem.df <- FullPhysChem

  Sites.df <- Sites

  Projects.df <- Projects

  # Join station data to full phys/chem (FullPhysChem.df)
  if (length(Sites.df > 1)) {
    if (nrow(Sites.df) > 0) {
      join1 <- FullPhysChem.df %>%
        # join stations to results
        dplyr::left_join(Sites.df,
          by = "MonitoringLocationIdentifier",
          multiple = "all",
          relationship = "many-to-many"
        ) %>%
        # remove ".x" suffix from column names
        dplyr::rename_at(dplyr::vars(dplyr::ends_with(".x")), ~ stringr::str_replace(., "\\..$", "")) %>%
        # remove columns with ".y" suffix
        dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    } else {
      join1 <- FullPhysChem.df
    }
  } else {
    join1 <- FullPhysChem.df
  }


  # Add QAPP columns from project
  if (length(Projects.df) > 1) {
    if (nrow(Projects.df) > 0) {
      join2 <- join1 %>%
        dplyr::left_join(
          dplyr::select(
            Projects.df, OrganizationIdentifier, OrganizationFormalName,
            ProjectIdentifier, ProjectName, ProjectDescriptionText,
            SamplingDesignTypeCode, QAPPApprovedIndicator, QAPPApprovalAgencyName,
            ProjectFileUrl, ProjectMonitoringLocationWeightingUrl
          ),
          by = c(
            "OrganizationIdentifier", "OrganizationFormalName",
            "ProjectIdentifier", "ProjectName"
          ),
          multiple = "all",
          # need to specify that this is expected to be a 1-to-many relationship
          relationship = "many-to-many"
        )
    } else {
      join2 <- join1
    }
  } else {
    join2 <- join1
  }


  return(join2)
}
