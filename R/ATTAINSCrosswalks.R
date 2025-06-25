#' Get Monitoring Location Identifier and Assessment Unit Identifier
#' Crosswalk from ATTAINS
#'
#' Tribes and States who participate in electronic reporting of water quality
#' conditions through EPA ATTAINS may optionally submit a crosswalk of WQP
#' monitoring location identifiers associated with their assessment units to
#' ATTAINS. If an organization has recorded this information in ATTAINS,
#' this function can be used to get the ATTAINS user submitted crosswalk of
#' known monitoring location identifiers and assessment units. As of 2025, all
#' tribal nations record this information in ATTAINS but only a few states.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading the
#' ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param.
#'
#' @return A dataframe with four columns, MonitoringLocationIdentifier,
#' OrganizationIdentifier, ATTAINS.assessmentunitidentifier, and
#' MonitoringDataLinkText is returned. This is the crosswalk between monitoring
#' location identifiers and assessment units that the state or tribal
#' organization submitted to ATTAINS (optional). If an ATTAINS organization
#' has not submitted this information in ATTAINS, the function will not return
#' a dataframe.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Alaska example
#' AK_crosswalk <- TADA_GetATTAINSAUSiteCrosswalk(org_id = "AKDECWQ")
#'
#' # Pueblo of Tesuque example
#' PUEBLOOFTESUQUE_crosswalk <- TADA_GetATTAINSAUSiteCrosswalk(
#'   org_id = "PUEBLOOFTESUQUE"
#' )
#'
#' # Arizona example, returns blank dataframe as of 1/21/25
#' AZ_crosswalk <- TADA_GetATTAINSAUSiteCrosswalk(org_id = "21ARIZ")
#' }
#'
TADA_GetATTAINSAUSiteCrosswalk <- function(org_id = NULL) {
  org.ref <- TADA_GetATTAINSOrgIDsRef()
  
  if (!org_id %in% org.ref$code) {
    print(paste0(
      "TADA_GetATTAINSAUSiteCrosswalk: ",
      "The organization identifier entered by user is not found in ATTAINS."
    ))
  }
  
  if (org_id %in% org.ref$code) {
    rm(org.ref)
    
    au.info <- rExpertQuery::EQ_AUsMLs(org_id = org_id, api_key = "lfzVzpwIlKS1O4l1QmbOLUeTzxyql4QdbHVR5Yf5")
    
    au.crosswalk <- au.info %>%
      dplyr::select(
        monitoringLocationId, monitoringLocationOrgId,
        assessmentUnitId, monitoringLocationDataLink
      ) %>%
      dplyr::filter(!is.na(monitoringLocationId)) %>%
      dplyr::distinct() %>%
      dplyr::rename(
        ATTAINS.assessmentunitidentifier = assessmentUnitId,
        MonitoringLocationIdentifier = monitoringLocationId,
        OrganizationIdentifier = monitoringLocationOrgId,
        MonitoringDataLinkText = monitoringLocationDataLink
      ) %>%
      # paste org_id in front of MLs from the specified org if they are missing
      # from ATTAINS
      dplyr::mutate(MonitoringLocationIdentifier = ifelse((
        OrganizationIdentifier == org_id &
          stringr::str_detect(MonitoringLocationIdentifier,
                              org_id,
                              negate = TRUE
          )),
        paste0(org_id, "-", MonitoringLocationIdentifier),
        MonitoringLocationIdentifier
      ))
    
    rm(au.info)
    
    if (length(au.crosswalk$MonitoringLocationIdentifier > 0)) {
      print(paste0(
        "TADA_GetATTAINSAUSiteCrosswalk: ",
        "There are ", nrow(au.crosswalk),
        " monitoring location identifiers associated with assessment units for ",
        org_id, " in ATTAINS."
      ))
      
      return(au.crosswalk)
    }
    
    if (length(au.crosswalk$MonitoringLocationIdentifier) == 0) {
      print(paste0(
        "TADA_GetATTAINSAUSiteCrosswalk: ",
        "No monitoring location identifiers were recorded in ATTAINS for ",
        org_id, " assessment units.", " No crosswalk can be returned."
      ))
      
      rm(au.crosswalk)
    }
  }
}



#' Create or Update Monitoring Location Identifier and Assessment Unit Identifier
#' Crosswalk in ATTAINS
#'
#' This function creates the batch upload files needed to create or update
#' Monitoring Location Identifiers in ATTAINS Assessment Unit profiles. Users
#' can specify whether all records should be overwritten (replaced) or if new
#' Monitoring Location Identifiers should be appended (added) to existing
#' records.
#'
#' ATTAINS batch upload files are available here:
#' https://www.epa.gov/waterdata/upload-data-resources-registered-attains-users#batch-upload-templates
#' See Assessment Unit Batch Upload Template.
#'
#' @param org_id Character argument. The ATTAINS organization identifier must
#' be supplied by the user. A list of organization identifiers can be found by
#' downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param.
#'
#' @param wqp_data_links Character argument. When wqp_data_links is equal to
#' "add" or "replace", the function will build the URL for the Water Quality
#' Portal Data Site page for each Monitoring Location Identifier in the data
#' frame. It will examine the response code of each URL and only retain those
#' with a 200 response, which indicates the URL is valid. When
#' wqp_data_links = "add", the URL will be added to any existing text in the
#' MS_DATA_LINK_TEXT column. When wqp_data_links = "replace", the URL will
#' replace any existing text in the MS_DATA_LINK_TEXT column. When
#' wqp_data_links = "none", no URLs will be created or added to the returned
#' data frame. Default is wqp_data_links = "add".
#'
#' @param attains_replace Character argument. When attains_replace = FALSE, all
#' Monitoring Location Identifiers in the user supplied crosswalk will be
#' appended to the existing ATTAINS crosswalk. When attains_replace = TRUE,
#' Monitoring Location Identifiers will only be retained if they are in the
#' user supplied crosswalk. Default equals FALSE.
#'
#' @param crosswalk A user-supplied dataframe with the columns
#' ASSESSMENT_UNIT_ID, MS_LOCATION_ID, MS_ORG_ID, and MONITORING_DATA_LINK_TEXT
#' is required. The ASSESSMENT_UNIT_ID and MS_LOCATION_ID must be filled out
#' in order to use this function. The additional columns,
#' MONITORING_DATA_LINK_TEXT, containing a single URL or "; " separated URLs
#' linking to information about the monitoring location, and MS_ORG_ID,
#' containing the WQP organization identifier for the monitoring location can
#' be left blank and the function will still run. Data link URLS to WQP site
#' pages cannot be automatically generated by this function unless the
#' MS_ORG_ID column is populated with the WQP OrganizationIdentifier. When
#' crosswalk = NULL, the crosswalk will be downloaded from ATTAINS. This allows
#' users to add URLs for the Water Quality Portal data site pages to the ATTAINS
#' assessment unit profile where possible without updating other information
#' in ATTAINS.
#'
#' @return A dataframe with four columns, MonitoringLocationIdentifier,
#' OrganizationIdentifier, ATTAINS.assessmentunitidentifier, and
#' MonitoringDataLinkText is returned. This matches the format of the batch
#' upload files required to add or update monitoring locations in ATTAINS.
#'
#' @seealso [TADA_GetATTAINSAUSiteCrosswalk()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Alaska example to updated data links with no user supplied crosswalk
#' AK_adddatalinks <- TADA_UpdateMonitoringLocationsInATTAINS(
#'   org_id = "AKDECWQ",
#'   crosswalk = NULL,
#'   attains_replace = FALSE,
#'   wqp_data_links = "replace"
#' )
#'
#' # Alaska example using a user supplied crosswalk to update entries in
#' # ATTAINS by appending user supplied information to ATTAINS crosswalk
#'
#' # example monitoring location identifiers
#' ASSESSMENT_UNIT_ID <- c(
#'   "AK_M_1021211_000", "AK_M_1021008_000",
#'   "AK_M_1021109_013", "AK_M_1021109_013",
#'   "AK_M_1021109_013"
#' )
#'
#' # example organization identifiers
#' MS_ORG_ID <- c("AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ", "AKDECWQ")
#'
#' # example assessment units
#' MS_LOCATION_ID <- c(
#'   "ExampleSite1", "ExampleSite2", "ExampleSite3",
#'   "ExampleSite4", "ExampleSite5"
#' )
#' # example urls
#' MONITORING_DATA_LINK_TEXT <- c(
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/",
#'   "https://www.waterqualitydata.us/provider/STORET/AKDECWQ/"
#' )
#'
#' # create example crosswalk data frame
#' ex.user.cw <- data.frame(
#'   MS_LOCATION_ID, MS_ORG_ID, ASSESSMENT_UNIT_ID,
#'   MONITORING_DATA_LINK_TEXT
#' )
#'
#' AK_appenduserdata <- TADA_UpdateMonitoringLocationsInATTAINS(
#'   org_id = "AKDECWQ",
#'   crosswalk = ex.user.cw,
#'   attains_replace = FALSE,
#'   wqp_data_links = "none"
#' )
#' }
#'
TADA_UpdateMonitoringLocationsInATTAINS <- function(org_id = NULL,
                                                    crosswalk = NULL,
                                                    attains_replace = FALSE,
                                                    wqp_data_links = "add") {
  # get list of organization identifiers from ATTAINS
  org.ref <- utils::read.csv(system.file("extdata", "ATTAINSOrgIDsRef.csv",
                                         package = "EPATADA"
  ))
  
  # stop function if organization identifiers is not found in ATTAINS
  if (!org_id %in% org.ref$code) {
    stop(paste0(
      "TADA_UpdateMonitoringLocationsInATTAINS: ",
      "The organization identifier entered by user is not found in ATTAINS."
    ))
  }
  
  if (is.null(crosswalk) & attains_replace == TRUE) {
    stop(paste0(
      "TADA_UpdateMonitoringLocationsInATTAINS: ",
      "in order to replace MonitoringLocations stored in ATTAINS ",
      "(with attains_replace = TRUE), user must provide a ",
      "MonitoringLocation/AssessmentUnitcrosswalk."
    ))
  }
  
  if (org_id %in% org.ref$code) {
    # remove intermediate object
    rm(org.ref)
    
    # if the crosswalk is not a dataframe, stop the function
    if (!is.data.frame(crosswalk) & !is.null(crosswalk)) {
      stop(paste0(
        "TADA_UpdateMonitoringLocationsInATTAINS: ",
        "A crosswalk dataframe with columns 'ATTAINS.assessmentunit.identifier' and ",
        "'MonitoringLocationIdentifier' or setting crosswalk = NULL is required to run ",
        "this function."
      ))
    }
    
    # check that crosswalk is a dataframe before proceeding
    if (is.data.frame(crosswalk)) {
      # check crosswalk has all of the required columns
      expected_cols <- c(
        "ASSESSMENT_UNIT_ID",
        "MS_LOCATION_ID",
        "MS_ORG_ID",
        "MONITORING_DATA_LINK_TEXT"
      )
      
      TADA_CheckColumns(crosswalk, expected_cols)
    }
    
    if (attains_replace == FALSE) {
      # create assessment unit crosswalk from ATTAINS
      attains.crosswalk <- TADA_GetATTAINSAUSiteCrosswalk(org_id = org_id) %>%
        dplyr::rename(
          ASSESSMENT_UNIT_ID = ATTAINS.assessmentunitidentifier,
          MS_ORG_ID = OrganizationIdentifier,
          MS_LOCATION_ID = MonitoringLocationIdentifier,
          MONITORING_DATA_LINK_TEXT = MonitoringDataLinkText
        )
      
      if (is.null(crosswalk)) {
        update.crosswalk <- attains.crosswalk
        
        
        rm(attains.crosswalk)
      }
      
      if (!is.null(crosswalk)) {
        # combine user supplied and attains crosswalks to create one crosswalk
        # no rows are omitted
        update.crosswalk <- attains.crosswalk %>%
          dplyr::full_join(crosswalk, by = dplyr::join_by(
            MS_LOCATION_ID,
            MS_ORG_ID,
            ASSESSMENT_UNIT_ID,
            MONITORING_DATA_LINK_TEXT
          )) %>%
          dplyr::distinct()
        
        rm(attains.crosswalk, crosswalk)
      }
    }
    
    # when replace is true, only rows in user-supplied crosswalk are used
    
    if (attains_replace == TRUE) {
      update.crosswalk <- crosswalk
      
      rm(attains.crosswalk, crosswalk)
    }
    
    # add Monitoring Location data links if wqp_data_links is not equal to "none"
    
    if (wqp_data_links != "none") {
      # get org/provider name ref
      
      provider.ref <- TADA_GetWQPOrgProviderRef() %>%
        dplyr::select(OrganizationIdentifier, ProviderName) %>%
        dplyr::distinct() %>%
        dplyr::rename(MS_ORG_ID = OrganizationIdentifier) %>%
        dplyr::mutate(OrgIDForURL = MS_ORG_ID)
      
      # add additional rows to account for the addition of "_WQX" to many org
      # names for WQP data
      add.orgs <- provider.ref %>%
        dplyr::filter(grepl("_WQX", MS_ORG_ID)) %>%
        dplyr::mutate(MS_ORG_ID = stringr::str_remove_all(
          OrgIDForURL,
          "_WQX"
        ))
      
      # combine provider refs
      provider.ref <- provider.ref %>%
        dplyr::bind_rows(add.orgs)
      
      # remove intermediate object
      rm(add.orgs)
      
      # join provider ref df to crosswalk
      update.crosswalk <- update.crosswalk %>%
        dplyr::left_join(provider.ref, by = dplyr::join_by(MS_ORG_ID))
      
      # next build the URLS for ms location urls
      update.crosswalk <- update.crosswalk %>%
        dplyr::mutate(MONITORING_DATA_LINK_TEXT.New = ifelse(
          is.na(OrgIDForURL), NA,
          URLencode(paste0(
            "https://www.waterqualitydata.us/provider/", ProviderName,
            "/", OrgIDForURL, "/", MS_LOCATION_ID, "/"
          ))
        ))
      
      # create df of urls to check
      urls.to.check <- update.crosswalk %>%
        dplyr::filter(!is.na(MONITORING_DATA_LINK_TEXT.New))
      
      # retrieve http response headers from url list
      headers <- urls.to.check$MONITORING_DATA_LINK_TEXT.New %>%
        purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))
      
      # extract response code from first line of header response
      response.code <- sapply(headers, "[[", 1)
      
      # create dataframe of urls and response codes
      response.df <- data.frame(urls.to.check, response.code) %>%
        dplyr::distinct()
      
      # join response codes to add.urls df
      update.crosswalk <- update.crosswalk %>%
        dplyr::left_join(response.df, by = names(update.crosswalk))
    }
    
    if (wqp_data_links == "replace") {
      update.crosswalk <- update.crosswalk %>%
        dplyr::mutate(MONITORING_DATA_LINK_TEXT = ifelse(
          grepl("200", response.code), MONITORING_DATA_LINK_TEXT.New,
          MONITORING_DATA_LINK_TEXT
        )) %>%
        dplyr::select(
          ASSESSMENT_UNIT_ID, MS_ORG_ID,
          MS_LOCATION_ID, MONITORING_DATA_LINK_TEXT
        ) %>%
        dplyr::distinct()
    }
    
    if (wqp_data_links == "add") {
      update.crosswalk <- update.crosswalk %>%
        dplyr::mutate(
          MONITORING_DATA_LINK_TEXT = ifelse(
            grepl("200", response.code),
            paste0(
              MONITORING_DATA_LINK_TEXT, "; ",
              MONITORING_DATA_LINK_TEXT.New
            ),
            MONITORING_DATA_LINK_TEXT
          ),
          MONITORING_DATA_LINK_TEXT = stringr::str_remove_all(
            MONITORING_DATA_LINK_TEXT,
            "NA, "
          )
        ) %>%
        tidyr::separate_rows(MONITORING_DATA_LINK_TEXT, sep = ", ") %>%
        dplyr::group_by(ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID) %>%
        suppressMessages(dplyr::summarise(
          MONITORING_DATA_LINK_TEXT =
            paste(
              unique(
                MONITORING_DATA_LINK_TEXT
              ),
              collapse = ", "
            )
        )) %>%
        dplyr::select(
          ASSESSMENT_UNIT_ID,
          MS_ORG_ID, MS_LOCATION_ID,
          MONITORING_DATA_LINK_TEXT
        ) %>%
        dplyr::distinct()
    }
    return(update.crosswalk)
  }
}



#' Create or Update ATTAINS, TADA/WQP/WQX, and EPA Criteria Search Tool (CST)
#' Parameter Name Crosswalk
#'
#' Use this function to help generate a crosswalk between each
#' ATTAINS.ParameterName used by a specific state or tribal nation and each
#' TADA.ComparableDataIdentifier present in the input TADA dataframe. The
#' crosswalk can be filled out by users within R or Excel. By default this
#' function will generate a user friendly Excel spreadsheet that includes a
#' drop down list list of all ATTAINS parameters that are applicable to the
#' organization selected by the function input 'org_id'. It also
#' highlights the cells in which users should input information. The excel
#' spreadsheet will be automatically downloaded to a user's downloads folder path.
#' Users may need to insert additional rows into the crosswalk if:
#' 1) an ATTAINS.ParameterName corresponds with multiple TADA.ComparableDataIdentifiers
#'    Example: An organization uses "ALUMINUM" for all aluminum related parameter causes
#'    but this ATTAINS.ParameterName may crosswalk to "ALUMINUM_TOTAL_NA_UG/L"
#'    for one use and "ALUMINUM_DISSOLVED_NA_UG/L" for another use; or
#' 2) an TADA.ComparableDataIdentifiers corresponds with multiple ATTAINS.ParameterNames.
#'    Example: An organization uses both "pH, HIGH" and "pH, LOW" as ATTAINS.ParameterNames,
#'    but both crosswalk to the same TADA.ComparableDataIdentifier, "PH_NA_NA_STD UNITS".
#'
#' Users who have already created an ATTAINS parameter and TADA/WQP characteristic
#' crosswalk can provide it as an input to this function. The user-supplied
#' crosswalk (dataframe entered into paramRef function input) must contain the
#' two required columns: TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
#' In addition, users who are interested in performing analyses for more than
#' one organization (multiple states and/or tribes) also need to include an additional column name:
#' 'ATTAINS.OrganizationIdentifier'. This ensures that the crosswalk between
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName are specific and
#' accurate for each organization. If a crosswalk has already been created in the
#' past and is entered into this function as a starting point, then any
#' TADA.ComparableDataIdentifiers that were previously matched
#' with ATTAINS parameters will be retained in the crosswalk, and any new
#' TADA.ComparableDataIdentifiers from the new input data frame will be added
#' to the crosswalk. Users can then focus on matching only the new
#' TADA.ComparableDataIdentifiers with applicable ATTAINS parameter names.
#'
#' Future development efforts may allow users to pull in magnitude values
#' for an ATTAINS parameter through the Criteria Search Tool depending on a
#' users quality control and review of these metrics.
#' The EPA TADA team created a draft crosswalk between characteristic
#' names (TADA.ComparableDataIdentifier) and EPA 304A pollutant names
#' (sourced from the Criteria Search Tool:
#' https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa)
#' This crosswalk only includes priority characteristics identified by the TADA
#' Working Group. You are welcome to reach out to the TADA team to ask for
#' additional matches to be included. You may run the following line of code
#' in the console to review this crosswalk:
#' 'CSTtoATTAINSParamCrosswalk <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))'.
#'
#' If no existing ATTAINS parameter name corresponds with a specific
#' TADA.ComparableDataIdentifier, users may contact the ATTAINS helpdesk
#' \email{attains@epa.gov} to inquire about adding the parameter. Users are
#' free to use any ATTAINS parameter name found in the ATTAINS parameter domain
#' value list, even if the parameter name  has not previously
#' been listed as a cause by the specific organization in the
#' past. The full list of ATTAINS parameter names can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' In the meantime, users can proceed by overriding the data validation in Excel
#' by value pasting. In that case, users will be warned in the
#' ATTAINS.FlagParameterName column that they choose to include an
#' ATTAINS.ParameterName that was not used by the selected organization in prior
#' ATTAINS assessment cycles.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' Organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this parameter. If a user supplied crosswalk is entered
#' into paramRef AND a user does not provide an org_id argument,
#' the function can identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If needed, please type the following into your R console:
#' file.path(Sys.getenv("USERPROFILE"), "Downloads") to ensure the file is downloaded
#' to the correct location. The file will be named "myfileRef.xlsx".
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param paramRef A dataframe which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier and ATTAINS.ParameterName. Users will need to
#' ensure this crosswalk contains the appropriate column names in order to
#' run the function. paramRef must contain at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName. Users who are
#' interested in performing analyses for more than
#' one organization (multiple states or tribes, or a single state/tribe and
#' EPA 304a criteria) also need to include an additional column name:
#' 'organization_identifier'.
#'
#' @param auto_assign A string value of "None", "All", or "Org". Default is "All".
#' If a user selects "All" this provides a match using TADA logic (IN DEVELOPMENT:
#' currently based on and exact match of WQP CharacteristicName with
#' ATTAINS ParameterName along with a few manual review). If "Org" then this
#' only provide the TADA logic match if your ATTAINS organization has included that
#' ATTAINS ParameterName in the past. If not, this will be left blank for your
#' organization to specify. "None" will result in an empty ATTAINS.ParameterName
#' column. Users will be required to fill this out on their own completely or
#' through a prior paramRef crosswalk. See paramRef argument input above for more
#' information.
#'
#' @return A excel file or data frame which contains the columns:
#' TADA.ComparableDataIdentifier, organization_identifier,
#' ATTAINS.ParameterName, and ATTAINS.FlagParameterName. Users will need to
#' complete the crosswalk between ATTAINS.ParameterName and
#' TADA.ComparableDataIdentifier.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This creates a blank paramRef template of UT Nutrients data.
#' # Users will need to fill this template out.
#' # Uncomment example below to generate Excel file
#' # (we recommended working on this in Excel):
#' # TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = TRUE)
#' # Example below generates the same output as a dataframe
#' paramRef_UT <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", auto_assign = "None", excel = FALSE
#' )
#' # Users can choose to edit the paramRef_UT through the R environment or in
#' # the excel spreadsheet. Users should be aware that any updates done only
#' # in the R environment will not reflect the 'ATTAINS.FlagParameterName' values
#' # correctly. If completed in R, we recommend users rerun this function
#' # to update the 'ATTAINS.FlagParameterName'.
#' # See below for a simple example of this workflow:
#'
#' # Manually add ATTAINS parameters to crosswalk using R
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT,
#'   ATTAINS.ParameterName = dplyr::case_when(
#'     grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'     grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'     grepl("NITROGEN", TADA.ComparableDataIdentifier) ~
#'       "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#'   )
#' )
#' # Update the 'ATTAINS.FlagParameterName' values
#' paramRef_UT3 <- TADA_CreateParamRef(Data_Nutrients_UT,
#'   paramRef = paramRef_UT2,
#'   org_id = "UTAHDWQ", auto_assign = "None", excel = FALSE
#' )
#'
#' # How does auto_assign = "All" compare to paramRef_UT3?
#' paramRef_UT4 <- TADA_CreateParamRef(Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", auto_assign = "All", excel = FALSE
#' )
#'
#' # Example where multiple org_id's are selected
#' # Retrieve data
#' shepherdstown <- TADA_DataRetrieval(
#'   startDate = "2022-01-01",
#'   endDate = "2025-12-31",
#'   huc = "02070004",
#'   applyautoclean = TRUE,
#'   ask = FALSE
#' )
#' # First, run key flag functions and harmonize synonyms across
#' # characteristic, fraction, and speciation columns
#' shepherdstown2 <- TADA_RunKeyFlagFunctions(shepherdstown)
#' shepherdstown3 <- TADA_HarmonizeSynonyms(shepherdstown2)
#' # Create ATTAINS parameter crosswalk for MD, VA, and PA
#' paramRef_shepherdstown <- TADA_CreateParamRef(shepherdstown3,
#'   org_id = c("MDE_EASP", "21VASWCB", "21PA"),
#'   auto_assign = "All",
#'   excel = FALSE
#' )
#' }
#'
TADA_CreateParamRef <- function(.data, org_id = NULL, paramRef = NULL, auto_assign = "None", # c("None", "All", "Org"),
                                excel = FALSE, overwrite = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
      "TADA_CreateParamRef: ",
      "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
      "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
    ))
  }
  
  # Ensures you have used a valid auto_assign name
  if (!auto_assign %in% c("None", "All", "Org")) {
    stop(paste0(
      "TADA_CreateParamRef: ",
      "argument input ", auto_assign, " is not a valid entry. Please type one of 'None', 'All', 'Org' as a value."
    ))
  }
  
  # if a user provides an org_id argument, it must be a character vector.
  if (!is.character(org_id) & is.null(org_id)) {
    stop("TADA.CreateParamRef: org_id must be a character vector")
  }
  
  # Allows for users to crosswalk parameters by multiple orgs.
  org_id <- as.list(org_id)
  
  # If  more than 1 org, it will create n duplicate rows for each TADA.ComparableDataIdentifier.
  if (length(org_id) > 1) {
    print(paste0(
      "TADA.CreateParamRef: More than one org_name was defined in your dataframe. ",
      "Generating duplicate rows of TADA.ComparableDataIdentifier for each org."
    ))
  }
  
  # Checks if your org is found in ATTAINS domain.
  org.ref <- utils::read.csv(system.file("extdata", "ATTAINSOrgIDsRef.csv", package = "EPATADA"))
  
  if (!sum(org_id %in% org.ref$code) == length(org_id)) {
    stop(paste0(
      "TADA_CreateParamRef: ",
      "One or more organization identifier(s) entered by user is not found in ATTAINS."
    ))
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop(paste0(
        "TADA_CreateParamRef: 'paramRef' must be a data frame with these 2 columns:",
        "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
      ))
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName"
      )
      
      ref.names <- names(paramRef)
      
      # Users are required to provide a parameter ref that contains
      # TADA.ComparableDataIdentifier and ATTAINS.ParameterName
      if (length(setdiff(col.names, ref.names)) > 0 &&
          !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
        stop(paste0(
          "TADA_CreateParamRef: 'paramRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
        ))
      }
    }
  }
  
  # If users don't provide TADA.ComparableDataIdentifier in their paramRef input,
  # crosswalk using TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText
  if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
    paramRef <- paramRef %>%
      dplyr::left_join(
        .data, c(
          "TADA.CharacteristicName", "TADA.MethodSpeciationName",
          "TADA.ResultSampleFractionText"
        )
      ) %>%
      dplyr::select(
        "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier",
        "ATTAINS.ParameterName", "ATTAINS.FlagParameterName"
      )
  }
  
  # 304a parameter name and standards are pulled in from the Criteria Search Tool (CST)
  # CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA"))
  
  # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
  TADA_param <- dplyr::distinct(
    .data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]
  ) %>%
    tidyr::uncount(weights = length(org_id)) %>%
    dplyr::mutate(ATTAINS.OrganizationIdentifier = as.character(rep(org_id, nrow(.) / length(org_id))))
  
  # Pulls in all domain values of parameter and use names in ATTAINS.
  ATTAINS_param_all <- utils::read.csv(
    system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA")
  ) 
  
  # Filters the full domain value by the specified org_id(s)
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) %>%
    dplyr::arrange(ATTAINS.ParameterName)
  
  # Should we stop or warn users in this step? We have chose to stop
  if (sum(!org_id %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
    stop(paste0(
      "TADA_CreateParamRef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS."
    ))
  }
  
  # If no paramRef is provided, the ATTAINS.ParameterName returns a blank column of NA that will need user input.
  if (tolower(auto_assign) == tolower("None")) {
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName # , EPA304A.PollutantName
      ) %>%
      dplyr::arrange(ATTAINS.OrganizationIdentifier) %>%
      dplyr::mutate(
        ATTAINS.ParameterName = as.character(NA),
        ATTAINS.FlagParameterName = "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment"
      ) %>%
      dplyr::mutate(
        Flag.ParameterInput =
          "Default. No Crosswalk was provided."
      ) %>%
      dplyr::distinct()
  }
  
  if (tolower(auto_assign) == tolower("All")) {
    print(paste0(
      "auto_assign == 'All' was selected, ",
      "finding an exact ATTAINS.ParameterName match for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
    ))
    ATTAINSParameterWQPCharRef <- utils::read.csv(system.file("extdata", "ATTAINSParameterWQPCharRef.csv", package = "EPATADA"))
    
    ATTAINSParameterWQPCharRef <- ATTAINSParameterWQPCharRef %>%
      dplyr::filter(ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName)
    
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) %>%
      dplyr::select(
        TADA.CharacteristicName, TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName # , EPA304A.PollutantName
      ) %>%
      dplyr::left_join(ATTAINSParameterWQPCharRef, by = c("TADA.CharacteristicName" = "CharacteristicName")) %>%
      dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName # , EPA304A.PollutantName
      ) %>%
      dplyr::arrange(ATTAINS.OrganizationIdentifier) %>%
      dplyr::mutate(
        ATTAINS.FlagParameterName = dplyr::case_when(
          ATTAINS.ParameterName == "No parameter match for TADA.ComparableDataIdentifier" | is.na(ATTAINS.ParameterName) ~
            "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
          !ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName ~
            "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
          ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName & !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
            "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
          paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
            "Parameter name is listed as a prior cause in ATTAINS for this organization."
        )
      ) %>%
      dplyr::mutate(
        Flag.ParameterInput = dplyr::if_else(
          !is.na(ATTAINS.ParameterName),
          "This crosswalk was provided through an exact match auto_assign = 'All', between ATTAINS.ParameterName and TADA.CharacteristicName.",
          "No Crosswalk was provided and no exact matches were found."
        )
      ) %>%
      dplyr::distinct()
  }
  
  if (tolower(auto_assign) == tolower("Org")) {
    print(paste0(
      "auto_assign == 'Org' was selected, ",
      "finding an exact ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
    ))
    
    ATTAINSParameterWQPCharRef <- utils::read.csv(system.file("extdata", "ATTAINSParameterWQPCharRef.csv", package = "EPATADA"))
    
    ATTAINSParameterWQPCharRef <- ATTAINSParameterWQPCharRef %>%
      dplyr::filter(ATTAINS.ParameterName %in% ATTAINS_param$ATTAINS.ParameterName)
    
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = as.character(NA)) %>%
      dplyr::select(
        TADA.CharacteristicName, TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName # , EPA304A.PollutantName
      ) %>%
      dplyr::left_join(ATTAINSParameterWQPCharRef, by = c("TADA.CharacteristicName" = "CharacteristicName")) %>%
      dplyr::mutate(ATTAINS.ParameterName = ATTAINS.ParameterName.y) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
        ATTAINS.ParameterName # , EPA304A.PollutantName
      ) %>%
      dplyr::arrange(ATTAINS.OrganizationIdentifier) %>%
      dplyr::mutate(
        ATTAINS.FlagParameterName = dplyr::case_when(
          ATTAINS.ParameterName == "No parameter match for TADA.ComparableDataIdentifier" | is.na(ATTAINS.ParameterName) ~
            "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
          !ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName ~
            "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List.",
          ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName & !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
            "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
          paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
            "Parameter name is listed as a prior cause in ATTAINS for this organization."
        )
      ) %>%
      # since auto_assign = Org matches only, then we must flag the parameter name, then only keep if it is a match
      dplyr::mutate(ATTAINS.ParameterName = dplyr::if_else(
        ATTAINS.FlagParameterName == "Parameter name is listed as a prior cause in ATTAINS for this organization.",
        ATTAINS.ParameterName,
        NA
      )) %>%
      dplyr::mutate(
        Flag.ParameterInput = dplyr::if_else(
          !is.na(ATTAINS.ParameterName),
          "This crosswalk was provided through an exact match auto_assign = 'Org', between ATTAINS.ParameterName and TADA.CharacteristicName.",
          "No Crosswalk was provided and no exact matches were found for this organization."
        )
      ) %>%
      dplyr::distinct()
  }
  
  if (!is.null(paramRef)) {
    # Identifies NEW rows in your current CreateParamRef data frame that are missing from your paramRef input -
    # i.e. current WQP Characteristics that you have not defined a crosswalk for
    Flag1 <- CreateParamRef %>%
      # anti_join will identify observations that exist in your 1st data frame, but not in the 2nd data frame.
      dplyr::anti_join(
        paramRef,
        by =
          c(
            "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier"
            # "ATTAINS.ParameterName", ATTAINS.FlagParameterName) # Exclude any dynamic values or possible NAs
          )
      ) %>%
      dplyr::mutate(
        Flag.ParameterInput =
          "Suspect: Your paramRef argument did not include this TADA.ComparableDataIdentifier. Please ensure this is not a new WQP Characteristic Name entry that needs to be crosswalked."
      )
    
    # identifies if a user has MODIFIED any ATTAINS.ParameterName values by TADA.ComparableDataIdentifier and ATTAINS.OrganizationIdentifier
    Flag2 <- paramRef %>%
      dplyr::anti_join(
        CreateParamRef,
        by = c(
          "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier"
          )
        ) %>%
      dplyr::mutate(
        Flag.ParameterInput =
          "This ATTAINS.ParameterName crosswalk was MODIFIED by your input(s) for this TADA.ComparableDataIdentifier."
      )
    
    CreateParamRef <- paramRef %>%
      dplyr::select("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName") %>%
      dplyr::full_join(
        Flag1 %>%
          dplyr::full_join(
            Flag2,
            by =
              c(
                "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier",
                "ATTAINS.ParameterName", "ATTAINS.FlagParameterName", "Flag.ParameterInput"
              )
          ),
        by = c("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName")
      ) %>%
      dplyr::mutate(Flag.ParameterInput = dplyr::if_else(is.na(ATTAINS.ParameterName), NA, Flag.ParameterInput)) %>%
      dplyr::rows_patch(CreateParamRef, by = "TADA.ComparableDataIdentifier") %>%
      dplyr::mutate(
        ATTAINS.FlagParameterName = dplyr::case_when(
          ATTAINS.ParameterName == "Not Applicable for Analysis." | is.na(ATTAINS.ParameterName) ~
            "No ATTAINS.ParameterName crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment.",
          !ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName ~
            "Parameter name is not included in ATTAINS, contact ATTAINS to add ATTAINS.ParameterName name to Domain List.",
          ATTAINS.ParameterName %in% ATTAINS_param_all$ATTAINS.ParameterName & !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
            "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
          paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName) ~
            "Parameter name is listed as a prior cause in ATTAINS for this organization"
        )
      ) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName,
        ATTAINS.FlagParameterName, Flag.ParameterInput
      ) %>%
      dplyr::distinct()
    
    # remove intermediate object Flag1
    rm(Flag1, Flag2)
  }
  
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if (excel == TRUE) {
    # Print message if there are many combinations of TADA Characteristic as it may slow run time.
    n <- nrow(dplyr::distinct(.data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]))
    if (n > 100 & excel == TRUE) {
      message(paste0("There are ", n, " unique TADA.ComparableDataIdentifier names in your TADA data frame.
    This may result in slow runtime for TADA_CreateParamRef() when generating the excel spreadsheet.
    Excel formulas will only be generated for the first 100 rows. Please fill down on Cells D1 and Cells E1 in excel
    to make all rows function dynamically (automatically updates the flag if a change was made to a crosswalk)."))
    }
    
    # Create column names for an empty dataframe
    columns <- c(
      "TADA.ComparableDataIdentifier",
      "ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier", "ATTAINS.FlagParameterName"
    )
    
    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
    colnames(par) <- columns
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "ATTAINSOrgNamesParamRef", visible = TRUE)
    openxlsx::addWorksheet(wb, "CreateParamRef", visible = TRUE)
    openxlsx::addWorksheet(wb, "Index", visible = FALSE)
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }
    
    # Format header and bodystyle
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    bodyStyle <- openxlsx::createStyle(wrapText = TRUE)
    
    # New row to rbind if a user selects "Not Applicable for Analysis."
    no_match_df <- data.frame(
      ATTAINS.OrganizationIdentifier = "NA", 
      ATTAINS.ParameterName = "Not Applicable for Analysis.",
      ATTAINS.UseName = "Not Applicable for Analysis."
    )
    
    # Index of allowable values for drop-down lists
    openxlsx::writeData(
      wb, "Index",
      startCol = 4,
      x = rbind(
        no_match_df,
        ATTAINS_param_all[, c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName")]
        %>% dplyr::arrange(ATTAINS.ParameterName)
      )
    )
    
    openxlsx::writeData(
      wb, "Index",
      startCol = 2,
      x = CreateParamRef[, c("ATTAINS.ParameterName", "Flag.ParameterInput")]
    )
    
    openxlsx::writeData(
      wb, "Index",
      startCol = 1,
      x = data.frame(
        "Not Applicable for Analysis.",
        ATTAINS.ParameterName = c(unique(ATTAINS_param$ATTAINS.ParameterName))
      )
    )
    
    openxlsx::writeData(
      wb, "CreateParamRef", 
      startCol = 1, 
      x = CreateParamRef, 
      headerStyle = header_st
      )
    
    # Creates a tab that contains the ATTAINS parameter-use filtered by the org_id input.
    openxlsx::writeData(
      wb, "ATTAINSOrgNamesParamRef", 
      startCol = 1, 
      x = ATTAINS_param, 
      headerStyle = header_st
      )
    
    # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab
    
    # Note: If we make edits to the data validation, please ensure the entire
    # data frame column is being referenced.
    # Ex. data validation will capture values in tab [Index] column h, for rows 2:50000 for input, value = sprintf("'Index'!$H$2:$H$50000")
    
    suppressWarnings(
      openxlsx::dataValidation(
        wb,
        sheet = "CreateParamRef",
        cols = 3, rows = 2:1000,
        type = "list",
        value = sprintf("'Index'!$E$2:$E$15000"), # please ensure this covers all values in the column E in the Index tab for future development.
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
      )
    )
    
    # remove intermediate objects
    rm(ATTAINS_param, ATTAINS_param_all)
    
    max_loops <- 0
    
    for (i in 1:nrow(CreateParamRef)) {
      max_loops <- max_loops + 1
      if (max_loops > 100) break
      
      openxlsx::writeFormula(
        wb, "CreateParamRef",
        startCol = 4,
        startRow = i + 1,
        array = TRUE,
        x = paste0(
          "=IF(OR(C", i + 1, '="",C', i + 1,
          '="Not Applicable for Analysis."),"No ATTAINS.ParameterName crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment",
          IF(ISNA(MATCH(C', i + 1, ',Index!E:E,0)),
            "Parameter name is not included in ATTAINS, contact ATTAINS to add ATTAINS.ParameterName name to Domain List.",
          IF(ISNA(MATCH(1,(C', i + 1, "=ATTAINSOrgNamesParamRef!D:D)*(B", i + 1, '=ATTAINSOrgNamesParamRef!A:A),0)),
            "Parameter name is listed as a prior cause in ATTAINS, but not for this organization.",
            "Parameter name is listed as a prior cause in ATTAINS for this organization.")))'
        )
      )
      
      openxlsx::writeFormula(
        wb, "CreateParamRef",
        startCol = 5,
        startRow = i + 1,
        array = TRUE,
        x = paste0(
          "IF(C", i + 1, "=Index!B$", i + 1, ",Index!C$", i + 1,
          ',"This ATTAINS.ParameterName crosswalk was MODIFIED by your input(s) for this TADA.ComparableDataIdentifier.")'
        )
      )
    }
    
    openxlsx::conditionalFormatting(
      wb, "CreateParamRef",
      cols = 3,
      rows = 1:nrow(CreateParamRef) + 1,
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )
    
    openxlsx::conditionalFormatting(
      wb, "CreateParamRef",
      cols = 3,
      rows = 1:nrow(CreateParamRef) + 1,
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    )
    
    # remove intermediate objects
    rm(TADA_param, max_loops)
    
    # Format column widths in CreateParamRef - for future considerations of formatting
    openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(CreateParamRef), widths = "auto")
    
    if (overwrite == TRUE) {
      message(
        paste0("Overwriting sheet [CreateParamRef] in ", downloads_path)
      )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      message(
        "If you would like to replace sheet [CreateParamRef], use overwrite = TRUE argument in TADA_CreateParamRef."
      )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  
  return(CreateParamRef)
}



#' Create or Update ATTAINS Parameter and Use crosswalk
#'
#' This function generates a crosswalk of all parameters and uses applicable
#' to the selected organization(s) in ATTAINS. Users should review and validate
#' each ATTAINS.ParameterName and associated ATTAINS.UseName combination.
#' As part of this review process, users should check to make sure each
#' 'ATTAINS.UseName' from the drop-down menu in the excel spreadsheet generated by this
#' function also accurately corresponds to the correct TADA.ComparableDataIdentifier
#' and ATTAINS.ParameterName found in the TADA dataframe. This function should be
#' run after creating a parameter (ATTAINS.ParameterName and
#' TADA.ComparableDataIdentifier) crosswalk.
#'
#' Before running this function, users must run TADA_CreateParamRef() to
#' create the crosswalk that defines the ATTAINS.ParameterName(s) and
#' ATTAINS.UseName(s) needing validation. All unique ATTAINS.UseNames from prior ATTAINS
#' assessment cycles are pulled in using TADA_CreateUseParamRef(). If a user has
#' defined multiple TADA.ComparableDataIdentifier matches to an
#' ATTAINS.ParameterName, they will need to define whether every
#' TADA.ComparableDataIdentifier matches to an associated ATTAINS.UseName.
#' If certain parameter and use combinations only apply to certain
#' TADA.ComparableDataIdentifier(s), users will need to select 'NA' or leave it
#' as blank to properly capture this logic.
#'
#' If an ATTAINS use name is not listed as a prior domain value for your organization
#' from prior ATTAINS assessment cycles, users can contact the ATTAINS helpdesk
#' \email{attains@epa.gov} to inquire about adding the use to the ATTAINS
#' domain list. Otherwise, users can still
#' proceed by overriding the data validation by value pasting in Excel.
#' Users will be warned in the ATTAINS.FlagUseName column if they choose to
#' include an ATTAINS use name that was not listed in prior ATTAINS assessment cycles as:
#' 'Use name is not listed as a prior cause in ATTAINS for this organization' or
#' 'Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name'.
#'
#' Note: Future development work will allow for crosswalking other names from the WQP
#' such as using pollutant names from the EPA's Criteria Search Tool (CST):
#' www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa.
#' The TADA Team has crosswalked the CST pollutant names for EPA304a standards with
#' TADA.ComparableDataIdentifier(s) to make the criteria values available for
#' use within TADA functions. The ATTAINS.UseName(s) associated with the EPA304a
#' criteria are included from the CST. All other ATTAINS.UseName(s) are specific to an
#' ATTAINS organization and come from the ATTAINS domain value for use_name.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param paramRef A dataframe which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier and ATTAINS.ParameterName. Users will need to
#' ensure this crosswalk contains the appropriate column names in order to
#' run the function. paramRef must contain at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName. Users who are
#' interested in performing analyses for more than
#' one organization (multiple states and/or tribes) also need to include an
#' additional column name: 'ATTAINS.OrganizationIdentifier'
#'
#' @param useParamRef A dataframe which contains a completed crosswalk of
#' organization specific ATTAINS.UseName(s) for each ATTAINS.ParameterName.
#' Users will need to ensure this crosswalk contains the appropriate column
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and review this output for accuracy.
#'
#' @param auto_assign NOTE: this has not been developed, will this be helpful?
#' A boolean value. If TRUE, this will assign all unique
#' use names to an ATTAINS.ParameterName that has not been defined by your
#' organization from ATTAINS. If FALSE, the values will be left blank and
#' will need you to manually assign use names as needed.
#'
#' @return A dataframe which contains the columns: TADA.ComparableDataIdentifier,
#' ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName,
#' and ATTAINS.FlagUseName. Users will need to review the crosswalk between
#' ATTAINS.ParameterName, ATTAINS.UseName and TADA.ComparableDataIdentifier.
#'
#' @seealso [TADA_CreateParamRef()]
#' @seealso [TADA_GetEPACSTRef()]
#'
#' @export
#'
#' @examples
#' # First, generate and fill out a parameter crosswalk (see TADA_CreateParamRef()):
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' paramRef_UT4 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   org_id = "UTAHDWQ", auto_assign = "All", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_CreateUseParamRef():
#' UseParamRef_UT <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Now, let's compare the crosswalk for paramRef_UT4 when we use auto_assign = "All".
#' # Notice, there are NA values for ATTAINS.UseName as these UT ATTAINS Parameter Name were
#' # not listed as a cause in prior ATTAINS assessment cycles.
#' UseParamRef_UT2 <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT4, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Let's test the "auto_assign" input
#' UseParamRef_UT3 <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT4, auto_assign = TRUE, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
TADA_CreateUseParamRef <- function(.data, org_id = NULL, paramRef = NULL, useParamRef = NULL,
                                   auto_assign = FALSE, excel = FALSE, overwrite = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
      "Argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
      "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
    ))
  }
  
  # Checks if paramRef argument contains a dataframe and necessary columns to proceed.
  if (is.null(paramRef)) {
    stop(paste0(
      "TADA.CreateUseParamRef: No paramRef argument provided."
    ))
  }
  
  # If a user does not fill in ANY values for the crosswalk of ATTAINS.ParameterName.
  # Users may want to proceed with only the EPA304a criteria crosswalk,
  # therefore we will allow users to proceed in this case.
  if (sum(!is.na(paramRef$ATTAINS.ParameterName)) == 0) {
    warning(paste0(
      "No values were found in ATTAINS.ParameterName.",
      "Please ensure that you have inputted all field values of interest in the",
      "ATTAINS.ParameterName column generated from TADA_CreateParamRef() function."
    ))
  }
  
  # If a user leaves at least one values for the crosswalk of ATTAINS.ParameterName blank.
  # Users are recommended to select 'No parameter match for this TADA.ComparableDataIdentifier' if
  # there is no crosswalk, but leaving it blank will be treated similarly.
  if (sum(is.na(paramRef$ATTAINS.ParameterName)) > 1) {
    print(paste0(
      "NAs were found in ATTAINS.ParameterName. ",
      "Please ensure that you have inputted all field values of interest in ",
      "the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function."
    ))
  }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and is filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop(paste0(
        "TADA_CreateUseParamRef: 'paramRef' must be a data frame with these 2 columns:",
        "TADA.ComparableDataIdentifier and ATTAINS.ParameterName."
      ))
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName"
      )
      
      ref.names <- names(paramRef)
      
      if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
        stop(paste0(
          "TADA_CreateUseParamRef: 'paramRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName."
        ))
      }
    }
  }
  
  # check to see if user-supplied parameter-use ref is a df with appropriate columns and is filled out.
  if (!is.null(useParamRef) & !is.character(useParamRef)) {
    if (!is.data.frame(useParamRef)) {
      stop(paste0(
        "TADA_CreateUseParamRef: 'UseParamRef' must be a data frame with these 5 columns:",
        "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
        "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
      ))
    }
    
    if (is.data.frame(useParamRef)) {
      col.names <- c(
        "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier",
        "ATTAINS.ParameterName", "ATTAINS.UseName", "IncludeOrExclude"
      )
      
      ref.names <- names(useParamRef)
      
      if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(useParamRef))) {
        stop(paste0(
          "TADA_CreateUseParamRef: 'useParamRef' must be a data frame with these 5 columns:",
          "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
          "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
        ))
      }
    }
  }
  
  .data <- as.data.frame(.data)
  
  # Pulls in all domain values of parameter and use names by orgs in ATTAINS.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  # Considers if we want to separate speciation, fraction, units as seprate columns in the future for crosswalk.
  if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
    paramRef <- paramRef %>%
      dplyr::left_join(
        .data,
        by = c(
          "TADA.CharacteristicName", "TADA.MethodSpeciationName",
          "TADA.ResultSampleFractionText"
        )
      ) %>%
      dplyr::select(
        "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier",
        "ATTAINS.ParameterName", "ATTAINS.FlagParameterName"
      )
  }
  
  # if user doesn't provide an org_id argument,
  # Selection of org_id will filter the drop down lists in future steps of creating the reference tables.
  if (is.null(org_id)) {
    stop("TADA.CreateUseParamRef: No organization identifier(s) provided.")
  }
  
  org_id <- as.list(org_id)
  
  # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_id.
  # 5/14/25 KW: We should use separate columns for CST organization/pollutant/use names in the future.
  if (sum(!org_id[tolower(org_id) != tolower("EPA304a")] %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
    stop(paste0(
      "TADA_CreateuseParamRef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS. "
    ))
  }
  
  # Checks if org_id are found in the user supplied paramRef argument.
  if (sum(!org_id[tolower(org_id) != tolower("EPA304a")] %in% paramRef$ATTAINS.OrganizationIdentifier) > 0) {
    stop(paste0(
      "TADA_CreateUseParamRef: ",
      "One or more organization identifiers entered by user is not found in your paramRef argument input. ",
      "Excluding those missing organization identifier(s) from output."
    ))
  }
  
  # Filters the ATTAINS parameter and use names by the org_id in user supplied df.
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::select(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %>%
    dplyr::filter(ATTAINS.ParameterName %in% paramRef$ATTAINS.ParameterName) %>%
    dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id)
  
  # Create the parameter-use reference table for validation
  CreateUseParamRef <- paramRef %>%
    dplyr::left_join(
      ATTAINS_param,
      by = c("ATTAINS.ParameterName", "ATTAINS.OrganizationIdentifier"),
      relationship = "many-to-many"
    ) %>%
    dplyr::select(
      TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
      ATTAINS.ParameterName, ATTAINS.UseName
    ) %>%
    # tidyr::drop_na(ATTAINS.ParameterName) %>%
    dplyr::filter(ATTAINS.ParameterName != "No parameter match for TADA.ComparableDataIdentifier") %>%
    dplyr::distinct() %>%
    dplyr::mutate(IncludeOrExclude = dplyr::if_else(
      is.na(ATTAINS.UseName),
      "Exclude",
      "Include"
    )) %>%
    dplyr::mutate(ATTAINS.FlagUseName = dplyr::if_else(
      is.na(ATTAINS.UseName),
      "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
      "Use name is listed as a prior cause in ATTAINS for this organization."
    )) %>%
    dplyr::mutate(
      Flag.UseInput =
        "Default: no modification was made to this row."
    )
  
  # If users want the EPA304a criteria. This pulls in the CST reference file.
  # Extracts the associated EPA304a pollutant names and its use_names.
  # if (tolower("epa304a") %in% tolower(org_id)) {
  #
  #   # handles case-insensitive inclusion of "epa304a" to "EPA304a" to pull in its magnitude
  #   org_id = c(org_id[tolower(org_id) != tolower("EPA304a")],"EPA304a")
  #
  #   CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
  #     dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, use_name) %>%
  #     dplyr::mutate(organization_identifier = "EPA304a")
  #
  #   EPA_param <- CreateUseParamRef %>%
  #     dplyr::left_join(CST_param, c("EPA304A.PollutantName"), relationship = "many-to-many") %>%
  #     dplyr::select(
  #       TADA.ComparableDataIdentifier,
  #       organization_identifier = organization_identifier.y,
  #       ATTAINS.ParameterName, EPA304A.PollutantName, use_name = use_name.y
  #     ) %>%
  #     dplyr::distinct()
  #
  #   # remove intermediate object CST_param
  #   rm(CST_param)
  #
  #   CreateUseParamRef <- CreateUseParamRef %>%
  #     dplyr::ungroup() %>%
  #     dplyr::full_join(
  #       EPA_param,
  #       by = c(
  #         "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName",
  #         "ATTAINS.OrganizationIdentifier", "EPA304A.PollutantName", "use_name"
  #       )
  #     ) %>%
  #     dplyr::select(
  #       TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
  #       EPA304A.PollutantName, ATTAINS.ParameterName, use_name
  #     ) %>%
  #     dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id)
  #
  #   # remove intermediate object EPA_param
  #   rm(EPA_param)
  # }
  
  if (auto_assign == TRUE) {
    print(paste0(
      "auto_assign == TRUE was selected, ",
      "assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an ",
      "organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed."
    ))
    
    use.names <- CreateUseParamRef %>%
      dplyr::select(ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
      tidyr::drop_na() %>%
      dplyr::distinct()
    
    CreateUseParamRef_temp <- CreateUseParamRef %>%
      dplyr::filter(is.na(ATTAINS.UseName)) %>%
      dplyr::left_join(use.names, by = c("ATTAINS.OrganizationIdentifier"), relationship = "many-to-many") %>%
      dplyr::mutate(ATTAINS.UseName = dplyr::coalesce(ATTAINS.UseName.x, ATTAINS.UseName.y)) %>%
      dplyr::select(-c(ATTAINS.UseName.x, ATTAINS.UseName.y)) %>%
      # dplyr::mutate(TADA.ComparableDataIdentifier = dplyr::coalesce(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
      # dplyr::select(-c(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
      dplyr::mutate(IncludeOrExclude = "Include") %>%
      dplyr::mutate(
        Flag.UseInput =
          "This row was MODIFIED by your input(s)."
      )
    
    CreateUseParamRef <- CreateUseParamRef %>%
      # dplyr::select(TADA.ComparableDataIdentifier, ATTAINS.ParameterName, ATTAINS.OrganizationIdentifier) %>%
      dplyr::filter(!is.na(ATTAINS.UseName)) %>%
      dplyr::full_join(CreateUseParamRef_temp, by = c("ATTAINS.ParameterName", "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude", "ATTAINS.FlagUseName", "Flag.UseInput")) %>%
      dplyr::mutate(ATTAINS.FlagUseName = dplyr::case_when(
        paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) ~
          "Use name is listed as a prior cause in ATTAINS for this organization.",
        !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) &
          ATTAINS.UseName %in% ATTAINS_param_all$ATTAINS.UseName ~
          "Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name.",
        is.na(ATTAINS.UseName) ~
          "No use name is provided. Consider choosing an appropriate ATTAINS.UseName."
      )) %>%
      dplyr::mutate(TADA.ComparableDataIdentifier = dplyr::coalesce(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
      dplyr::select(-c(TADA.ComparableDataIdentifier.x, TADA.ComparableDataIdentifier.y)) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
        IncludeOrExclude, ATTAINS.FlagUseName, Flag.UseInput
      ) %>%
      dplyr::arrange(match(IncludeOrExclude, c("Include")), ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
      dplyr::distinct()
  }
  
  if (!is.null(useParamRef)) {
    # identifies if a user has excluded any useParam rows. This row is showing up as a new entry but has not been defined.
    # This should flag users that they need to review this entry and if they
    # truly want to exclude it or not. What should the default be?
    Flag1 <- CreateUseParamRef %>%
      dplyr::anti_join(
        useParamRef,
        by =
          c(
            "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName"
            # "ATTAINS.UseName", "IncludeOrExclude", "ATTAINS.FlagUseName"
            )
        ) %>%
      dplyr::mutate(
        Flag.UseInput =
          "Suspect: Your useParamRef argument did not include this TADA.ComparableDataIdentifier. Please ensure you have provided all ATTAINS.UseName and ATTAINS.ParameterName combinations in your input."
      ) %>%
      dplyr::mutate(IncludeOrExclude = "Exclude")
    
    # identifies if a user has MODIFIED any useParam rows.
    Flag2 <- useParamRef %>%
      dplyr::anti_join(
        CreateUseParamRef, 
        by = c(
          "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
          "IncludeOrExclude" # , "ATTAINS.FlagUseName"
          )
      ) %>%
      dplyr::mutate(
        Flag.UseInput =
          "This row was MODIFIED by your input(s)."
      )
    
    
    CreateUseParamRef <- useParamRef %>%
      dplyr::select("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude", "ATTAINS.ParameterName", "ATTAINS.UseName") %>%
      dplyr::full_join(
        Flag1 %>%
          dplyr::full_join(
            Flag2,
            by =
              c(
                "TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude",
                "ATTAINS.ParameterName", "ATTAINS.UseName", "ATTAINS.FlagUseName", "Flag.UseInput"
              )
          ),
        by = c("TADA.ComparableDataIdentifier", "ATTAINS.OrganizationIdentifier", "IncludeOrExclude", "ATTAINS.ParameterName", "ATTAINS.UseName")
      ) %>%
      dplyr::mutate(ATTAINS.FlagUseName = dplyr::case_when(
        is.na(ATTAINS.UseName) ~
          "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
        paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) ~
          "Use name is listed as a prior cause in ATTAINS for this organization.",
        !paste(ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName) %in% paste(ATTAINS_param_all$ATTAINS.OrganizationIdentifier, ATTAINS_param_all$ATTAINS.ParameterName, ATTAINS_param_all$ATTAINS.UseName) &
          ATTAINS.UseName %in% ATTAINS_param_all$ATTAINS.UseName ~
          "Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name."
      )) %>%
      dplyr::mutate(Flag.UseInput = dplyr::case_when(
        is.na(Flag.UseInput) ~
          "Default: no modification was made to this row.",
        !is.na(Flag.UseInput) ~
          Flag.UseInput
      )) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName, ATTAINS.UseName,
        IncludeOrExclude, ATTAINS.FlagUseName, Flag.UseInput
      ) %>%
      dplyr::arrange(match(IncludeOrExclude, c("Include")), ATTAINS.OrganizationIdentifier, ATTAINS.UseName) %>%
      dplyr::distinct()
    
    # remove intermediate objects
    rm(Flag1, Flag2)
  }
  
  # remove intermediate objects
  rm(ATTAINS_param)
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if (excel == TRUE) {
    # Print message if there are many combinations of TADA Characteristic as it may slow run time.
    n <- nrow(CreateUseParamRef)
    if (n > 100) {
      message(paste0("There are ", n, " rows in your CreateUseParamRef.
    This may result in slow runtime for TADA_CreateUseParamRef() when generating the excel spreadsheet.
    Excel formulas will only be generated for the first 100 rows. Please fill down on Cells F1 and Cells G1 in excel
    to make all rows function dynamically (automatically updates the flag if a change was made to a crosswalk)."))
    }
    
    # Create column names for an empty dataframe
    columns <- c(
      "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName",
      "ATTAINS.FlagParameterName", "ATTAINS.FlagUseName"
    )
    
    # empty dataframe with just column names
    par <- data.frame(matrix(nrow = 0, ncol = length(columns)))
    colnames(par) <- columns
    
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    
    # If a user chooses to rerun the TADA_CreateUseParamRef() function,
    # the sheet will already exist and error.
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateUseParamRef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateUseParamRef")
        openxlsx::addWorksheet(wb, "CreateUseParamRef")
      }
    )
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Export CreateUseParamRef dataframe into the excel spreadsheet tab
    openxlsx::writeData(wb, "CreateUseParamRef", startCol = 1, x = CreateUseParamRef, headerStyle = header_st)
    
    # Index of allowable values for drop-down lists
    openxlsx::writeData(wb, "Index", startCol = 9, x = data.frame("IncludeOrExclude" = c("Include", "Exclude")))
    
    openxlsx::writeData(wb, "Index", startCol = 7, x = CreateUseParamRef[, c("ATTAINS.FlagUseName", "Flag.UseInput")])
    
    # Data validation drop down list created below.
    # Note: ATTAINSOrgNamesParamRef contains the list of prior param and use cause by org names specific.
    # Since Use Names are individual to each Organization.
    suppressWarnings(
      # Data validation for ATTAINS.UseName.
      openxlsx::dataValidation(
        wb,
        sheet = "CreateUseParamRef",
        cols = 4, rows = 2:1000,
        type = "list",
        value = sprintf("'ATTAINSOrgNamesParamRef'!$E$2:$E$50000"),
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
      )
    )
    
    suppressWarnings(
      # Data validation for "Include" or "Exclude" values.
      openxlsx::dataValidation(
        wb,
        sheet = "CreateUseParamRef",
        cols = 5, rows = 2:1000,
        type = "list",
        value = sprintf("'Index'!$I$2:$I$5"),
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
      )
    )
    
    max_loops <- 0
    
    for (i in 1:nrow(CreateUseParamRef)) {
      # Formula based cell values in excel.
      openxlsx::writeFormula(
        wb, "CreateUseParamRef",
        startCol = 6, startRow = i + 1,
        array = TRUE,
        x = paste0(
          "=IF(E", i + 1, '="Exclude",
            "Use name does not apply for this ATTAINS.ParameterName. Excluding this use name from analysis.",
          IF(ISBLANK(D', i + 1, '),
            "No use name is provided. Consider choosing an appropriate ATTAINS.UseName.",
          IF(ISNA(MATCH(1,(D', i + 1, "=ATTAINSOrgNamesParamRef!E:E)*(B", i + 1, '=ATTAINSOrgNamesParamRef!A:A),0)),
            "Use name is not listed as a prior cause in ATTAINS for this organization.",
          IF(ISNA(MATCH(1,(C', i + 1, "=ATTAINSOrgNamesParamRef!D:D)*(D", i + 1, "=ATTAINSOrgNamesParamRef!E:E)*(B", i + 1, '=ATTAINSOrgNamesParamRef!A:A),0)),
            "Use name is listed as a prior cause in ATTAINS for this organization, but not for this parameter name.",
            "Use name is listed as a prior cause in ATTAINS for this organization."))))'
        )
      )
      
      openxlsx::writeFormula(
        wb, "CreateUseParamRef",
        startCol = 7,
        startRow = i + 1,
        array = TRUE,
        x = paste0(
          "IF(F", i + 1, "=Index!G$", i + 1, ",Index!H$", i + 1,
          ',"This row was MODIFIED by your input(s).")'
        )
      )
      max_loops <- max_loops + 1
      if (max_loops > 100) break
    }
    
    # Conditional formatting created below.
    
    # If a user has left an ATTAINS.UseName blank, flag as a red cell.
    openxlsx::conditionalFormatting(
      wb, "CreateUseParamRef",
      cols = 4, rows = 1:nrow(CreateUseParamRef) + 1,
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )
    
    # If a user has an ATTAINS.UseName filled out, flag as a yellow cell.
    openxlsx::conditionalFormatting(
      wb, "CreateUseParamRef",
      cols = 4, rows = 1:nrow(CreateUseParamRef) + 1,
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    )
    
    # If a user has chose to Exclude a use name for a parameter, flag as a red cell.
    openxlsx::conditionalFormatting(
      wb, "CreateUseParamRef",
      cols = 5, rows = 1:nrow(CreateUseParamRef) + 1,
      type = "contains",
      rule = c("Exclude"),
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
    )
    
    # If a user has chose to Include a use name for a parameter, flag as a yellow cell.
    openxlsx::conditionalFormatting(
      wb, "CreateUseParamRef",
      cols = 5, rows = 1:nrow(CreateUseParamRef) + 1,
      type = "contains",
      rule = c("Include"),
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    )
    
    # Format Column widths
    openxlsx::setColWidths(wb, "CreateUseParamRef", cols = 1:ncol(CreateUseParamRef), widths = "auto")
    
    # Handles overwriting the excel file.
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      warning(
        "If you would like to replace [CreateUseParamRef], use overwrite = TRUE argument in TADA_CreateUseParamRef"
      )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  return(CreateUseParamRef)
}



#' Apply Uses and Unique Site Criteria to Monitoring Location Sites or Assessment Units
#'
#' This function will pull in all unique MonitoringLocationName, MonitoringLocationType,
#' and MonitoringLocationIdentifier from the TADA dataframe. Users are not required
#' to provide a crosswalk between WQP Monitoring locations and Assessment units
#' if they are only interested in summarizing assessments on a monitoring location level.
#'
#' If users are interested in summarizing water quality data results by Assessment Units,
#' users should utilize TADA Module 2 tools to assist in their monitoring location
#' to assessment unit crosswalk prior to this step (see TADA_CreateWaterUseParamRef
#' and TADA_CreateUseAURef).
#' Users can choose to submit a TADA dataframe that contains
#' the concatenated columns from TADA_GetATTAINS and will utilize the crosswalk of
#' monitoring locations to assessment units performed in this function (please check out
#' the TADAModule2.Rmd for an example workflow for TADA_GetATTAINS.)
#'
#' Users may also submit their own monitoring location to assessment unit crosswalk
#' reference file which contains the appropriate column names as an argument input. This
#' reference file will be prioritized as a crosswalk
#'
#' Users are expected to modify this AU ref table with the appropriate AU and
#' MonitoringLocationName, MonitoringLocationType, and MonitoringLocationId crosswalk
#' for the current Assessment cycle. Users will decide to "Include" or "Exclude"
#' a MonitoringLocation in the "IncludeOrExclude" column. for an AU.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param waterUseParamRef An option data frame input. If provided, this data frame
#' should contain a completed crosswalk of any unique spatial criteria applied to
#' a water body, use, or parameter and by any combinations if needed. Users will
#' need to ensure this crosswalk contains the appropriate column names in order to
#' run the function. See output of [TADA_CreateWaterUseParamRef()] for column names.
#'
#' @param useAURef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with an assessment unit.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function. See output of [TADA_CreateUseAURef()] for column names.
#'
#' @param sitesAURef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. Users will need to ensure this crosswalk contains the
#' appropriate column names in order to run the function.
#' See module 2 vignette and sample output of [TADA_GetATTAINS()].
#'
#' @param useParamRef A required data frame which contains a completed crosswalk of
#' organization specific ATTAINS.UseName(s) for each ATTAINS.ParameterName.
#' Users will need to ensure this crosswalk contains the appropriate column
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and review this output for accuracy.
#'
#' @param spatialRef An optional data frame which contains the completed spatial
#' crosswalk to assign any unique spatial criteria to a parameter, use, waterbody
#' or monitoring site/assessment unit.
#'
#' @return A data frame with any unique spatial descriptions defined for
#'
#' @seealso [TADA_CreateUseParamRef()]
#' @seealso [TADA_CreateUseAURef()]
#' @seealso [TADA_CreateWaterUseParamRef()]
#'
#' @export
#'
#' @examples
#' # First, generate and fill out a parameter crosswalk (see TADA_CreateParamRef()):
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_CreateUseParamRef():
#' UseParamRef_UT <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Now, run TADA_CreateSpatialRef()
#' SpatialRef_UT <- TADA_CreateSpatialRef(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   waterUseParamRef = NULL, useAURef = NULL, sitesAURef = NULL,
#'   useParamRef = UseParamRef_UT,
#'   excel = FALSE
#' )
#'
TADA_CreateSpatialRef <- function(.data, org_id = NULL, waterUseParamRef = NULL, useAURef = NULL, useParamRef = NULL,
                                  sitesAURef = NULL, spatialRef = NULL,
                                  # applyUniqueSpatial = NULL, applyToWater = NULL,
                                  # applyToParam = NULL, applyToUse = NULL,
                                  # applyToAU = NULL, applyToML = NULL,
                                  excel = FALSE, overwrite = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
      "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
      "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
    ))
  }
  
  # Creates the data frame.
  CreateSpatialRef <- data.frame()
  
  # default Downloads file location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # This allows a user to provide the mod 2 function TADA_GetATTAINS() as the .data data frame.
  # In this case, the ML to AU crosswalk is generated from TADA_GetATTAINS().
  if (!is.data.frame(.data)) {
    if (!any(c(
      "TADA_with_ATTAINS", "ATTAINS_catchments", "ATTAINS_points", "ATTAINS_lines", "ATTAINS_polygons"
    ) %in% names(.data))) {
      stop("Your input dataframe was not produced from `TADA_GetATTAINS()` or it was modified. Please create your list of ATTAINS features using `TADA_GetATTAINS(return_sf = TRUE)`")
    }
    # .data <- .data[["TADA_with_ATTAINS"]]
  }
  
  # check to see if user-supplied UseAURef is a df with appropriate columns and is filled out.
  if (!is.null(useAURef) & !is.character(useAURef)) {
    if (!is.data.frame(useAURef)) {
      stop(paste0(
        "TADA_CreateSpatialRef: 'useAURef' must be a data frame with these 3 columns:",
        "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.assessmentunitidentifier"
      ))
    }
    
    if (is.data.frame(useAURef)) {
      col.names <- c(
        "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier"
      )
      
      ref.names <- names(useAURef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop(paste0(
          "TADA_CreateSpatialRef: 'useAURef' must be a data frame with these 3 columns:",
          "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.assessmentunitidentifier"
        ))
      }
    }
  }
  
  # check to see if user-supplied useParamRef ref is a df with appropriate columns and filled out.
  if (!is.null(useParamRef) & !is.character(useParamRef)) {
    if (!is.data.frame(useParamRef)) {
      stop(paste0(
        "TADA_CreateSpatialRef: 'useParamRef' must be a data frame with these 5 columns:",
        "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
        "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
      ))
    }
    
    if (is.data.frame(useParamRef)) {
      col.names <- c(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName"
      )
      
      ref.names <- names(useParamRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop(paste0(
          "TADA_CreateSpatialRef: 'useParamRef' must be a data frame with these 5 columns:",
          "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
          "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
        ))
      }
    }
  }
  
  useParamRef <- dplyr::filter(useParamRef, IncludeOrExclude == "Include")
  
  # If there are no siteAURef, this will return the Spatial Ref Table on a monitoring sites level.
  if (is.null(sitesAURef)) {
    print("No sitesAURef was provided. Creating SpatialRef table on a monitoring sites level. NAs are generated for any ATTAINS AU columns.")
    CreateSpatialRef <- useParamRef %>%
      dplyr::left_join(.data, by = c("TADA.ComparableDataIdentifier"), relationship = "many-to-many") %>%
      dplyr::mutate(ATTAINS.assessmentunitname = NA) %>%
      dplyr::mutate(ATTAINS.assessmentunitidentifier = NA) %>%
      dplyr::mutate(ATTAINS.waterTypeCode = NA) %>%
      dplyr::mutate(ApplyUniqueSpatialCriteria = NA) %>%
      dplyr::mutate(IncludeOrExclude = "Include") %>%
      dplyr::mutate(Flag.AssessmentNote = "Default: No spatial criteria applied.") %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.assessmentunitidentifier, ATTAINS.assessmentunitname,
        MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName,
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.waterTypeCode,
        LongitudeMeasure, LatitudeMeasure, Flag.AssessmentNote, IncludeOrExclude, ApplyUniqueSpatialCriteria
      ) %>%
      dplyr::distinct()
  }
  
  # If a user DOES provide a sitesAURef, this will create the Spatial Table on an AU level
  if (!is.null(sitesAURef)) {
    # If user does not provide a UseAURef, run it to pull in prior uses for AU,
    # Otherwise, if a user has already customized this and provided this useAURef, then use that table.
    if (is.null(useAURef)) {
      # Pulls in UseAURef
      print("A sitesAURef was provided, but no UseAURef was provided. Running TADA_CreateUseAURef to pull in all prior use names for your AU.")
      useAURef <- TADA_CreateUseAURef(.data, sitesAURef = sitesAURef, org_id = org_id, excel = FALSE)
    }
    
    if (is.null(waterUseParamRef)) {
      print("A sitesAURef was provided, but no waterUseParamRef was provided. Running TADA_CreateWaterUseParamRef to pull in all prior use names for your AU.")
      waterUseParamRef <- TADA_CreateWaterUseParamRef(.data, useParamRef = useParamRef, useAURef = useAURef, org_id = org_id, excel = FALSE)
    }
    
    # Only keep rows that have include
    useAURef <- useAURef %>%
      dplyr::filter(IncludeOrExclude == "Include") %>%
      dplyr::select(-IncludeOrExclude)
    
    waterUseParamRef <- waterUseParamRef %>%
      dplyr::filter(IncludeOrExclude == "Include") %>%
      dplyr::select(-IncludeOrExclude)
    
    # Joins the crosswalk tables for CreateSpatialRef
    CreateSpatialRef <- waterUseParamRef %>%
      dplyr::right_join(useAURef, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.UseName")) %>%
      dplyr::left_join(sitesAURef, by = c("ATTAINS.assessmentunitidentifier", "ATTAINS.assessmentunitname")) %>%
      dplyr::mutate(
        Flag.AssessmentNote =
          dplyr::case_when(
            # commented out. Check & review these logic:
            # ATTAINS.OrganizationIdentifier != "EPA304a" & !is.na(ATTAINS.assessmentunitidentifier) & is.na(MonitoringLocationIdentifier) ~
            #   "Suspect: No ML assigned to this AU. You did not define a crosswalk for this ML to AU.",
            # ATTAINS.OrganizationIdentifier == "EPA304a" & !is.na(ATTAINS.assessmentunitidentifier) & !is.na(MonitoringLocationIdentifier) ~
            #   "Pass: This is an EPA304a standard, but was not assigned to an AU/ML/WaterType classfication.",
            is.na(ATTAINS.OrganizationIdentifier) & !is.na(ATTAINS.assessmentunitidentifier) & !is.na(MonitoringLocationIdentifier) ~
              "Suspect: No organization identifier provided for this AU/ML/WaterType. This row may not be relevant for assessment",
            is.na(MonitoringLocationIdentifier) ~
              "Suspect: No monitoring location identifier(s) assigned to this Assessment Unit.",
            .default = "Default: No spatial criteria applied."
          )
      ) %>%
      dplyr::mutate(IncludeOrExclude = "Include") %>%
      dplyr::mutate(ApplyUniqueSpatialCriteria = NA) %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.assessmentunitidentifier, ATTAINS.assessmentunitname,
        MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName,
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.waterTypeCode,
        LongitudeMeasure, LatitudeMeasure, Flag.AssessmentNote, IncludeOrExclude, ApplyUniqueSpatialCriteria
      ) %>%
      dplyr::filter(MonitoringLocationIdentifier %in% .data$MonitoringLocationIdentifier) %>%
      dplyr::distinct()
  }
  
  # if(!is.null(applyUniqueSpatial)){
  #   df <- list(applyToWater, applyToParam, applyToUse, applyToAU, applyToML)
  #
  #   n <- max(lengths(df))
  #
  #   if(sum(lapply(df,length) > 0) == 0){
  #     stop("You have specificed an 'applyUniqueSpatial' Criteria vector but did not apply this to any columns. Please specified where you would like to apply these unique spatial criteria.")
  #   }
  #
  #   if(sum(lapply(df,length) > 0) > 0){
  #   df2 <- data.frame(
  #     "ApplyUniqueSpatialCriteria" = applyUniqueSpatial,
  #     expand.grid(list(applyUniqueSpatial, "ATTAINS.assessmentunitidentifier" = if(is.null(applyToAU))  NA else applyToAU))[2],
  #     expand.grid(list(applyUniqueSpatial, "MonitoringLocationIdentifier" = if(is.null(applyToML))  NA else applyToML))[2],
  #     expand.grid(list(applyUniqueSpatial, "ATTAINS.ParameterName" = if(is.null(applyToParam))  NA else applyToParam))[2],
  #     expand.grid(list(applyUniqueSpatial, "ATTAINS.UseName" = if(is.null(applyToUse))  NA else applyToUse))[2],
  #     expand.grid(list(applyUniqueSpatial, "ATTAINS.waterTypeCode" = if(is.null(applyToWater))  NA else applyToWater))[2]
  #   )
  #
  #   }
  #
  #   CreateSpatialRef <- dplyr::left_join(CreateSpatialRef, df2, by = c(
  #     "ATTAINS.assessmentunitidentifier", "MonitoringLocationIdentifier", "ATTAINS.ParameterName",
  #     "ATTAINS.UseName", "ATTAINS.waterTypeCode"
  #     ))
  # }
  
  if (!"ATTAINS.assessmentunitidentifier" %in% colnames(CreateSpatialRef)) {
    print(paste0(
      "No Monitoring Location to Assessment Unit crosswalk provided. ",
      "Consider providing this crosswalk if you would like to summarize assessments on an Assessment Unit level."
    ))
  }
  
  # User provides their own spatialRef that has been filled out.
  if (!is.null(spatialRef)) {
    # identifies if a user has excluded any spatial rows. This row is showing up as a new entry but has not been defined.
    # should this be a suspect or named something else? This should flag users that they need to review this entry and if they
    # truly want to exclude it or not. What should the default be?
    Flag1 <- CreateSpatialRef %>%
      dplyr::anti_join(spatialRef,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier", "ATTAINS.assessmentunitname",
                           "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName",
                           "ATTAINS.ParameterName", "ATTAINS.UseName", "ATTAINS.waterTypeCode",
                           "LongitudeMeasure", "LatitudeMeasure" # , "IncludeOrExclude", "ApplyUniqueSpatialCriteria"
                         )
      ) %>%
      dplyr::mutate(
        Flag.AssessmentNote =
          "Suspect: The spatial criteria for this row was REMOVED from your provided spatial reference."
      ) %>%
      dplyr::mutate(IncludeOrExclude = "Exclude")
    
    # identifies if a user has ADDED on any spatial rows.
    Flag2 <- spatialRef %>%
      dplyr::anti_join(CreateSpatialRef,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier", "ATTAINS.assessmentunitname",
                           "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName",
                           "ATTAINS.ParameterName", "ATTAINS.UseName", "ATTAINS.waterTypeCode",
                           "LongitudeMeasure", "LatitudeMeasure", "IncludeOrExclude", "ApplyUniqueSpatialCriteria"
                         )
      ) %>%
      dplyr::mutate(
        Flag.AssessmentNote =
          "The spatial criteria for this row was ADDED from your spatial reference"
      )
    
    CreateSpatialRef <- CreateSpatialRef %>%
      dplyr::select(-ApplyUniqueSpatialCriteria) %>%
      dplyr::full_join(Flag1,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier", "ATTAINS.assessmentunitname",
                           "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName",
                           "ATTAINS.ParameterName", "ATTAINS.UseName", "IncludeOrExclude", "ATTAINS.waterTypeCode",
                           "LongitudeMeasure", "LatitudeMeasure"
                         )
      ) %>%
      dplyr::full_join(Flag2,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier", "ATTAINS.assessmentunitname",
                           "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName",
                           "ATTAINS.ParameterName", "ATTAINS.UseName", "IncludeOrExclude", "ATTAINS.waterTypeCode",
                           "LongitudeMeasure", "LatitudeMeasure"
                         )
      ) %>%
      dplyr::mutate(Flag.AssessmentNote = dplyr::coalesce(Flag.AssessmentNote, Flag.AssessmentNote.x, Flag.AssessmentNote.y)) %>%
      dplyr::select(-c(Flag.AssessmentNote.x, Flag.AssessmentNote.y)) %>%
      dplyr::mutate(ApplyUniqueSpatialCriteria = dplyr::coalesce(ApplyUniqueSpatialCriteria.x, ApplyUniqueSpatialCriteria.y)) %>%
      dplyr::select(-c(ApplyUniqueSpatialCriteria.x, ApplyUniqueSpatialCriteria.y)) %>%
      dplyr::select(
        ATTAINS.OrganizationIdentifier, ATTAINS.assessmentunitidentifier, ATTAINS.assessmentunitname,
        MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName,
        ATTAINS.ParameterName, ATTAINS.UseName, ATTAINS.waterTypeCode,
        LongitudeMeasure, LatitudeMeasure, Flag.AssessmentNote, IncludeOrExclude, ApplyUniqueSpatialCriteria
      ) %>%
      dplyr::arrange(match(IncludeOrExclude, c("Include"))) %>%
      dplyr::distinct()
  }
  
  # Only run if user wants to create an excel guided spreadsheet.
  if (excel == TRUE) {
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateSpatialRef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateSpatialRef")
        openxlsx::addWorksheet(wb, "CreateSpatialRef")
      }
    )
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    
    # Format Column widths
    openxlsx::setColWidths(
      wb, "CreateSpatialRef",
      cols = 8:ncol(CreateSpatialRef),
      widths = "auto"
    )
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }
    
    # writes CreateSpatialRef dataframe
    openxlsx::writeData(
      wb, "CreateSpatialRef",
      startCol = 1,
      x = CreateSpatialRef,
      headerStyle = header_st
    )
    
    # data validation drop down list created below.
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "CreateSpatialRef", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$5"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    
    # Conditional Formatting
    openxlsx::conditionalFormatting(
      wb, "CreateSpatialRef",
      cols = 13, rows = 2:(nrow(CreateSpatialRef) + 1),
      type = "contains",
      rule = "Include",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # default values or indicates good to go cells.
    
    openxlsx::conditionalFormatting(
      wb, "CreateSpatialRef",
      cols = 13, rows = 2:(nrow(CreateSpatialRef) + 1),
      type = "contains",
      rule = "Exclude",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    # conditionalFormatting(wb, "CreateSpatialRef",
    #                       cols = 8, rows = 2:(nrow(CreateSpatialRef) + 1),
    #                       type = "notContains", rule = c("Exclude","Include"), style = createStyle(bgFill = "red")) # Likely error. Invalid value is possible here.
    openxlsx::conditionalFormatting(
      wb, "CreateSpatialRef",
      cols = 14, rows = 2:(nrow(CreateSpatialRef) + 1),
      type = "blanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # green is default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb, "CreateSpatialRef",
      cols = 14, rows = 2:(nrow(CreateSpatialRef) + 1),
      type = "notBlanks",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
    
    CreateSpatialRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateSpatialRef")
  }
  
  return(CreateSpatialRef)
}



#' Apply Water Type, Use Name, and Parameter Name Combination
#'
#' This is a helper function to TADA_CreateSpatialRef and is meant to
#' help users with reviewing all water type, use name, and parameter name combination. 
#' This will require users to have provided a sitesAURef argument created from 
#' TADA_CreateUseAURef to run. This function will help to assign ATTAINS use names 
#' to any new or modified assessment unit provided from a user's sitesAURef if there are any.
#'
#' This function will assume all use names for an existing AU in prior assessment cycles
#' are being done for an organization's assessment. It will filter down the useParam list to
#' only valid use names done in prior assessment cycle for an AU. It will assume any parameter and
#' use name defined in useParamRef are being assessed - ie we only join useParamRef and useAURef by
#' use name.
#'
#' Users are expected to modify this AU ref file with the appropriate AU and
#' MonitoringLocationName/MonitoringLocationType/MonitoringLocationId crosswalk
#' for the current Assessment cycle. Users can decide to "Include or Exclude" a MonitoringLocation
#' within an AU if desired. This can be used if a MonitoringLocation would still like to be
#' crosswalk to the AU but may only be applicable for certain parameters.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param useAURef An option data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with an assessment unit.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function. See output of [TADA_CreateUseAURef()] for column names.
#'
#' @param useParamRef A required data frame which contains a completed crosswalk of
#' organization specific ATTAINS.UseName(s) for each ATTAINS.ParameterName.
#' Users will need to ensure this crosswalk contains the appropriate column
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and review this output for accuracy.
#'
#' @param waterUseParamRef An option data frame input. If provided, this data frame
#' should contain a completed crosswalk of any unique spatial criteria applied to
#' a water body, use, or parameter and by any combinations if needed. Users will
#' need to ensure this crosswalk contains the appropriate column names in order to
#' run the function. See output of [TADA_CreateWaterUseParamRef()] for column names.
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#'
#' @seealso [TADA_CreateUseParamRef()]
#' @seealso [TADA_CreateUseAURef()]
#' @seealso [TADA_CreateSpatialRef()]
#'
#' @export
#'
#' @examples
#' # First, generate and fill out a parameter crosswalk (see TADA_CreateParamRef()):
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
#'   grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
#'   grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' # Next, enter the crosswalk generated above as the paramRef function input
#' # for TADA_CreateUseParamRef():
#' UseParamRef_UT <- TADA_CreateUseParamRef(
#'   Data_Nutrients_UT,
#'   paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
#' )
#'
#' # Generate our ML to AU crosswalk. See Module 2 for more information
#' # Preload Data_Nutrients_UT_GetATTAINS which already contains output from TADA_GetATTAINS.
#' UT_AU_ML <- Data_Nutrients_UT_GetATTAINS
#'
#' # Now, run TADA_CreateUseAURef()
#' UseAURef_UT <- TADA_CreateUseAURef(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   sitesAURef = UT_AU_ML,
#'   excel = FALSE
#' )
#'
#' # Now, run TADA_CreateWaterUseParamRef()
#' WaterUseParamRef_UT <- TADA_CreateWaterUseParamRef(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   useAURef = UseAURef_UT,
#'   useParamRef = UseParamRef_UT,
#'   excel = FALSE
#' )
#'
TADA_CreateWaterUseParamRef <- function(.data, useParamRef = NULL, useAURef = NULL, waterUseParamRef = NULL,
                                        org_id = NULL, excel = FALSE, overwrite = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
      "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
      "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
    ))
  }
  
  # default Downloads file location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # Pulls in all domain values of parameter and use names by orgs in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  # check to see if user-supplied UseAURef is a df with appropriate columns and is filled out.
  if (!is.null(useAURef) & !is.character(useAURef)) {
    if (!is.data.frame(useAURef)) {
      stop(paste0(
        "TADA_CreateWaterUseParamRef: 'useAURef' must be a data frame with these 3 columns:",
        "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.assessmentunitidentifier"
      ))
    }
    
    if (is.data.frame(useAURef)) {
      col.names <- c(
        "ATTAINS.UseName", "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier"
      )
      
      ref.names <- names(useAURef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop(paste0(
          "TADA_CreateWaterUseParamRef: 'useAURef' must be a data frame with these 3 columns:",
          "ATTAINS.UseName, ATTAINS.OrganizationIdentifier and ATTAINS.assessmentunitidentifier"
        ))
      }
    }
  }
  
  # check to see if user-supplied useParamRef ref is a df with appropriate columns and filled out.
  if (!is.null(useParamRef) & !is.character(useParamRef)) {
    if (!is.data.frame(useParamRef)) {
      stop(paste0(
        "TADA_CreateWaterUseParamRef: 'useParamRef' must be a data frame with these 5 columns:",
        "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
        "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
      ))
    }
    
    if (is.data.frame(useParamRef)) {
      col.names <- c(
        "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName", "ATTAINS.UseName"
      )
      
      ref.names <- names(useParamRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop(paste0(
          "TADA_CreateWaterUseParamRef: 'useParamRef' must be a data frame with these 5 columns:",
          "TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier, ",
          "ATTAINS.ParameterName, ATTAINS.UseName, IncludeOrExclude"
        ))
      }
    }
  }
  
  # If org_id argument is not provided, this will attempt to pull in org_id from TADA_GetATTAINS.
  if (is.null(org_id)) {
    print(
      "TADA_CreateWaterUseParamRef: No organization identifier(s) provided.",
      "Attempting to pull in organization identifiers found in the TADA data frame.",
      "Please ensure that you have ran TADA_GetATTAINS if you did not provide an org_id argument input."
    )
    print(
      "Users should provide a list of ATTAINS organization state or tribal name",
      "that pertains to their assessment."
    )
    TADA_CheckColumns(.data, "ATTAINS.organizationname")
    org_id <- unique(stats::na.omit(.data[, "ATTAINS.organizationname"]))
  }
  # if user doesn't provide an org_id argument, the function extracts the unique org_id from TADA_GetATTAINS().
  # Users will need to have ran TADA_GetATTAINS() for this option to be allowed. Selection of org_id will filter the drop down lists in future steps of creating the reference tables.
  if (is.null(org_id)) {
    stop("TADA_CreateWaterUseParamRef: No organization identifier(s) provided.")
  }
  
  # Handle later, if multiple org_id are used, create a loop when calling rATTAINS (or if we use EQ National extract, no loop needed)
  # org_id <- as.list(org_id)
  
  # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_id.
  if (sum(!org_id[org_id != "EPA304a"] %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
    warning(paste0(
      "TADA_CreateWaterUseParamRef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS. "
    ))
  }
  
  CreateWaterUseParamRef <- useParamRef %>%
    dplyr::left_join(
      useAURef,
      by = c(
        # "ATTAINS.ParameterName" = "parameterName",
        "ATTAINS.OrganizationIdentifier",
        "ATTAINS.UseName" = "ATTAINS.UseName"
      ), relationship = "many-to-many"
    ) %>%
    dplyr::select(
      ATTAINS.OrganizationIdentifier, ATTAINS.ParameterName,
      # ATTAINS.assessmentunitidentifier = assessment_unit_identifier, ATTAINS.assessmentunitname,
      ATTAINS.UseName, waterType
    ) %>%
    dplyr::bind_cols(
      data.frame(
        IncludeOrExclude = as.character("Include")
      )
    ) %>%
    dplyr::distinct() %>%
    # dplyr::filter(waterType %in% unique(OrgID_assessments$waterType)) %>%
    dplyr::filter(!is.na(ATTAINS.OrganizationIdentifier)) %>%
    dplyr::mutate(
      TADA.FlagAssessment =
        dplyr::if_else(
          is.na(waterType) & ATTAINS.OrganizationIdentifier != "EPA304a",
          "Suspect: This parameter and use was not listed as a prior cause for any of your Assessment Units of interest", "Pass"
        )
    ) %>%
    dplyr::arrange(waterType, ATTAINS.UseName)
  
  # User provides their own waterUseParamRef that has been filled out and wants to re-use/re-run flagging of their crosswalk table.
  if (!is.null(waterUseParamRef)) {
    # identifies if a user has provided a row combinations that is potentially not relevant to their assessments.
    # i.e. a user has included a parameter, ATTAINS.UseName, and waterType combination that is not found in prior ATTAINS assessment cycles.
    Flag1 <- CreateWaterUseParamRef %>%
      dplyr::anti_join(waterUseParamRef,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName",
                           "ATTAINS.UseName", "waterType", "IncludeOrExclude", "TADA.FlagAssessment"
                         )
      ) %>%
      dplyr::mutate(TADA.FlagAssessment = dplyr::case_when(
        (!ATTAINS.ParameterName %in% CreateWaterUseParamRef$ATTAINS.ParameterName &
           !ATTAINS.UseName %in% CreateWaterUseParamRef$ATTAINS.UseName &
           !waterType %in% CreateWaterUseParamRef$waterType) ~
          "Suspect: Exclude from assessment. This parameter, ATTAINS.UseName, and waterType combination is not defined in your waterUseParamRef"
      )) %>%
      dplyr::mutate(IncludeOrExclude = "Exclude")
    
    CreateWaterUseParamRef <- Flag1 %>%
      dplyr::full_join(waterUseParamRef,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName",
                           "ATTAINS.UseName", "waterType", "IncludeOrExclude", "TADA.FlagAssessment"
                         )
      ) %>%
      dplyr::mutate(
        TADA.FlagAssessment =
          dplyr::case_when(
            is.na(waterType) & ATTAINS.OrganizationIdentifier != "EPA304a" ~
              "Suspect: This parameter and use was not listed as a prior cause for any of your Assessment Units of interest",
            .default = as.character(TADA.FlagAssessment)
          )
      ) %>%
      dplyr::arrange(match(IncludeOrExclude, c("Include")), waterType, ATTAINS.UseName) %>%
      dplyr::distinct()
  }
  
  if (excel == TRUE) {
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateWaterUseParamRef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateWaterUseParamRef")
        openxlsx::addWorksheet(wb, "CreateWaterUseParamRef")
      }
    )
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(wb, "CreateWaterUseParamRef", cols = 1:ncol(CreateWaterUseParamRef), widths = "auto")
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }
    
    # writes CreateSpatialRef dataframe
    openxlsx::writeData(wb, "CreateWaterUseParamRef", startCol = 1, x = CreateWaterUseParamRef, headerStyle = header_st)
    
    # data validation drop down list created below.
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "CreateWaterUseParamRef", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$5"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    # Conditional Formatting
    openxlsx::conditionalFormatting(
      wb, "CreateWaterUseParamRef",
      cols = 5, rows = 2:(nrow(CreateWaterUseParamRef) + 1),
      type = "contains",
      rule = "Include",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb, "CreateWaterUseParamRef",
      cols = 5, rows = 2:(nrow(CreateWaterUseParamRef) + 1),
      type = "contains",
      rule = "Exclude",
      style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateParamRef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
    
    CreateWaterUseParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateWaterUseParamRef")
  }
  
  return(CreateWaterUseParamRef)
}



#' Assessment Unit and Use Name Crosswalk
#'
#' This function will pull in all prior use names associated with an organization identifier
#' assessment units from prior ATTAINS cycles.
#'
#' Note that any new or modified AU information must be provided from the user's end if
#' they decide to incorporate this in their summaries as we
#' cannot query use names from AU that are not shown in ATTAINS yet.
#' NOTE: Future development looks to allow users to submit their own
#' AU shapefiles into TADA module 2 functions to allow for
#' matching ML to these new/modified AU.
#'
#' This function requires users to provide a crosswalk between ML and AU.
#' The output from TADA_GetATTAINS(.data, return_sf = FALSE) can be used directly
#' as the sitesAURef argument input in this function.
#'
#' Users are expected to modify this AU ref file with the appropriate AU and
#' MonitoringLocationName/MonitoringLocationType/MonitoringLocationId crosswalk
#' for the current Assessment cycle. Users can decide to "Include or Exclude" a MonitoringLocation
#' within an AU if desired. This can be used if a MoniotringLocation would still like to be
#' crosswalk to the AU but may only be applicable for certain parameters. Users
#' can choose to add new parameters and uses as needed.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning,
#' processing, harmonization, filtering, and handling of censored data functions
#' prior to running this function.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
#' organization identifiers are listed in the "OrgName" tab.
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument,
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path.
#' If you have any trouble locating the file, please type the following into
#' your R console to locate it: file.path(Sys.getenv("USERPROFILE"), "Downloads").
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight
#' the cells in which users should input information.
#'
#' @param overwrite A Boolean value that ensures the function will not overwrite
#' the user supplied crosswalk entered into this function via the paramRef
#' function input. This helps prevent users from overwriting their progress.
#'
#' @param sitesAURef An option data frame input. If provided, this data frame
#' should contain a completed crosswalk of monitoring location sites associated
#' with an assessment unit. Users will need to ensure this crosswalk contains the
#' appropriate column names in order to run the function.
#' See module 2 vignette and sample output of [TADA_GetATTAINS()].
#'
#' @param useAURef An optional data frame input. If provided, this data frame
#' should contain a completed crosswalk of use names associated with an assessment unit.
#' Users will need to ensure this crosswalk contains the appropriate column names in
#' order to run the function.
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for each defined AU.
#'
#' @export
#'
#' @examples
#' # First, we will generate our ML to AU crosswalk. See Module 2 for more information
#' # Preload Data_Nutrients_UT_GetATTAINS which already contains output from TADA_GetATTAINS.
#' # UT_AU_ML <- TADA_GetATTAINS(Data_Nutrients_UT, return_sf = FALSE)
#' UT_AU_ML <- Data_Nutrients_UT_GetATTAINS
#'
#' # Now, run TADA_CreateUseAURef()
#' UseAURef_UT <- TADA_CreateUseAURef(
#'   Data_Nutrients_UT,
#'   org_id = c("UTAHDWQ"),
#'   sitesAURef = UT_AU_ML,
#'   excel = FALSE
#' )
#'
TADA_CreateUseAURef <- function(.data, sitesAURef = NULL, useAURef = NULL,
                                org_id = NULL, excel = FALSE, overwrite = FALSE, assign_all = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
      "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
      "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
    ))
  }
  
  # default Downloads file location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  # rExpertQuery API key for TADA
  tadakey <- "EKtgCrmatyP4G8iFgADMIfwlddbpDlSqRxetlN09"
  
  # Pulls in all domain values of parameter and use names by orgs in ATTAINS. Filtering by state is done in the next steps.
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  # check to see if user-supplied sitesAURef is a df with appropriate columns and is filled out.
  if (!is.null(sitesAURef) & !is.character(sitesAURef)) {
    if (!is.data.frame(sitesAURef)) {
      stop(paste0(
        "TADA_CreateUseAURef: 'sitesAURef' must be a data frame with these 3 columns:",
        "MonitoringLocationIdentifier, ATTAINS.organizationid and ATTAINS.assessmentunitidentifier"
      ))
    }
    
    if (is.data.frame(sitesAURef)) {
      col.names <- c(
        "MonitoringLocationIdentifier", "OrganizationIdentifier", "ATTAINS.assessmentunitidentifier"
      )
      
      ref.names <- names(sitesAURef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop(paste0(
          "TADA_CreateUseAURef: 'sitesAURef' must be a data frame with these 3 columns:",
          "MonitoringLocationIdentifier, OrganizationIdentifier and ATTAINS.assessmentunitidentifier"
        ))
      }
    }
  }
  
  # if user doesn't provide an org_id argument
  if (is.null(org_id)) {
    stop("TADA_CreateUseAURef: No organization identifier(s) provided.")
  }
  
  # Handle later, if multiple org_id are used, create a loop when calling rATTAINS (or if we use EQ National extract, no loop needed)
  # org_id <- as.list(org_id)
  
  # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_id.
  if (sum(!org_id[org_id != "EPA304a"] %in% ATTAINS_param_all$ATTAINS.OrganizationIdentifier) > 0) {
    warning(paste0(
      "TADA_CreateUseAURef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS. "
    ))
  }
  
  OrgID_assessments <- suppressMessages(rExpertQuery::EQ_Assessments(org_id = org_id, api_key = tadakey))
  
  OrgID_assessments <- dplyr::filter(OrgID_assessments, assessmentUnitId %in% unique(sitesAURef$ATTAINS.assessmentunitidentifier))
  
  CreateUseAURef <- sitesAURef %>%
    dplyr::left_join(
      OrgID_assessments,
      by = c(
        "ATTAINS.assessmentunitidentifier" = "assessmentUnitId",
        "ATTAINS.organizationid" = "organizationId"
      ),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      TADA.AssessmentUnitStatus =
        dplyr::if_else(
          ATTAINS.assessmentunitidentifier %in% unique(OrgID_assessments$assessmentUnitId), "Existing", "New"
        )
    ) %>%
    dplyr::bind_cols(
      data.frame(
        IncludeOrExclude = as.character("Include")
      )
    ) %>%
    dplyr::select(
      ATTAINS.OrganizationIdentifier = ATTAINS.organizationid, ATTAINS.assessmentunitidentifier, ATTAINS.assessmentunitname,
      ATTAINS.UseName = useName, waterType, TADA.AssessmentUnitStatus, IncludeOrExclude
    ) %>%
    dplyr::filter(ATTAINS.OrganizationIdentifier %in% org_id) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct() %>%
    dplyr::arrange(ATTAINS.assessmentunitidentifier, ATTAINS.UseName)
  
  # User provides their own useAURef that has been filled out.
  if (!is.null(useAURef)) {
    # What rows did the user have in their useAURef that was not found in the most recent ATTAINS data system?
    Flag1 <- CreateUseAURef %>%
      dplyr::anti_join(useAURef,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier", "ATTAINS.assessmentunitname",
                           "ATTAINS.UseName", "waterType", "TADA.AssessmentUnitStatus", "IncludeOrExclude"
                         )
      ) %>%
      dplyr::mutate(TADA.AssessmentUnitStatus = dplyr::case_when(
        !ATTAINS.assessmentunitidentifier %in% sitesAURef$ATTAINS.assessmentunitidentifier ~ "New",
        ATTAINS.assessmentunitidentifier %in% sitesAURef$ATTAINS.assessmentunitidentifier ~
          "Suspect: Excluding from Assessment. This AU is not found in your useAURef"
      )) %>%
      dplyr::mutate(IncludeOrExclude = dplyr::case_when(
        ATTAINS.assessmentunitidentifier %in% sitesAURef$ATTAINS.assessmentunitidentifier ~
          "Exclude"
      ))
    
    CreateUseAURef <- Flag1 %>%
      dplyr::full_join(useAURef,
                       by =
                         c(
                           "ATTAINS.OrganizationIdentifier", "ATTAINS.assessmentunitidentifier", "ATTAINS.assessmentunitname",
                           "ATTAINS.UseName", "waterType", "TADA.AssessmentUnitStatus", "IncludeOrExclude"
                         )
      ) %>%
      dplyr::mutate(TADA.AssessmentUnitStatus = dplyr::case_when(
        !ATTAINS.assessmentunitidentifier %in% sitesAURef$ATTAINS.assessmentunitidentifier ~ "New",
        TRUE ~ TADA.AssessmentUnitStatus
      )) %>%
      dplyr::arrange(match(IncludeOrExclude, c("Include")), waterType, ATTAINS.UseName) %>%
      dplyr::distinct()
  }
  
  if (excel == TRUE) {
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateUseAURef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateUseAURef")
        openxlsx::addWorksheet(wb, "CreateUseAURef")
      }
    )
    
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(wb, "CreateUseAURef", cols = 1:ncol(CreateUseAURef), widths = "auto")
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    n_sheets <- length(wb$worksheets)
    for (i in 1:n_sheets) {
      sV <- wb$worksheets[[i]]$sheetViews
      wb$worksheets[[i]]$sheetViews <- set_zoom(90)
    }
    
    # writes CreateSpatialRef dataframe
    openxlsx::writeData(wb, "CreateUseAURef", startCol = 1, x = CreateUseAURef, headerStyle = header_st)
    
    # data validation drop down list created below.
    suppressWarnings(openxlsx::dataValidation(wb, sheet = "CreateUseAURef", cols = 9, rows = 2:1000, type = "list", value = sprintf("'Index'!$B$2:$B$5"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
    
    # Conditional Formatting
    openxlsx::conditionalFormatting(
      wb, "CreateUseAURef",
      cols = 6, rows = 2:(nrow(CreateUseAURef) + 1),
      type = "contains", rule = "Existing", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb, "CreateUseAURef",
      cols = 6, rows = 2:(nrow(CreateUseAURef) + 1),
      type = "contains", rule = "New", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    openxlsx::conditionalFormatting(
      wb, "CreateUseAURef",
      cols = 6, rows = 2:(nrow(CreateUseAURef) + 1),
      type = "contains", rule = "Dropped/Existing", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    
    openxlsx::conditionalFormatting(
      wb, "CreateUseAURef",
      cols = 7, rows = 2:(nrow(CreateUseAURef) + 1),
      type = "contains", rule = "Include", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[9])
    ) # default values or indicates good to go cells.
    openxlsx::conditionalFormatting(
      wb, "CreateUseAURef",
      cols = 7, rows = 2:(nrow(CreateUseAURef) + 1),
      type = "contains", rule = "Exclude", style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
    ) # using yellow to indicate modified cell
    
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      warning("If you would like to replace the file, use overwrite = TRUE argument in TADA_CreateUseAUef")
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
    
    CreateUseAURef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateUseAURef")
  }
  
  return(CreateUseAURef)
}
