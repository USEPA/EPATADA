#' Get WQP/WQX MonitoringLocationIdentifier and ATTAINS.assessmentunitidentifier
#' Crosswalk from ATTAINS
#'
#' Tribes and States who participate in electronic reporting of water quality
#' conditions through EPA ATTAINS may also submit a crosswalk of WQP
#' MonitoringLocationIdentifiers associated with their Assessment Units to
#' ATTAINS. If the organization has recorded MonitoringLocationIdentifiers
#' associated with their Assessment Units in ATTAINS, this function can be used
#' to create a crosswalk of known MonitoringLocationIdentifiers and Assessment
#' Units. All tribal nations record this crosswalk in ATTAINS but only a few
#' states. If a state has not supplied Monitoring Location information to
#' ATTAINS, the function will not return a data frame.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the
#' user. A list of organization identifiers can be found by downloading the
#' ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx.
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param.
#'
#' @return A dataframe with three columns, ATTAINS.assessmentunitidentifier,
#' MonitoringLocationIdentiifer, and OrganizationIdentifier is returned.
#' This is the crosswalk between WQP/WQX MonitoringLocationIdentifiers and
#' Assessment Units that the state or tribal organization submitted to ATTAINS.
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

    au.info <- rATTAINS::assessment_units(organization_id = org_id)

    au.crosswalk <- au.info %>%
      tidyr::unnest(monitoring_stations) %>%
      dplyr::select(
        monitoring_location_identifier, monitoring_organization_identifier,
        assessment_unit_identifier, monitoring_data_link_text
      ) %>%
      dplyr::filter(!is.na(monitoring_location_identifier)) %>%
      dplyr::distinct() %>%
      dplyr::rename(
        ATTAINS.assessmentunitidentifier = assessment_unit_identifier,
        MonitoringLocationIdentifier = monitoring_location_identifier,
        OrganizationIdentifier = monitoring_organization_identifier,
        MonitoringDataLinkText = monitoring_data_link_text
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
        " WQP MonitoringLocationIdentifiers associated with Assessment Units for ",
        org_id, " in ATTAINS."
      ))

      return(au.crosswalk)
    }

    if (length(au.crosswalk$MonitoringLocationIdentifier) == 0) {
      print(paste0(
        "TADA_GetATTAINSAUSiteCrosswalk: ",
        "No MonitoringLocationIdentifiers were recorded in ATTAINS for ",
        org_id, " Assessment Units.", " No crosswalk can be returned."
      ))

      rm(au.crosswalk)
    }
  }
}

#' Add or Update Monitoring Location Identifiers in ATTAINS
#'
#' This function creates the batch upload files needed to add or update
#' Monitoring Location Identifiers in ATTAINS Assessment Unit profiles. Users
#' can specify whether all records should be overwritten (replaced) or if new
#' Monitoring Location Identifiers should be appended (added) to existing
#' records.
#'
#' @param org_id Character argument. The ATTAINS organization identifier must
#' be supplied by the user. A list of organization identifiers can be found by
#' downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx.
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
#' linking to information about the Monitoring Location, and MS_ORG_ID,
#' containing the WQP organization identifier for the Monitoring Location can
#' be left blank and the function will still run. Data link URLS to WQP site
#' pages cannot be automatically generated by this function unless the
#' MS_ORG_ID column is populated with the WQP OrganizationIdentifier. When
#' crosswalk = NULL, the crosswalk will be downloaded from ATTAINS. This allows
#' users to add URLs for the Water Quality Portal Data Site pages to the ATTAINS
#' Assessment Unit profile where possible without updating other information
#' in ATTAINS.
#'
#' @return The csv batch upload files for ATTAINS to add or update
#' Monitoring Locations.
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



#' ATTAINS Parameter Name and TADA/WQP Parameter Name Crosswalk
#'
#' To use TADA functions to compare WQP data with numeric criteria, 
#' users are required to provide a crosswalk between ATTAINS parameter names 
#' used by their organization and each TADA.ComparableDataIdentifier present 
#' in the TADA dataframe. This function creates a draft
#' crosswalk that can be modified by users. The default for this function 
#' is to generate an excel spreadsheet which allows users to select 
#' which 'ATTAINS.ParameterName' from the drop-down list excel spreadsheet 
#' aligns with each TADA.ComparableDataIdentifier.
#'
#' If there is no existing ATTAINS parameter name that corresponds with a
#' TADA.ComparableDataIdentifier needed for an organization's assessments, 
#' users should contact the ATTAINS team to inquire about adding the parameter. 
#' Users are free to use any ATTAINS parameter names found in the 
#' ATTAINS parameter domain value list, even if the parameter name 
#' has not previously been listed as a cause by the organization. 
#' A list of ATTAINS parameter names can be found by downloading 
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. 
#'
#' Otherwise, users can still proceed by overriding the data validation 
#' by value pasting. Users will be warned in the ATTAINS.FlagParameterName 
#' column if they choose to include an ATTAINS.ParameterName that was not 
#' named in prior ATTAINS assessment cycles as:
#' 'parameter name is not found as a prior parameter name for this organization'.
#'
#' Users who have already created a parameter crosswalk can provide it 
#' as an input to the function. Any previous crosswalk decisions the user 
#' had made regarding TADA.ComparableDataIdentifiers from
#' the original TADA df will be added to the new crosswalk. 
#' Users can then identify and fill any missing cell values that were either 
#' not addressed in the creation of the original crosswalk or which pertain 
#' to TADA.ComparableDataIdentifiers not included in the original crosswalk.
#'
#' The user-supplied crosswalk table must contain the required columns. 
#' Users will need to provide a paramRef data frame which contains at least these 
#' two column names: TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
#'
#' Users who are interested in doing an assessment or comparing criteria for 
#' more than one organization also need to include an additional column name: '
#' organization_identifier'. This ensures that the crosswalk between 
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName are correct for
#' each organization.
#'
#' A draft crosswalk between TADA.CharacteristicName and EPA 304A 
#' pollutant names (sourced from the Criteria Search Tool) has been created by 
#' the EPATADA team. This crosswalk is still in development and only focuses 
#' on the TADA priority characteristics. Please run the following below 
#' in the R environment to view current crosswalks:
#' 'utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))'.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning, 
#' processing, harmonization, filtering, handling of censored data, and 
#' addition of geospatial components (via TADA_GetATTAINS) functions
#' prior to running TADA_CreateParamRef.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the 
#' user. A list of organization identifiers can be found by downloading 
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. 
#' organization identifiers are listed in the "OrgName" tab. 
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument, 
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if 
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path. 
#' In the R console please type in:
#' file.path(Sys.getenv("USERPROFILE"), "Downloads") 
#' to get the file path in File Explorer if there is trouble locating the file. 
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight 
#' the cells in which users should input information. Users may need to insert 
#' additional rows if:
#' 1) ATTAINS.ParameterName correspond with multiple TADA.ComparableDataIdentifiers
#'    Example: An org uses "ALUMINUM" for all aluminum related parameter causes, 
#'    but this ATTAINS.parameter name may crosswalk to "ALUMINUM_TOTAL_NA_UG/L" 
#'    for one designated use and "ALUMINUM_DISSOLVED_NA_UG/L"
#' OR
#' 2) TADA.ComparableDataIdentifiers which are matched with multiple 
#'    ATTAINS.ParameterNames.
#'    Example: An org uses both "pH, HIGH" and "pH, LOW" as ATTAINS.ParameterNames, 
#'    both crosswalk to the TADA.ComparableDataIdentifier "PH_NA_NA_STD UNITS".
#'
#' @param overwrite A Boolean value that allows users to not overwrite the 
#' excel file that is created. This will help prevent a user from overwriting 
#' their progress when completing the excel spreadsheet.
#'
#' @param paramRef A data frame which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier and ATTAINS.ParameterName. Users will need to 
#' ensure this crosswalk contains the appropriate column names in order to 
#' run the function. Users will need to ensure that they supply a paramRef data 
#' frame which contains at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
#
#' Users who are interested in doing an assessment for more than one org_id 
#' will need to also include in the paramRef data frame which contains an 
#' additional column name: 'organization_identifier' in order to determine 
#' the proper crosswalk between TADA.ComparableDataIdentifier and
#' ATTAINS.ParameterName by organization identifier.
#'
#' @return A data frame which contains the columns: TADA.ComparableDataIdentifier,
#' organization_identifier, EPA304A.PollutantName, ATTAINS.ParameterName, 
#' and ATTAINS.FlagParameterName. Users will need to complete the crosswalk 
#' between ATTAINS.ParameterName and TADA.ComparableDataIdentifier.
#'
#' @export
#'
#' @examples
#' # This creates a blank paramRef template of UT Nutrients data. 
#' Users will need to fill this template out.
#' paramRef_UT <- TADA_CreateParamRef(
#'   Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE 
#' )
#' # User can choose to edit the paramRef_UT through the R environment or in 
#' # the excel spreadsheet. Users should be aware that any updates done only 
#' # in the R environment will not reflect the ATTAINS.FlagParameterName' values 
#' # correctly. They are recommended to rerun the function with this completed 
#' # crosswalk to do so if it is done in the R environment only and not in excel.
#' # See below for a simple example of this workflow:
#'
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   TADA.CharacteristicName == "AMMONIA" ~ "AMMONIA, TOTAL",
#'   TADA.CharacteristicName == "NITRATE" ~ "NITRATE",
#'   TADA.CharacteristicName == "NITROGEN" ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(Data_Nutrients_UT,
#'   paramRef = paramRef_UT2,
#'   org_id = "UTAHDWQ", excel = FALSE
#' )
#'
#' # The selectin of multiple org_id is allowed
#' Data_NCTCShepherdstown <- TADA_RunKeyFlagFunctions(Data_NCTCShepherdstown_HUC12)
#' Data_NCTCShepherdstown2 <- TADA_HarmonizeSynonyms(Data_NCTCShepherdstown)
#' paramRef_NCTC <- TADA_CreateParamRef(Data_NCTCShepherdstown2,
#'   org_id =
#'     c("MDE_EASP", "21VASWCB", "21PA"), excel = FALSE
#' )
#'
TADA_CreateParamRef <- function(.data, org_id = NULL, paramRef = NULL, excel = TRUE,
                                overwrite = FALSE) {
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
      if (length(setdiff(col.names, ref.names)) > 0 
          && !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
        stop(paste0(
          "TADA_CreateParamRef: 'paramRef' must be a data frame with these 2 columns:", 
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
          ))
      }
    }
  }
  
  .data <- as.data.frame(.data)
  
  # If users don't provide TADA.ComparableDataIdentifier in their paramRef input, 
  # crosswalk using TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText
  if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
    paramRef <- paramRef %>%
      dplyr::left_join(
        .data, c(
          "TADA.CharacteristicName", "TADA.MethodSpeciationName", 
          "TADA.ResultSampleFractionText")
        ) %>%
      dplyr::select(
        "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "organization_identifier", 
        "EPA304A.PollutantName", "ATTAINS.ParameterName", "ATTAINS.FlagParameterName"
        )
  }
  
  # if a user provides an org_id argument, it must be a character vector.
  if (!is.character(org_id) & !is.null(org_id)) {
    stop("TADA.CreateParamRef: org_id must be a character vector or left as NULL")
  }
  
  # if user doesn't provide an org_id argument, the function extracts the unique 
  # org_id from TADA_GetATTAINS(). Users will need to have ran TADA_GetATTAINS() 
  # for this option to be allowed. Selection of org_id will filter the ATTAINS 
  # domain list for parameter and use name by org_id.
  if (is.null(org_id)) {
    print(paste0(
      "TADA.CreateParamRef: No organization identifier(s) provided.",
      "Attempting to pull in organization identifiers found in the TADA data frame.",
      "Please ensure that you have ran TADA_GetATTAINS if you did not provide an org_id argument input."
      ))
    TADA_CheckColumns(.data, "ATTAINS.organizationname")
    org_id <- unique(stats::na.omit(.data[, "ATTAINS.organizationname"]))
  }
  
  org_id <- as.list(org_id)
  
  # If  more than 1 org, it will create n duplicate rows for each TADA.ComparableDataIdentifier.
  if (length(org_id) > 1) {
    print(paste0(
      "TADA.CreateParamRef: More than one org_name was defined in your dataframe.", 
      "Generating duplicate rows of TADA.ComparableDataIdentifier for each org."
      ))
  }
  
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
    "argument input excel = FALSE and overwrite = TRUE is an invalid combination. 
    Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
    ))
  }
  
  # 304a parameter name and standards are pulled in from the Criteria Search Tool (CST)
  CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA"))
  
  # Pulls in all unique combinations of TADA.ComparableDataIdentifier in user's dataframe.
  TADA_param <- dplyr::distinct(
    .data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]) %>%
    dplyr::left_join(CST_param, "TADA.CharacteristicName") %>%
    dplyr::select(
      TADA.CharacteristicName, TADA.ComparableDataIdentifier, 
      EPA304A.PollutantName = POLLUTANT_NAME) %>%
    dplyr::arrange(TADA.CharacteristicName, TADA.ComparableDataIdentifier) %>%
    tidyr::uncount(weights = length(org_id)) %>%
    dplyr::mutate(organization_identifier = rep(org_id, nrow(.) / length(org_id)))
  
  TADA_param$organization_identifier <- as.character(TADA_param$organization_identifier)
  
  # Pulls in all domain values of parameter names in ATTAINS. 
  ATTAINS_param_all <- utils::read.csv(
    system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::filter(organization_identifier %in% org_id) %>%
    dplyr::arrange(parameter)
  
  # Should we stop or warn users in this step?
  if (sum(!org_id %in% ATTAINS_param_all$organization_identifier) > 0) {
    warning(paste0(
      "TADA_CreateParamRef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS."
    ))
  }
  
  # Print message if there are many combinations of TADA Characteristic as it may slow run time.
  n <- nrow(dplyr::distinct(.data[, c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")]))
  if (n > 100) {
    message(paste0("There are ", n, " unique TADA.ComparableDataIdentifier names in your TADA data frame.
    This may result in slow runtime for TADA_CreateParamRef() if you are generating an excel spreadsheet."))
  }
  
  # If no paramRef is provided, the ATTAINS.ParameterName returns a blank column of NA that will need user input.
  if (is.null(paramRef)) {
    CreateParamRef <- TADA_param %>%
      dplyr::mutate(ATTAINS.ParameterName = NA) %>%
      dplyr::select(
        TADA.CharacteristicName, TADA.ComparableDataIdentifier, organization_identifier, 
        EPA304A.PollutantName, ATTAINS.ParameterName
        ) %>%
      dplyr::arrange(organization_identifier, TADA.CharacteristicName) %>%
      dplyr::distinct()
  }
  
  # If a user does provide a paramRef, this joins the dataframe to match the final output. 
  # rename() is used as we may need to consider if we want to consider allowing users
  # to crosswalk CST parameter names or modify EPA 304A pollutant names on their ends.
  if (!is.null(paramRef)) {
    paramRef$organization_identifier <- as.character(paramRef$organization_identifier)
    
    CreateParamRef <- TADA_param %>%
      dplyr::left_join(
        paramRef, 
        c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", 
          "organization_identifier"), 
        relationship = "many-to-many") %>%
      dplyr::rename(
        dplyr::any_of(
          c(EPA304A.PollutantName = "EPA304A.PollutantName.x", 
            organization_identifier = "organization_identifier.x", 
            ATTAINS.ParameterName = "ATTAINS.ParameterName.y")
          )
        ) %>%
      dplyr::select(
        TADA.CharacteristicName, TADA.ComparableDataIdentifier, 
        organization_identifier, EPA304A.PollutantName, ATTAINS.ParameterName
        ) %>%
      dplyr::filter(organization_identifier %in% org_id) %>%
      dplyr::arrange(organization_identifier, TADA.CharacteristicName) %>%
      dplyr::distinct()
  }
  
  # Re-runs the flagging data after a user has inputted values.
  # Will need to be done if a user only inputs values in the R environment and not in excel.
  if (is.null(paramRef)) {
    CreateParamRef <- CreateParamRef %>%
      dplyr::mutate(
        ATTAINS.ParameterName = NA, 
        ATTAINS.FlagParameterName = "No parameter crosswalk provided. Parameter will not be used for assessment"
        )
  }
  
  if (!is.null(paramRef)) {
    Flag1 <- paramRef %>%
      dplyr::anti_join(
        ATTAINS_param_all, 
        by = c("ATTAINS.ParameterName" = "parameter", "organization_identifier")
        ) %>%
      dplyr::select(ATTAINS.ParameterName, organization_identifier) %>%
      dplyr::distinct() %>%
      dplyr::mutate(ATTAINS.FlagParameterName1 = dplyr::case_when(
        ATTAINS.ParameterName == "No parameter match for TADA.ComparableDataIdentifier" | is.na(ATTAINS.ParameterName) ~ 
          "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment",
        !ATTAINS.ParameterName %in% ATTAINS_param_all$parameter ~ 
          "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List",
        !ATTAINS.ParameterName %in% ATTAINS_param_all[ATTAINS_param_all$organization_identifier == organization_identifier, "parameter"] ~ 
          "Parameter name is listed as a prior cause in ATTAINS, but not for this organization"
      ))
    
    CreateParamRef <- CreateParamRef %>%
      dplyr::left_join(Flag1, c("ATTAINS.ParameterName", "organization_identifier")) %>%
      dplyr::mutate(ATTAINS.FlagParameterName = dplyr::case_when(
        is.na(ATTAINS.ParameterName) ~ 
          "No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment",
        !is.na(ATTAINS.FlagParameterName1) ~ 
          ATTAINS.FlagParameterName1,
        is.na(ATTAINS.FlagParameterName1) ~ 
          "Parameter name is listed as a prior cause in ATTAINS for this organization"
      )) %>%
      dplyr::select(
        TADA.CharacteristicName, TADA.ComparableDataIdentifier, organization_identifier, 
        EPA304A.PollutantName, ATTAINS.ParameterName, ATTAINS.FlagParameterName
        ) %>%
      dplyr::distinct()
    
    # remove intermediate object Flag1
    rm(Flag1)
  }
  
  # Excel ref files to be stored in the Downloads folder location.
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if (excel == TRUE) {
    # Create column names for an empty dataframe
    columns <- c(
      "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "EPA304A.PollutantName", 
      "ATTAINS.ParameterName", "organization_identifier", "ATTAINS.FlagParameterName"
    )
    
    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) # empty dataframe with just column names
    colnames(par) <- columns
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "ATTAINSOrgNamesParamRef", visible = TRUE)
    openxlsx::addWorksheet(wb, "CreateParamRef", visible = TRUE)
    openxlsx::addWorksheet(wb, "Index", visible = FALSE)
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    sV <- wb$worksheets[[2]]$sheetViews
    wb$worksheets[[2]]$sheetViews <- set_zoom(90)
    # Format header and bodystyle
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    bodyStyle <- openxlsx::createStyle(wrapText = TRUE)
    
    # Index of allowable values for drop-down lists
    openxlsx::writeData(
      wb, "Index", 
      startCol = 4, 
      x = rbind(
        ATTAINS_param_all, 
        c(rep("NA", 3), "No use name match for TADA.ComparableDataIdentifier", "No parameter match for TADA.ComparableDataIdentifier")
        ))
    
    openxlsx::writeData(
      wb, "Index", 
      startCol = 3, 
      x = data.frame(
        organization_identifier = c(unique(ATTAINS_param$organization_identifier))
        ))
    
    openxlsx::writeData(
      wb, "Index", 
      startCol = 2, 
      x = unique(CST_param$CST.PollutantName)
      )
    
    openxlsx::writeData(
      wb, "Index", 
      startCol = 1, 
      x = data.frame(
        ATTAINS.ParameterName = c(unique(ATTAINS_param$parameter), "No parameter match for TADA.ComparableDataIdentifier")
        ))
    
    # Format column widths in CreateParamRef - for future considerations of formatting
    openxlsx::setColWidths(wb, "CreateParamRef", cols = 1:ncol(par), widths = "auto")
    
    openxlsx::writeData(wb, "CreateParamRef", startCol = 1, x = CreateParamRef, headerStyle = header_st)
    
    # Creates a tab that contains the ATTAINS parameter-use filtered by the org_id input.
    openxlsx::writeData(wb, "ATTAINSOrgNamesParamRef", startCol = 1, x = ATTAINS_param, headerStyle = header_st)
    
    # The list of allowable values for each column in excel tab [CreateParamRef] will be defined by the [Index] tab
    
    # Note: If we make edits to the data validation, please ensure the entire 
    # data frame column is being referenced. 
    # Ex. data validation will capture values in tab [Index] column h, for rows 2:50000 for input, value = sprintf("'Index'!$H$2:$H$50000")
    suppressWarnings(
      openxlsx::dataValidation(
        wb, sheet = "CreateParamRef", 
        cols = 3, rows = 2:1000, 
        type = "list", 
        value = sprintf("'Index'!$C$2:$C$5000"), 
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        ))
    
    suppressWarnings(
      openxlsx::dataValidation(
        wb, sheet = "CreateParamRef", 
        cols = 4, rows = 2:1000, 
        type = "list", 
        value = sprintf("'Index'!$B$2:$B$5000"), 
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        ))
    
    suppressWarnings(
      openxlsx::dataValidation(
        wb, sheet = "CreateParamRef", 
        cols = 5, rows = 2:1000, 
        type = "list", 
        value = sprintf("'Index'!$H$2:$H$50000"), 
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        ))
    
    # remove intermediate objects
    rm(CST_param, ATTAINS_param, ATTAINS_param_all)
    
    for (i in 1:nrow(CreateParamRef)) {
      openxlsx::writeFormula(
        wb, "CreateParamRef",
        startCol = 6, 
        startRow = i + 1, 
        array = TRUE,
        x = paste0(
          "=IF(OR(E", i + 1, '="",E', i + 1,
            '="No parameter match for TADA.ComparableDataIdentifier"),"No parameter crosswalk provided for TADA.ComparableDataIdentifier. Parameter will not be used for assessment",
          IF(ISNA(MATCH(E', i + 1, ',Index!H:H,0)),
            "Parameter name is not included in ATTAINS, contact ATTAINS to add parameter name to Domain List",
          IF(ISNA(MATCH(1,(E', i + 1, "=ATTAINSOrgNamesParamRef!E:E)*(C", i + 1, '=ATTAINSOrgNamesParamRef!A:A),0)),
            "Parameter name is listed as a prior cause in ATTAINS, but not for this organization",
            "Parameter name is listed as a prior cause in ATTAINS for this organization")))'
          )
        )
      
      openxlsx::conditionalFormatting(
        wb, "CreateParamRef",
        cols = 5, 
        rows = i + 1,
        type = "blanks", 
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
      )
      
      openxlsx::conditionalFormatting(
        wb, "CreateParamRef",
        cols = 5, 
        rows = i + 1,
        type = "notBlanks", 
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      )
    }
    
    # remove intermediate object TADA_param
    rm(TADA_param)
    
    if (overwrite == TRUE) {
      message(
        paste0("Overwriting sheet [CreateParamRef] in ", downloads_path)
        )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      message(
        "If you would like to replace sheet [CreateParamRef], use overwrite = TRUE argument in TADA_CreateParamRef"
        )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  
  return(CreateParamRef)
}

#' ATTAINS Parameter Name and Use Name crosswalk
#'
#' Users will be required to validate the use name crosswalk for each 
#' combination of ATTAIN parameter name and associated use_name that applies 
#' to their org(s) with its associated TADA.ComparableDataIdentifier(s). 
#' This can be accomplished by determining which 'use_name'(s) from the 
#' drop-down menu in the excel spreadsheet generated by this function correspond 
#' to the TADA.ComparableDataIdentifier(s) found in the TADA dataframe.
#'
#' Before running this function, users must run TADA_CreateParamRef() to 
#' create the crosswalk that defines the ATTAINS.ParameterName(s) and 
#' use_name(s) needing validation. All unique use_names from prior ATTAINS 
#' assessment cycle are pulled in using TADA_CreateParamUseRef(). If a user has 
#' defined multiple TADA.ComparableDataIdentifier matches to an 
#' ATTAINS.ParameterName, they will need to define whether every 
#' TADA.ComparableDataIdentifier matches to an associated use_name. 
#' If certain parameter and use combinations only apply tocertain 
#' TADA.ComparableDataIdentifier, users will need to select 'NA' or leave it 
#' as blank to properly capture this logic before summarizing Criteria and 
#' Methodology results.
#'
#' If an ATTAINS use name is not listed as a prior domain value for your org 
#' from prior ATTAINS assessment cycles, users should consider contacting the 
#' ATTAINS team to add this to the domain list. Otherwise, users can still 
#' proceed by overriding the data validation through value pasting.
#' Users will be warned in the ATTAINS.FlagUseName column if they choose to 
#' include an ATTAINS use name that was not named in prior ATTAINS assessment cycles as:
#' 'Use name is not listed as a prior cause in ATTAINS for this organization' or
#' 'Use name is listed as a prior cause in this organization, but not for this parameter name'
#'
#' Users will have the flexibility to include the EPA304a standards by including 
#' the string 'EPA304a' in the org_id function argument.
#'
#' Users who only want the EPA304a standards would input as an argument input:
#'
#' org_id = "EPA304a"
#'
#' Users who want both their orgs and EPA304a standards would input a vector which includes:
#'
#' org_id = c("EPA304a", "UTAHDWQ")
#'
#' NOTE: The EPA304a standards are not a part of the ATTAINS domain value, 
#' these standards have been crosswalk to a list of priority 
#' TADA.ComparableDataIdentifier by the internal EPATADA team.
#' The use_name for EPA304a standards are matched from the CriteriaSearchTool (CST):
#' www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa
#' while the use_name for any other ATTAINS organization identifiers come from 
#' ATTAINS domain value for use_name.
#'
#' @param .data A TADA dataframe. The user should run all desired data cleaning, 
#' processing, harmonization, filtering, handling of censored data, and 
#' addition of geospatial components (via TADA_GetATTAINS) functions
#' prior to running TADA_CreateParamRef.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the 
#' user. A list of organization identifiers can be found by downloading 
#' the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. 
#' organization identifiers are listed in the "OrgName" tab. 
#' The "code" column contains the organization identifiers that
#' should be used for this param. If a user does not provide an org_id argument, 
#' the function attempts to identify which organization identifier(s) to include
#' based on the unique ATTAINS organization identifiers found in the dataframe.
#'
#' @param excel A Boolean value that returns an excel spreadsheet if 
#' excel = TRUE. This spreadsheet is created in the user's downloads folder path. 
#' In the R console please type in:
#' file.path(Sys.getenv("USERPROFILE"), "Downloads") 
#' to get the file path in File Explorer if there is trouble locating the file. 
#' The file will be named "myfileRef.xlsx". The excel spreadsheet will highlight 
#' the cells in which users should input information. Users may need to insert 
#' additional rows if:
#' 1) ATTAINS.ParameterName correspond with multiple TADA.ComparableDataIdentifiers
#'    Example: An org uses "ALUMINUM" for all aluminum related parameter causes, 
#'    but this ATTAINS.parameter name may crosswalk to "ALUMINUM_TOTAL_NA_UG/L" 
#'    for one designated use and "ALUMINUM_DISSOLVED_NA_UG/L"
#' OR
#' 2) TADA.ComparableDataIdentifiers which are matched with multiple 
#'    ATTAINS.ParameterNames.
#'    Example: An org uses both "pH, HIGH" and "pH, LOW" as ATTAINS.ParameterNames, 
#'    both crosswalk to the TADA.ComparableDataIdentifier "PH_NA_NA_STD UNITS".
#'
#' @param overwrite A Boolean value that allows users to not overwrite the 
#' excel file that is created. This will help prevent a user from overwriting 
#' their progress when completing the excel spreadsheet.
#'
#' @param paramRef A data frame which contains a completed crosswalk between
#' TADA_ComparableDataIdentifier and ATTAINS.ParameterName. Users will need to 
#' ensure this crosswalk contains the appropriate column names in order to 
#' run the function. Users will need to ensure that they supply a paramRef data 
#' frame which contains at least these two column names:
#' TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
#
#' Users who are interested in doing an assessment for more than one org_id 
#' will need to also include in the paramRef data frame which contains an 
#' additional column name: 'organization_identifier' in order to determine 
#' the proper crosswalk between TADA.ComparableDataIdentifier and
#' ATTAINS.ParameterName by organization identifier.
#'
#' @param paramUseRef A data frame which contains a completed crosswalk of 
#' org specific domain value of use_name by ATTAINS.ParameterName. 
#' Users will need to ensure this crosswalk contains the appropriate column 
#' names in order to  run the function. Users who have previously completed
#' this crosswalk table can re-use it and will review this output for accuracy.
#'
#' @return A data frame which contains the columns: TADA.ComparableDataIdentifier, 
#' organization_identifier, EPA304A.PollutantName, ATTAINS.ParameterName, 
#' and ATTAINS.FlagUseName. Users will need to review the crosswalk between 
#' ATTAINS.ParameterName, use_name and TADA.ComparableDataIdentifier.
#'
#' @export
#'
#' @examples
#' # Users are required to provide a paramRef argument first. 
#' # See TADA_CreateParamRef() for additional comments.
#' paramRef_UT <- TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#'
#' paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
#'   TADA.CharacteristicName == "AMMONIA" ~ "AMMONIA, TOTAL",
#'   TADA.CharacteristicName == "NITRATE" ~ "NITRATE",
#'   TADA.CharacteristicName == "NITROGEN" ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
#' ))
#' paramRef_UT3 <- TADA_CreateParamRef(
#'   Data_Nutrients_UT, paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE)
#'
#' # After running TADA_CreateParamRef() users will provide this as a function input. 
#' paramUseRef_UT <- TADA_CreateParamUseRef(
#'   Data_Nutrients_UT, paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE)
#'
#' # Users can include the EPA304a standards by itself or compared to their org(s)
#' paramUseRef_UT2 <- TADA_CreateParamUseRef(
#'   Data_Nutrients_UT, 
#'   paramRef = paramRef_UT3, 
#'   org_id = c("EPA304a", "UTAHDWQ"), excel = FALSE)
#'   
#' paramUseRef_UT3 <- TADA_CreateParamUseRef(
#'   Data_Nutrients_UT, 
#'   paramRef = paramRef_UT3, 
#'   org_id = c("EPA304a"), excel = FALSE)
#'
TADA_CreateParamUseRef <- function(.data, org_id = NULL, paramRef = NULL, 
                                   paramUseRef = NULL, excel = FALSE, overwrite = FALSE) {
  # overwrite argument should only be used when creating an excel file.
  if (excel == FALSE && overwrite == TRUE) {
    stop(paste0(
      "argument input excel = FALSE and overwrite = TRUE is an invalid combination.",
      "Cannot overwrite the excel generated spreadsheet if a user specifies excel = FALSE"
      ))
  }
  
  # Checks if paramRef argument contains a dataframe and necessary columns to proceed.
  if (is.null(paramRef)) {
    paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")
  }
  
  # If org_id argument is not provided, this will attempt to pull in org_id from TADA_GetATTAINS.
  if (is.null(org_id)) {
    print(paste0(
      "TADA.CreateParamRef: No organization identifier(s) provided.", 
      "Attempting to pull in organization identifiers found in the TADA data frame.",
      "Please ensure that you have ran TADA_GetATTAINS if you did not provide an org_id argument input."
      ))
    TADA_CheckColumns(.data, "ATTAINS.organizationname")
    org_id <- unique(stats::na.omit(.data[, "ATTAINS.organizationname"]))
  }
  
  # If a user does not fill in ANY values for the crosswalk of ATTAINS.ParameterName.
  # Users may want to proceed with only the EPA304a standards crosswalk, 
  # therefore we will allow users to proceed in this case.
  if (sum(!is.na(paramRef$ATTAINS.ParameterName)) == 0) {
    warning(paste0(
      "No values were found in ATTAINS.ParameterName.", 
      "Please ensure that you have inputted all field values of interest in the",
      "ATTAINS.ParameterName column generated from TADA_CreateParamRef() function"
      ))
  }
  
  # If a user leaves at least one values for the crosswalk of ATTAINS.ParameterName blank.
  # Users are recommended to select 'No parameter match for this TADA.ComparableDataIdentifier' if
  # there is no crosswalk, but leaving it blank will be treated similarly.
  if (sum(is.na(paramRef$ATTAINS.ParameterName)) > 1) {
    print(paste0(
      "NAs were found in ATTAINS.ParameterName.",
      "Please ensure that you have inputted all field values of interest in",
      "the ATTAINS.ParameterName column generated from TADA_CreateParamRef() function"
      ))
  }
  
  # NOTE: For TADA Development: Do we need this? Should users be allowed to modify the EPA304a crosswalk we did?
  # For the time being I would allow this as an option, but will comment this section out for further discussions.
  # if (sum(is.na(paramRef$EPA304A.PollutantName)) > 1 && org_id == "EPA304a") {
  #   print("NAs were found in EPA304A.PollutantName. Please ensure that you have inputted all field values of interest in the EPA304A.PollutantName column generated from TADA_CreateParamRef() function if you are interested in using the 304a recommended standards")
  # }
  
  # check to see if user-supplied parameter ref is a df with appropriate columns and is filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop(paste0(
        "TADA_CreateParamUseRef: 'paramuseRef' must be a data frame with these 2 columns:",
        "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
        ))
    }
    
    if (is.data.frame(paramRef)) {
      col.names <- c(
        "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName"
      )
      
      ref.names <- names(paramRef)
      
      if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
        stop(paste0(
          "TADA_CreateParamUseRef: 'paramuseRef' must be a data frame with these 2 columns:",
          "TADA.ComparableDataIdentifier and ATTAINS.ParameterName"
        ))
      }
    }
  }
  
  # check to see if user-supplied parameter-use ref is a df with appropriate columns and is filled out.
  if (!is.null(paramRef) & !is.character(paramRef)) {
    if (!is.data.frame(paramRef)) {
      stop(paste0(
        "TADA_CreateParamUseRef: 'paramUseRef' must be a data frame with these 6 columns:", 
        "TADA.ComparableDataIdentifier, organization_identifier, EPA304A.PollutantName,", 
        "ATTAINS.ParameterName, use_name, IncludeOrExclude"
        ))
    }
    
    if (is.data.frame(paramUseRef)) {
      col.names <- c(
        "TADA.ComparableDataIdentifier", "organization_identifier", 
        "EPA304A.PollutantName", "ATTAINS.ParameterName", "use_name", "IncludeOrExclude"
      )
      
      ref.names <- names(paramUseRef)
      
      if (length(setdiff(col.names, ref.names)) > 0 && !("TADA.ComparableDataIdentifier" %in% names(paramUseRef))) {
        stop(paste0(
          "TADA_CreateParamUseRef: 'paramUseRef' must be a data frame with these 6 columns:", 
          "TADA.ComparableDataIdentifier, organization_identifier, EPA304A.PollutantName,", 
          "ATTAINS.ParameterName, use_name, IncludeOrExclude"
        ))
      }
    }
  }
  
  .data <- as.data.frame(.data)
  
  # Pulls in all domain values of parameter and use names by orgs in ATTAINS. 
  ATTAINS_param_all <- utils::read.csv(system.file("extdata", "ATTAINSParamUseEntityRef.csv", package = "EPATADA"))
  
  if (!is.null(paramRef) & !("TADA.ComparableDataIdentifier" %in% names(paramRef))) {
    paramRef <- paramRef %>%
      dplyr::left_join(
        .data, 
        by = c("TADA.CharacteristicName", "TADA.MethodSpeciationName", 
               "TADA.ResultSampleFractionText")
        ) %>%
      dplyr::select(
        "TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "organization_identifier", 
        "EPA304A.PollutantName", "ATTAINS.ParameterName", "ATTAINS.FlagParameterName"
        )
  }
  
  # if user doesn't provide an org_id argument, the function extracts the unique org_id from TADA_GetATTAINS().
  # Users will need to have ran TADA_GetATTAINS() for this option to be allowed. 
  # Selection of org_id will filter the drop down lists in future steps of creating the reference tables.
  if (is.null(org_id)) {
    stop("TADA.CreateParamUseRef: No organization identifier(s) provided.")
  }
  
  org_id <- as.list(org_id)
  
  # Checks if org_id are valid names found in ATTAINS - with the exception of "EPA304a" as that is not an ATTAINS org_id.
  if (sum(!org_id[org_id != "EPA304a"] %in% ATTAINS_param_all$organization_identifier) > 0) {
    warning(paste0(
      "TADA_CreateParamRef: ",
      "One or more organization identifiers entered by user is not found in ATTAINS. "
    ))
  }
  
  # Checks if org_id are found in the user supplied paramRef argument.
  if (sum(!org_id[org_id != "EPA304a"] %in% paramRef$organization_identifier) > 0) {
    warning(paste0(
      "TADA_CreateParamRef: ",
      "One or more organization identifiers entered by user is not found in your paramRef argument input.",
      "Excluding those missing organization identifier(s) from output"
    ))
  }
  
  # Filters the ATTAINS parameter and use names by the org_id in user supplied df.
  ATTAINS_param <- ATTAINS_param_all %>%
    dplyr::select(organization_identifier, parameter, use_name) %>%
    dplyr::filter(parameter %in% paramRef$ATTAINS.ParameterName) %>%
    dplyr::filter(organization_identifier %in% org_id)
  
  # Create the parameter-use reference table for validation
  CreateParamUseRef <- paramRef %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      ATTAINS_param, 
      by = c("ATTAINS.ParameterName" = "parameter", "organization_identifier"), 
      relationship = "many-to-many"
      ) %>%
    dplyr::select(
      TADA.ComparableDataIdentifier, organization_identifier, 
      EPA304A.PollutantName, ATTAINS.ParameterName, use_name
      ) %>%
    tidyr::drop_na(ATTAINS.ParameterName) %>%
    dplyr::filter(ATTAINS.ParameterName != "Parameter not used for analysis") %>%
    dplyr::distinct()
  
  # If users want the EPA304a standards. This pulls in the CST reference file. 
  # Extracts the associated EPA304a pollutant names and its use_names.
  if ("EPA304a" %in% org_id) {
    CST_param <- utils::read.csv(system.file("extdata", "CST.csv", package = "EPATADA")) %>%
      dplyr::select(EPA304A.PollutantName = POLLUTANT_NAME, use_name) %>%
      dplyr::mutate(organization_identifier = "EPA304a")
    
    EPA_param <- CreateParamUseRef %>%
      dplyr::left_join(CST_param, c("EPA304A.PollutantName"), relationship = "many-to-many") %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, organization_identifier = organization_identifier.y, 
        ATTAINS.ParameterName, EPA304A.PollutantName, use_name = use_name.y
        ) %>%
      dplyr::distinct()
    
    # remove intermediate object CST_param
    rm(CST_param)
    
    CreateParamUseRef <- CreateParamUseRef %>%
      dplyr::ungroup() %>%
      dplyr::full_join(
        EPA_param, 
        by = c(
          "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", 
          "organization_identifier", "EPA304A.PollutantName", "use_name"
          )) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, organization_identifier, 
        EPA304A.PollutantName, ATTAINS.ParameterName, use_name
        ) %>%
      dplyr::filter(organization_identifier %in% org_id)
    
    # remove intermediate object EPA_param
    rm(EPA_param)
  }
  
  # If a user provide a paramUseRef argument, this will
  if (!is.null(paramUseRef)) {
    CreateParamUseRef <- paramUseRef %>%
      dplyr::left_join(
        CreateParamUseRef, 
        by = c(
          "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", 
          "organization_identifier", "EPA304A.PollutantName", "use_name"
          )) %>%
      dplyr::select(
        TADA.ComparableDataIdentifier, organization_identifier, 
        EPA304A.PollutantName, ATTAINS.ParameterName, use_name
        ) %>%
      dplyr::filter(organization_identifier %in% org_id)
  }
  
  # This updates the flagging column. Users who only creates an R dataframe in the R environment will need to ensure they re-run the function with their completed paramRef as an input to reflect this column accurately.
  Flag1 <- CreateParamUseRef %>%
    dplyr::anti_join(
      ATTAINS_param_all, 
      by = c("use_name", "organization_identifier")
      ) %>%
    dplyr::select(use_name, organization_identifier) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      ATTAINS.FlagUseName1 = "Use name is listed as a prior cause in this organization, but not for this parameter name"
      )
  
  Flag2 <- CreateParamUseRef %>%
    dplyr::anti_join(
      ATTAINS_param_all, 
      by = c("ATTAINS.ParameterName" = "parameter", "use_name", "organization_identifier")
      ) %>%
    dplyr::select(use_name, ATTAINS.ParameterName, organization_identifier) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      ATTAINS.FlagUseName2 = "Use name is not listed as a prior cause in ATTAINS for this organization"
      )
  
  CreateParamUseRef <- CreateParamUseRef %>%
    dplyr::left_join(Flag2, c("ATTAINS.ParameterName", "use_name", "organization_identifier")) %>%
    dplyr::left_join(Flag1, c("use_name", "organization_identifier")) %>%
    dplyr::mutate(IncludeOrExclude = "Include") %>%
    dplyr::mutate(ATTAINS.FlagUseName = dplyr::case_when(
      IncludeOrExclude == "Exclude" ~ 
        "Use name does not apply for this ATTAINS.ParameterName. Excluding this use name from analysis.",
      is.na(use_name) ~ 
        "No use name is provided. Consider choosing an appropriate use_name.",
      organization_identifier == "EPA304a" ~ 
        "Will use the EPA304a recommended standards for this parameter. Do not edit EPA304a use_name",
      organization_identifier != "EPA304a" ~ 
        "Use name is listed as prior cause in ATTAINS for this org",
      !is.na(ATTAINS.FlagUseName2) ~ ATTAINS.FlagUseName2,
      !is.na(ATTAINS.FlagUseName1) ~ ATTAINS.FlagUseName1,
    )) %>%
    dplyr::select(
      TADA.ComparableDataIdentifier, organization_identifier, EPA304A.PollutantName, 
      ATTAINS.ParameterName, use_name, IncludeOrExclude, ATTAINS.FlagUseName
      )
  
  # remove intermediate objects
  rm(ATTAINS_param, Flag1, Flag2)
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
  
  if (excel == TRUE) {
    # Create column names for an empty dataframe
    columns <- c(
      "organization_identifier", "ATTAINS.ParameterName", "use_name", 
      "ATTAINS.FlagParameterName", "ATTAINS.FlagUseName"
    )
    
    # empty dataframe with just column names
    par <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
    colnames(par) <- columns
    
    wb <- openxlsx::loadWorkbook(wb, downloads_path)
    
    # If a user chooses to rerun the TADA_CreateParamUseRef() function, 
    # the sheet will already exist and error.
    tryCatch(
      {
        openxlsx::addWorksheet(wb, "CreateParamUseRef")
      },
      error = function(e) {
        openxlsx::removeWorksheet(wb, "CreateParamUseRef")
        openxlsx::addWorksheet(wb, "CreateParamUseRef")
      }
    )
    
    # If a user performs TADA_CreateParamRef() with excel = FALSE, but decides to perform TADA_CreateParamUseRef() with excel = TRUE,
    # and if the file already exists, we need to consider how to display the excel tab for [CreateParamRef] to ensure it does not display
    # non-matching dataframe crosswalk to avoid confusion.
    # excel_paramRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")[,c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "organization_identifier")]
    #
    # if(nrow(dplyr::inner_join(
    #   paramRef[,c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "organization_identifier")],
    #   excel_paramRef[,c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName", "organization_identifier")]))
    #   !=nrow(excel_paramRef)){
    #     if(nrow(excel_paramRef)!=nrow(paramRef)){
    #       warning("Your user-supplied paramRef table does not match the parameter reference table in excel sheet [CreateParamRef]. This may have occured if you have previously ran TADA_CreateParamRef() with excel = TRUE on a previous dataframe and proceeded with running TADA_CreateParamRef() with excel = FALSE and TADACreateParamUseRef() with excel = TRUE")
    #     }
    # }
    
    # set zoom size
    set_zoom <- function(x) gsub('(?<=zoomScale=")[0-9]+', x, sV, perl = TRUE)
    sV <- wb$worksheets[[2]]$sheetViews
    wb$worksheets[[2]]$sheetViews <- set_zoom(90)
    sV <- wb$worksheets[[3]]$sheetViews
    wb$worksheets[[3]]$sheetViews <- set_zoom(90)
    sV <- wb$worksheets[[4]]$sheetViews
    wb$worksheets[[4]]$sheetViews <- set_zoom(90)
    # Format column header
    header_st <- openxlsx::createStyle(textDecoration = "Bold")
    # Format Column widths
    openxlsx::setColWidths(wb, "CreateParamUseRef", cols = 1:ncol(CreateParamUseRef), widths = "auto")
    
    # Export CreateParamUseRef dataframe into the excel spreadsheet tab
    openxlsx::writeData(wb, "CreateParamUseRef", startCol = 1, x = CreateParamUseRef, headerStyle = header_st)
    
    # Index of allowable values for drop-down lists
    openxlsx::writeData(wb, "Index", startCol = 2, x = data.frame("IncludeOrExclude" = c("Include", "Exclude")))
    
    # data validation drop down list created below.
    # Note: ATTAINSOrgNamesParamRef contains the list of prior param and use cause by org names specific.
    # Since Use Names are individual to each Organization.
    suppressWarnings(
      openxlsx::dataValidation(
        wb, sheet = "CreateParamUseRef", 
        cols = 5, rows = 2:1000, 
        type = "list", 
        value = sprintf("'ATTAINSOrgNamesParamRef'!$D$2:$D$50000"), 
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        ))
    
    suppressWarnings(
      openxlsx::dataValidation(
        wb, sheet = "CreateParamUseRef", 
        cols = 6, rows = 2:1000, 
        type = "list", 
        value = sprintf("'Index'!$B$2:$B$5"), 
        allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE
        ))
    
    for (i in 1:nrow(CreateParamUseRef)) {
      openxlsx::conditionalFormatting(
        wb, "CreateParamUseRef",
        cols = 5, rows = i + 1,
        type = "blanks", 
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13] )
      )
      
      openxlsx::conditionalFormatting(
        wb, "CreateParamUseRef",
        cols = 6, rows = i + 1,
        type = "contains", 
        rule = c("Exclude"), 
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[13])
      )
      
      openxlsx::conditionalFormatting(
        wb, "CreateParamUseRef",
        cols = 6, rows = i + 1,
        type = "contains", 
        rule = c("Include"), 
        style = openxlsx::createStyle(bgFill = TADA_ColorPalette()[8])
      )
      
      openxlsx::writeFormula(
        wb, "CreateParamUseRef",
        startCol = 7, startRow = i + 1, 
        array = TRUE,
        x = paste0(
        "=IF(B", i + 1, '="EPA304a",
          "Will use the EPA304a recommended standards for this parameter. Do not edit EPA304a use_name",
        IF(F', i + 1, '="Exclude",
          "Use name does not apply for this ATTAINS.ParameterName. Exclusing this use name from analysis.",
        IF(ISBLANK(E', i + 1, '),
          "No use name is provided. Consider choosing an appropriate use_name.",
        IF(ISNA(MATCH(1,(E', i + 1, "=Index!G:G)*(B", i + 1, '=Index!D:D),0)),
          "Use name is not listed as a prior cause in ATTAINS for this organization",
        IF(ISNA(MATCH(1,(D', i + 1, "=Index!H:H)*(E", i + 1, "=Index!G:G)*(B", i + 1, '=Index!D:D),0)),
          "Use name is listed as a prior cause in this organization, but not for this parameter name",
          "Use name is listed as prior cause in ATTAINS for this org")))))'
        )
      )
    }
    
    # Handles overwriting the excel file. 
    if (overwrite == TRUE) {
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = T)
    }
    
    if (overwrite == FALSE) {
      warning(
        "If you would like to replace [CreateParamUseRef], use overwrite = TRUE argument in TADA_CreateParamUseRef"
        )
      openxlsx::saveWorkbook(wb, downloads_path, overwrite = F)
    }
    
    cat("File saved to:", gsub("/", "\\\\", downloads_path), "\n")
  }
  return(CreateParamUseRef)
}
