#' Get WQP/WQX MonitoringLocationIdentifier and ATTAINS.assessmentunitidentifier Crosswalk 
#' from ATTAINS
#'
#' Tribes and States who participate in electronic reporting of water quality conditions
#' through EPA ATTAINS may also submit a crosswalk of WQP MonitoringLocationIdentifiers associated 
#' with their Assessment Units to ATTAINS. If the organization has recorded 
#' MonitoringLocationIdentifiers associated with their Assessment Units in ATTAINS, this function 
#' can be used to create a crosswalk of known MonitoringLocationIdentifiers and Assessment Units. 
#' All tribal nations record this crosswalk in ATTAINS but only a few states. If a state has not 
#' supplied Monitoring Location information to ATTAINS, the function does not return a data frame.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the user. A list of
#' organization identifiers can be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. Organization identifiers
#' are listed in the "OrgName" tab. The "code" column contains the organization identifiers that
#' should be used for this param.
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
#' PUEBLOOFTESUQUE_crosswalk <- TADA_GetATTAINSAUSiteCrosswalk(org_id = "PUEBLOOFTESUQUE")
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
      # paste org_id in front of MLs from the specified org if they are missing from ATTAINS
      dplyr::mutate(MonitoringLocationIdentifier = ifelse((OrganizationIdentifier == org_id & 
                                                             stringr::str_detect(MonitoringLocationIdentifier, org_id, negate = TRUE)),
                                                          paste0(org_id, "-", MonitoringLocationIdentifier),
                                                          MonitoringLocationIdentifier))

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

WQXProviderRef_Cached <- NULL

#' Get Organization and Provider Reference Table (IN ACTIVE DEVELOPMENT)
#'
#' This function creates a crosswalk of all OrganizationIdentifiers, 
#' OrganizationFormalNames, and ProviderNames in the Water Quality Portal (WQP).
#'
#' @return A crosswalk dataframe including the following columns: 
#' OrganizationIdentifier, OrganizationFormalName, ProviderName.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' provider.ref <- TADA_GetProviderRef()
#' }
#'
TADA_GetProviderRef <- function() {
  # If there is a cached table available return it
  if (!is.null(WQXProviderRef_Cached)) {
    return(WQXProviderRef_Cached)
  }

  # Try to download up-to-date raw data
  raw.data <- tryCatch(
    {
      # read raw csv from url
      utils::read.csv(url("https://www.waterqualitydata.us/data/Organization/search?mimeType=csv&zip=no")) %>%
        dplyr::select(OrganizationIdentifier, OrganizationFormalName, ProviderName) %>%
        dplyr::distinct()
    },
    error = function(err) {
      NULL
    }
  )
  
  # need to remove providers w/ no sites on date site pages

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest WQP Organization and Provider Reference Table failed!")
    message("Falling back to (possibly outdated) internal file.")
    return(utils::read.csv(system.file("extdata", "WQXProviderRef.csv", package = "EPATADA")))
  }

  # Save updated table in cache
  WQXProviderRef <- raw.data

  WQXProviderRef_Cached <- WQXProviderRef

  WQXProviderRef
}

# Update Characteristic Reference Table internal file (for internal use only)

TADA_UpdateProviderRef <- function() {
  utils::write.csv(TADA_GetProviderRef(), file = "inst/extdata/WQXProviderRef.csv", row.names = FALSE)
}


#' Add or Update Monitoring Location Identifiers in ATTAINS
#'
#' This function creates the batch upload files needed to add or update  
#' Monitoring Location Identifiers in ATTAINS Assessment Unit profiles. Users
#' can specify whether all records should be overwritten (replaced) or if new 
#' Monitoring Location Identifiers should be appended (added) to existing records.
#'
#' @param org_id Character argument. The ATTAINS organization identifier must  
#' be supplied by the user. A list of organization identifiers can be found by
#' downloading the ATTAINS Domains Excel file: 
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. 
#' Organization identifiers are listed in the "OrgName" tab. The "code" column
#' contains the organization identifiers that should be used for this param.
#'
#' @param wqp_data_links Character argument. When wqp_data_links is equal to "add" 
#' or "replace", the function will build the URL for the Water Quality Portal 
#' Data Site page for each Monitoring Location Identifier in the data frame. It will 
#' examine the response code of each URL and only retain those with a 
#' 200 response, which indicates the URL is valid. When wqp_data_links = "add", 
#' the URL will be added to any existing text in the MS_DATA_LINK_TEXT column. 
#' When wqp_data_links = "replace", the URL will replace any existing text in the
#' MS_DATA_LINK_TEXT column. When wqp_data_links = "none", no URLs will be created
#' or added to the returned data frame. Default is wqp_data_links = "add".
#'
#' @param attains_replace Character argument. When attains_replace = FALSE, all 
#' Monitoring Location Identifiers associated with an Assessment Unit in ATTAINS
#' will be retained even if they are not included in the user supplied crosswalk.
#' When attains_replace = TRUE, Monitoring Location Identifiers will only be 
#' retained if they are in the user supplied crosswalk. Default equals FALSE.
#'
#' @param crosswalk A user-supplied dataframe with the columns ASSESSMENT_UNIT_ID
#' and MS_LOCATION_ID. When crosswalk = NULL, the crosswalk will be downloaded 
#' from ATTAINS. This allows users to add URLs for the Water Quality Portal Data 
#' Site pages to the ATTAINS Assessment Unit profile where possible without 
#' updating other information in ATTAINS.
#'
#' @return The csv batch upload files for ATTAINS to add or update Monitoring Locations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Alaska example
#' AK_batchAUupload <- TADA_UpdateMonitoringLocationsInATTAINS(org_id = "AKDECWQ", 
#'                                                crosswalk = NULL, 
#'                                                attains_replace = FALSE, 
#'                                                wqp_data_links = "replace")
#'
TADA_UpdateMonitoringLocationsInATTAINS <- function(org_id = NULL,
                                                    crosswalk = NULL,
                                                    attains_replace = FALSE,
                                                    wqp_data_links = "add") {
  # get list of organization identifiers from ATTAINS
  org.ref <- utils::read.csv(system.file("extdata", "ATTAINSOrgIDsRef.csv", package = "EPATADA"))

  # stop function if organization identifiers is not found in ATTAINS
  if (!org_id %in% org.ref$code) {
    stop(paste0(
      "TADA_UpdateMonitoringLocationsInATTAINS: ",
      "The organization identifier entered by user is not found in ATTAINS."
    ))
  }
  
  if(is.null(crosswalk) & attains_replace == TRUE) {
    stop(paste0("TADA_UpdateMonitoringLocationsInATTAINS: ",
                "in order to replace MonitoringLocations stored in ATTAINS ",
                "(with attains_replace = TRUE), user must provide a ",
                "MonitoringLocation/AssessmentUnitcrosswalk."))
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
        "MS_ORG_ID"
      )

      TADA_CheckColumns(crosswalk, expected_cols)
    }

      if (attains_replace == FALSE) {
        # create assessment unit crosswalk from ATTAINS
        attains.crosswalk <- TADA_GetATTAINSAUSiteCrosswalk(org_id = org_id) %>%
          dplyr::rename(ASSESSMENT_UNIT_ID = ATTAINS.assessmentunitidentifier,
                        MS_ORG_ID = OrganizationIdentifier, 
                        MS_LOCATION_ID = MonitoringLocationIdentifier,
                        MONITORING_DATA_LINK_TEXT = MonitoringDataLinkText) 

        if(is.null(crosswalk)) {
          update.crosswalk <- attains.crosswalk
            
          
          rm(attains.crosswalk)
        }

        if(!is.null(crosswalk)) {
        # filter crosswalk from ATTAINS to retain only Assessment Units included in user-supplied
        # crosswalk
          
        attains.crosswalk <- attains.crosswalk %>%
          dplyr::filter(ASSESSMENT_UNIT_ID %in% crosswalk$ASSESSMENT_UNIT_ID)

        # combine user supplied and attains crosswalks to create one crosswalk
        # no rows are omitted
        update.crosswalk <- attains.crosswalk %>%
          dplyr::full_join(crosswalk, by = dplyr::join_by(
            ASSESSMENT_UNIT_ID,
            MS_ORG_ID, MS_LOCATION_ID
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
        # get org/provider name ref from TADA ref files
        provider.ref <- utils::read.csv(system.file("extdata", "WQXProviderRef.csv", package = "EPATADA")) %>%
          dplyr::select(OrganizationIdentifier, ProviderName) %>%
          dplyr::distinct() %>%
          dplyr::rename(MS_ORG_ID = OrganizationIdentifier) %>%
          dplyr::mutate(OrgIDForURL = MS_ORG_ID)

        # add additional rows to account for the addition of "_WQX" to many org names for WQP data
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
            )
          )
        
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
                paste0(MONITORING_DATA_LINK_TEXT, "; ", MONITORING_DATA_LINK_TEXT.New),
                MONITORING_DATA_LINK_TEXT
              ),
              MONITORING_DATA_LINK_TEXT = stringr::str_remove_all(
                MONITORING_DATA_LINK_TEXT,
                "NA, "
              )
            ) %>%
            tidyr::separate_rows(MONITORING_DATA_LINK_TEXT, sep = ", ") %>%
            dplyr::group_by(ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID) %>%
            suppressMessages(dplyr::summarise(MONITORING_DATA_LINK_TEXT = paste(unique(
              MONITORING_DATA_LINK_TEXT
            ), collapse = ", "))) %>%
            dplyr::select(ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID, MONITORING_DATA_LINK_TEXT) %>%
            dplyr::distinct()
        }
    return(update.crosswalk)
      }
}



## Commented out for now as per conversation w/ WR on 12/12/24 (these additional files are
## likely necessary to update the ML/AUIDs in ATTAINS, will need to test in Demo ATTAINS)
# # Create Assessment_Units df
# # need to figure out assessment comments, they are not currently included (discuss w. FG)
# assessment_units <- au.info %>%
#   dplyr::select(assessment_unit_identifier, assessment_unit_name, state_code,
#                 agency_code, location_description_text, use_class_name) %>%
#   dplyr::rename(ASSESSMENT_UNIT_ID = assessment_unit_identifier,
#                 ASSESSMENT_UNIT_NAME = assessment_unit_name,
#                 ASSESSMENT_UNIT_STATE = state_code,
#                 ASSESSMENT_UNIT_AGENCY = agency_code,
#                 LOCATION_DESCRIPTION = location_description_text,
#                 USE_CLASS_NAME = use_class_name
#                 ) %>%
#   dplyr::mutate(ASSESSMENT_UNIT_COMMENT = "") %>%
#   dplyr::select(ASSESSMENT_UNIT_IDENTIFIER, ASSESSMENT_UNIT_NAME, ASSESSMENT_UNIT_STATE,
#                 ASSESSMENT_UNIT_AGENCY, ASSESSMENT_UNIT_COMMENT, LOCATION_DESCRIPTION,
#                 USE_CLASS_NAME)
#
# # Create Water_Types df
# water_types <- au.info %>%
#   dplyr::select(assessment_unit_identifier, water_type_code, water_size_number,
#                 units_code, size_source_text, size_estimation_method_code,
#                 size_source_scale_text) %>%
#   dplyr::rename(ASSESSMENT_UNIT_ID = assessment_unit_identifier,
#                 WATER_TYPE = water_type_code,
#                 WATER_SIZE = water_size_number,
#                 WATER_UNIT = units_code,
#                 SIZE_SOURCE = size_source_text,
#                 ESTIMATION_METHOD = size_estimation_method_code,
#                 SOURCE_SCALE = size_source_scale_text)
#
# # create Locations df
# locations <- au.info %>%
#   tidyr::unnest(locations)
#   dplyr::select(assessment_unit_identifier,
#                 location_type_code,
#                 organization_identifier,
#                 location_text) %>%
#   dplyr::rename(ASSESSMENT_UNIT_ID = assessment_unit_identifier,
#                 LOCATION_TYPE_CODE = location_type_code,
#                 LOCATION_TYPE_CONTEXT = organization_identifier,
#                 LOCATION_TEXT = location_text)
# Monitoring_Stations <- ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID, MS_DATA_LINK
