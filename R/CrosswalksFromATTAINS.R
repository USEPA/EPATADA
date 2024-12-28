#' Get MonitoringLocationIdentifier and ATTAINS.assessmentunitidentifier Crosswalk
#'
#' If the organization has recorded MonitoringLocationIdentifiers associated with their
#' Assessment Units in ATTAINS, this function can be used to create a crosswalk of known
#' MonitoringLocationIdentifier and Assessment Units.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the user. A list of
#' organization identifiers can be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. Organization identifiers
#' are listed in the "OrgName" tab. The "code" column contains the organization identifiers that
#' should be used for this param.
#'
#'
#' @return A dataframe with three columns, ATTAINS.assessmentunitidentifier,
#' MonitoringLocationIdentiifer, and OrganizationIdentifier is returned.
#'
#' @export
#'
#' @examples
#' # Alaska example
#' AK_crosswalk <- TADA_GetAssessmentUnitCrosswalk(org_id = "AKDECWQ")
#'
#' \dontrun{
#'
#' }

#'
TADA_GetAssessmentUnitCrosswalk <- function(org_id = NULL) {

  org.ref <- TADA_GetATTAINSOrgIDsRef()

if (!org_id %in% org.ref$code) {
  print(paste0("TADA_GetAssessmentUnitCrosswalk: ",
        "The organization identifier entered by user is not found in ATTAINS."))
    }

    if (org_id %in% org.ref$code) {
      
      rm(org.ref)
      
      au.info <- rATTAINS::assessment_units(organization_id = org_id)

      au.crosswalk <- au.info %>%
        tidyr::unnest(monitoring_stations) %>%
        dplyr::select(
          monitoring_location_identifier, monitoring_organization_identifier,
          assessment_unit_identifier
        ) %>%
        dplyr::filter(!is.na(monitoring_location_identifier)) %>%
        dplyr::distinct() %>%
        dplyr::rename(
          ATTAINS.assessmentunitidentifier = assessment_unit_identifier,
          MonitoringLocationIdentifier = monitoring_location_identifier,
          OrganizationIdentifier = monitoring_organization_identifier
        )

      rm(au.info)

      if (length(au.crosswalk$MonitoringLocationIdentifier > 0)) {
        print(paste0("TADA_GetAssessmentUnitCrosswalk: ",
          "There are ", nrow(au_crosswalk),
          " MonitoringLocationIdentifiers associated with Assessment Units for ",
          org_id, " in ATTAINS."
        ))

        return(au.crosswalk)
      }

      if (length(au.crosswalk$MonitoringLocationIdentifier) == 0) {
       print(paste0("TADA_GetAssessmentUnitCrosswalk: ",
          "No MonitoringLocationIdentifiers were recorded in ATTAINS for ",
          org_id, " Assessment Units."
        ))
      }
    }
}

#' Get Organization Identifier For All MonitoringLocations (IN ACTIVE DEVELOPMENT)
#'
#' This function creates a data frame with two columns to show the organization identifier for each
#' monitoring location. The user can select whether the monitoring location identifier column 
#' displays the original WQP 'MonitoringLocation' or the 'TADA.MonitoringLocationIdentifier'.
#'
#' @param .data A TADA data frame.
#' 
#' @param id Character argument. Determines which monitoring location identifier and organization
#' identifier from the TADA data frame are displayed. When id = "wqp", 'MonitoringLocation' is used. When id = "tada", 
#' 'TADA.MonitoringLocationIdentifier" is used. Default is id = "wqp".
#'
#'
#' @return A crosswalk of monitoring locations and organization identifiers.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
#' 

TADA_GetMonLocByOrgId <- function(.data, id = "wqp") {
  
  if(id == "wqp") {
  .data <- .data %>%
    dplyr::select(MonitoringLocationIdentifier, OrganizationIdentifier) %>%
      dplyr::distinct()
  }
  
  if(id == "tada") {
    .data <- .data %>%
      dplyr::select(TADA.MonitoringLocationIdentifier, OrganizationIdentifier) %>%
      dplyr::distinct()
  }
  
}

#' Get Provider Ref for All Organizations (IN ACTIVE DEVELOPMENT)
#'
#' This function creates 
#'
#' @return A crosswalk of monitoring locations and organization identifiers.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
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

  # If the download failed fall back to internal data (and report it)
  if (is.null(raw.data)) {
    message("Downloading latest Organization and Provider Reference Table failed!")
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


#' Update Monitoring Location Identifiers in ATTAINS
#'
#' This function creates the batch upload files needed to update the MonitoringLocations for
#' Assessment Unit profiles in ATTAINS.Users can specify whether all MonitoringLocation records
#' should be overwritten or if any new MonitoringLocations should be appended to existing records.
#'
#' @param org_id The ATTAINS organization identifier must be supplied by the user. A list of
#' organization identifiers can be found by downloading the ATTAINS Domains Excel file:
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx. Organization identifiers
#' are listed in the "OrgName" tab. The "code" column contains the organization identifiers that
#' should be used for this param.
#'
#' @param crosswalk A user-supplied data frame with the columns
#'
#'
#' @return The Assessment Unit csv batch upload files for ATTAINS.
#'
#' @export
#'
#' @examples
#' # Alaska example
#' AK_crosswalk <- TADA_GetAssessmentUnitCrosswalk(org_id = "AKDECWQ")
#'
#' \dontrun{
#'
#' }

#'
#'

# fields that are needed to create bacth upload:
#  AssessmentUnits <- ASSESSMENT_UNIT_ID, ASSESSMENT_UNIT_NAME, ASSESSMENT_UNIT_STATE, ASSESSMENT_UNIT_AGENCY,
# ASSESSMENT_UNIT_COMMENT, LOCATION_DESCRIPTION

# Water_Types <- ASSESSMENT_UNIT_ID, WATER_TYPE, WATER_SIZE, WATER_UNIT, SIZE_SOURCE,
# ESTIMATION_METHOD, SOURCE_SCALE

# Locations <- ASSESSMENT_UNIT_ID, LOCATION_TYPE_CODE, LOCATION_TYPE_CONEXT, LOCATION_TEXT

# Monitoring_Stations <- ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID, MS_DATA_LINK
# 
# 
TADA_UpdateMonitoringLocationsInATTAINS <- function(org_id = NULL, crosswalk = NULL, replace =
FALSE, attains.import = "update",
                                                    add.link = TRUE) {


  # get list of organization identifiers from ATTAINS
  org.ref <- TADA_GetATTAINSOrgIDsRef()

  # stop function if organization identifiers is not found in ATTAINS
  if (!org_id %in% org.ref$code) {
    stop(paste0("TADA_UpdateMonitoringLocationsInATTAINS: ",
                 "The organization identifier entered by user is not found in ATTAINS."))
  }

  if (org_id %in% org.ref$code) {

    # remove intermediate object
    rm(org.ref)

    # import assessment unit data from ATTAINS web services
    au.data <- rATTAINS::assessment_units(organization_id = org_id)

    # create assessment unit crosswalk from ATTAINS
    attains.crosswalk <- au.data %>%
      tidyr::unnest(monitoring_stations) %>%
      dplyr::select(assessment_unit_identifier, monitoring_organization_identifier,
                    monitoring_location_identifier, monitoring_data_link_text
      ) %>%
      dplyr::filter(!is.na(monitoring_location_identifier)) %>%
      dplyr::distinct() %>%
      dplyr::rename(
        ASSESSMENT_UNIT_ID = assessment_unit_identifier,
        MS_ORG_ID = monitoring_organization_identifier,
        MS_LOCATION_ID = monitoring_location_identifier,
        MONITORING_DATA_LINK_TEXT = monitoring_data_link_text
      )

    if (!is.data.frame(crosswalk)) {
      stop(paste0("TADA_UpdateMonitoringLocationsInATTAINS: ",
                  "A crosswalk data frame with columns 'ATTAINS.assessmentunit.identifier' and ",
                  "'MonitoringLocationIdentifier' is required to run this function."))}

    if(is.data.frame(crosswalk)) {

      if(!c("ASSESSMENT_UNIT_ID", "MS_ORG_ID", "MS_LOCATION_ID", "MONITORING_DATA_LINK_TEXT") %in%
         names(crosswalk)) {

        stop(paste0("TADA_UpdateMonitoringLocationsInATTAINS: ",
                    "The user-supplied crosswalk data frame does not contain all required columns: ",
                    "ASSESSMENT_UNIT_ID, MS_ORG_IG, MS_LOCATION_ID, and MONITORING_DATA_LINK_TEXT."))
      }

      if(c("ASSESSMENT_UNIT_ID", "MS_ORG_ID", "MS_LOCATION_ID", "MONITORING_DATA_LINK_TEXT") %in%
         names(crosswalk)){

        update.crosswalk <- attains.crosswalk %>%
          dplyr::filter(ASSESSMENT_UNIT_ID %in% crosswalk$ASSESSMENT_UNIT_ID)


        if(replace = FALSE) {

          update.crosswalk2 <- update.crosswalk %>%
            rbind(attains.crosswalk) %>%
            dplyr::distinct()
        }

# when replace is true, should existing info for data links be considered/retained?

        if(replace = TRUE) {

          update.crosswalk2 <- crosswalk

        }

        # trying to add data links - need to learn more about WQP web services (HRM 12/26/24)

        provider.ref <- TADA_GetProviderRef() %>%
          dplyr::rename(MS_ORG_ID = OrganizationIdentifier) %>%
          dplyr::mutate(OrgIDForURL = MS_ORG_ID)
          
        
        add.orgs <- provider.ref %>%
          dplyr::filter(grepl("_WQX", MS_ORG_ID)) %>%
          dplyr::rename(OrgIDForURL = MS_ORG_ID) %>%
          dplyr::mutate(MS_ORG_ID = stringr::str_remove_all(OrgIDForURL,
                                                                         "_WQX"))
        
        
        update.crosswalk3 <- update.crosswalk %>%
          dplyr::left_join(provider.ref, by = dplyr::join_by(MS_ORG_ID))
        
        matched.crosswalks <- update.crosswalk3 %>%
          dplyr::filter(!is.na(OrgIDForURL))
        
        nomatch.org <- update.crosswalk3 %>%
          dplyr::filter(is.na(OrgIDForURL)) %>%
          dplyr::select(-OrgIDForURL, -OrganizationFormalName, -ProviderName) %>%
          dplyr::left_join(add.orgs) 
        
        total <- matched.crosswalks %>%
          dplyr::bind_rows(nomatch.org)
        
        # next build the URLS for ms location url
        
         

        

    }



    ## Commented out for now as per conversation w/ WR on 12/12/24 (these additional files may
    ## not be necessary to update the ML/AUIDs in ATTAINS, will need to test in Demo ATTAINS)
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

  }
}



