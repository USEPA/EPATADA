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
#' https://www.epa.gov/system/files/other-files/2023-09/DOMAINS.xlsx.
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