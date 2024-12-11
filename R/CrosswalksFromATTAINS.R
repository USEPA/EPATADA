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
  if (is.null(org_id)) {
    stop("An organization identifier must be supplied to create a crosswalk.")
  } else {
    
    org.ref <- TADA_GetATTAINSOrgIDsRef()

    if (!org_id %in% org.ref$code) {
      stop("The organization identifier entered by user is not found in ATTAINS.")
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
        print(paste0(
          "There are ", nrow(au_crosswalk),
          " MonitoringLocationIdentifiers associated with Assessment Units for ",
          org_id, " in ATTAINS."
        ))

        return(au.crosswalk)
      }

      if (length(au.crosswalk$MonitoringLocationIdentifier) == 0) {
        stop(paste0(
          "No MonitoringLocationIdentifiers were recorded in ATTAINS for ",
          org_id, " Assessment Units."
        ))
      }
    }
  }
}
