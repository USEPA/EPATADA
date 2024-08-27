#' Create Assessment Unit Crosswalk
#'
#' This function creates a data frame crosswalk between MonitoringLocationIdentifier and
#' ATTAINS.assessmentunitidentifier by leveraging TADA_GetATTAINS. The crosswalk should be
#' reviewed before use in criteria comparison or impairment decision functions.
#'
#' @param .data TADA dataframe
#'
#' @param return_sf Boolean argument. When return_sf = TRUE, geometry from TADA_GetATTAINS will be
#' returned as part of the data frame. When return_sf = FALSE, no geometry will be returned. The
#' default is return_sf = FALSE. To user TADA_ViewATTAINS on the resulting data frame, the user must
#' set return_sf = TRUE.
#'
#' @param attains_orgid Character argument. If the user supplies an organization id, the data frame
#' will be filtered to retain only assessment units from the specified organization. Default is "null".
#' When "null", no organization filter is applied and all assessment units relevant to MonitoringLocations
#' in TADA data frame will be returned.
#' 
#' @param watertype_check Boolean argument. When watertype_check = TRUE, waterbody types from the WQP
#' MonitoringLocations ("MonitoringLocationTypeName") and ATTAINS ()

#'
#' @return A data frame with four columns: MonitoringLocationIdentifier, ATTAINS.assessmentunitidentifier
#' ATTAINS.assessmentunitidentifier and TADA.AUIDCrosswalkFlag.
#'
#' @export
#'
#' @examples
#' # create criteria reference for Utah nutrients example data set
#' UT_CriteriaRef <- TADA_CreateSimpleCriteriaRef(Data_Nutrients_UT)
#'
#'

# TADA_CreateAUIDCrosswalk <- function(.data, return_sf = FALSE, attains_orgid = "null") {
# 
#   unique_mls <- .data %>%
#     dplyr::select(MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName,
#                   OrganizationIdentifier, OrganizationFormalName,
#                   LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName) %>%
#     dplyr::distinct()
#   
#   # add comparison of waterbody types in attains and wqp
# 
#   if(return_sf == FALSE) {
# 
#   unique_attains <- TADA_GetATTAINS(unique_mls, return_sf = FALSE)
#   
#   crosswalk <- unique_attains %>%
#     dplyr::select(index, MonitoringLocationIdentifier, OrganizationIdentifier,
#                   OrganizationFormalName, ATTAINS.assessmentunitidentifier,
#                   ATTAINS.assessmentunitidentifier, ATTAINS.organizationid,
#                   ATTAINS.organizationname) %>%
#     dplyr::distinct() %>%
#     dplyr::group_by(index) %>%
#     dplyr::mutate(N_AUID = length(MonitoringLocationIdentifier),
#                   TADA.AUIDCrosswalk.Flag = dplyr::case_when(is.na(ATTAINS.assessmentunitidentifier) ~ "No AUID identified",
#                                                              N_AUID > 1 ~ "MonitoringLocation matched with multiple AssessmentUnits",
#                                                              !is.na(ATTAINS.assessmentunitidentifier) & N_AUID == 1 ~ "MonitoringLocation matched with one AssessmentUnit"))
#   
#   rm()
#   }
# 
#   if(return_sf == TRUE) {
# 
#     unique_attains_sf <- unique_mls %>%
#       TADA_GetATTAINS(return_sf = TRUE)
#     
#     testcross <- unique_attains %>%
#       dplyr::select(index, MonitoringLocationIdentifier, OrganizationIdentifier,
#                     OrganizationFormalName, ATTAINS.assessmentunitidentifier,
#                     ATTAINS.assessmentunitidentifier, ATTAINS.organizationid,
#                     ATTAINS.organizationname) %>%
#       dplyr::distinct() %>%
#       dplyr::group_by(index) %>%
#       dplyr::mutate(N_AUID = length(MonitoringLocationIdentifier),
#                     TADA.AUIDCrosswalk.Flag = dplyr::case_when(is.na(ATTAINS.assessmentunitidentifier) ~ "No AUID identified",
#                                                                N_AUID > 1 ~ "MonitoringLocation matched with multiple AssessmentUnits",
#                                                                !is.na(ATTAINS.assessmentunitidentifier) & N_AUID == 1 ~ "MonitoringLocation matched with one AssessmentUnit"))
# 
# 
#   }
# }

#' function to compare cross walk to old crosswalk
#'
#' function to format crosswalk for attains batch upload (use attains webservices to populate other info)