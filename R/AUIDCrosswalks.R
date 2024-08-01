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

TADA_CreateAUIDCrosswalk <- function(.data, return_sf = FALSE, attains_orgid = "null") {
  
  if(return_sf == FALSE) {
  
  .data <- TADA_GetATTAINS(.data, return_sf = FALSE)
  
  select.cols <- c("MonitoringLocationIdentifier",  "OrganizationIdentifier",
                   "OrganizationFormalName", "ATTAINS.assessmentunitidentifier", 
                   "ATTAINS.assessmentunitidentifier, ATTAINS.organizationid",
                   "ATTAINS.organizationname")
  }
  
  if(return_sf == FALSE) {
    
    .data <- TADA_GetATTAINS(.data, return_sf = TRUE)
    
    select.cols <- c("MonitoringLocationIdentifier",  "OrganizationIdentifier",
                     "OrganizationFormalName", "ATTAINS.assessmentunitidentifier", 
                     "ATTAINS.assessmentunitidentifier, ATTAINS.organizationid",
                     "ATTAINS.organizationname")
  }
  
  .data <- .data %>%
    dplyr::select(MonitoringLocationIdentifier, OrganizationIdentifier,
                  OrganizationFormalName, ATTAINS.assessmentunitidentifier,
                  ATTAINS.assessmentunitidentifier, ATTAINS.organizationid,
                  ATTAINS.organizationname) %>%
    dplyr::distinct() %>%
    dplyr::mutate(TADA.AUIDCrosswalk.Flag = case_when(!is.na(ATTAINS.assessmentunitidentifier) ~ "No AUID identified",
                                                      length(ATTAINS.assessmentunitidentifier > 1 ~ "MonitoringLocation matched with multiple Assessment Units")))
  

  
  
}

#' function to compare cross walk to old crosswalk
#' 
#' function to format crosswalk for attains batch upload (use attains webservices to populate other info)