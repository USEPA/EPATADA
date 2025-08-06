#' #' TADA_CreateAUMLRef
#' #'
#' #' Create the assessment unit and monitoring location ref by utilizing an optional
#' #' user-supplied crosswalk, AU/ML crosswalk from ATTAINS (if org has entered that data),
#' #' and TADA_GetATTAINS to match unassigned monitoring locations to assessment units.
#' #'
#' #' @param .data A dataframe created by `TADA_DataRetrieval()`.
#' #' @param au_ref Optional. A user-supplied df with the columns AssessmentUnitIdentifier
#' #' and MonitoringLocationIdentifier.
#' #' @param org_id Organization id to match AUs.
#' #'
#' #' @return Need to add the full list
#' #'
#' #' @seealso [TADA_GetATTAINS()] # add additional functions here
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #'
#' TADA_CreateAUMLRef <- function(.data, au_ref = NULL, org_id = NULL) {
#'   # need to write checks for each component
#'
#'   # user supplied au_ref section
#'   # subset data for au_ref
#'   au.ref.mls <- .data %>%
#'     dplyr::filter(TADA.MonitoringLocationIdentifier %in% au_ref$MonitoringLocationIdentifier)
#'
#'   # rename au_ref cols for nex function
#'   au_ref <- au_ref %>%
#'     dplyr::rename(ATTAINS.MonitoringLocationIdentifier = MonitoringLocationIdentifier,
#'                   ATTAINS.AssessmentUnitIdentifier = AssessmentUnitIdentifier)
#'
#'   # get geospatial data for au_ref monitoring locations
#'   user.matches <- TADA_GetATTAINSByAUID(au.ref.mls, au_ref = au_ref)
#'
#'   # ATTAINS supplied ref section
#'   # get attains crosswalk
#'   attains.cw <- TADA_GetATTAINSAUSiteCrosswalk(org_id = org_id)
#'
#'   attains.cw <- TADA_UpdateMonitoringLocationsInATTAINS(crosswalk = attains.cw,
#'                                                         org_id = org_id,
#'                                                         attains_replace = TRUE)
#'
#'   attains.cw.mls <- .data %>%
#'     dplyr::filter(TADA.MonitoringLocationIdentifier %in% attains.cw$ATTAINS.MonitoringLocationIdentifier)
#'
#'
#'
#' }
#' 1. user
#' 2. ATTAINS
#' 3. GetATTAINS
#'
#' flag for review site that was assigned in two different ways
#'
