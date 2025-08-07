#' TADA_CreateAUMLRef
#'
#' Create the assessment unit and monitoring location ref by utilizing an optional
#' user-supplied crosswalk, AU/ML crosswalk from ATTAINS (if org has entered that data),
#' and TADA_GetATTAINS to match unassigned monitoring locations to assessment units.
#'
#' @param .data A dataframe created by `TADA_DataRetrieval()`.
#' @param au_ref Optional. A user-supplied df with the columns AssessmentUnitIdentifier
#' and MonitoringLocationIdentifier.
#' @param org_id Organization id to match AUs.
#' @param add_catch Optional. When add_catch = TRUE, catchments are matched to monitoring
#' locations from  the user-supplied and ATTAINS crosswalk monitoring locations. Fetching
#' and matching these additional geospatial data will increase the run time of this function
#' significantly. Default is add_catch = FALSE.
#'
#' @return Need to add the full list
#'
#' @seealso [TADA_GetATTAINS()] # add additional functions here
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
 TADA_CreateAUMLRef <- function(.data, au_ref = NULL,
                                org_id = NULL, add_catch = FALSE) {
   # need to write checks for each component

   # check for user supplied ref
   if(is.null(au_ref)) {

     user.matches <- list(
       "TADA_with_ATTAINS" = NULL,
       "ATTAINS_catchments" = NULL,
       "ATTAINS_points" = NULL,
       "ATTAINS_lines" = NULL,
       "ATTAINS_polygons" = NULL
     )
   }

   if(!is.null(au_ref)) {

   if(!is.data.frame(au_ref)) {
        stop("The user supplied au_ref must be a data frame.")
     }

     if(is.data.frame(au_ref)) {

       req.cols <- c("AssessmentUnitIdentifier",
                     "MonitoringLocationIdentifier")

       # should this be using a more generic function?
       TADA_CheckColumns(au_ref, req.cols)

   # rename au_ref cols for nex function
    au_ref <- au_ref %>%
         dplyr::rename(ATTAINS.MonitoringLocationIdentifier = MonitoringLocationIdentifier,
                       ATTAINS.AssessmentUnitIdentifier = AssessmentUnitIdentifier)

   # subset data for au_ref
   au.ref.mls <- .data %>%
     dplyr::filter(TADA.MonitoringLocationIdentifier %in% au_ref$ATTAINS.MonitoringLocationIdentifier) %>%
     dplyr::mutate(TADA.AURefSource = "User-supplied Ref")

   # get geospatial data for au_ref monitoring locations
   user.matches <- TADA_GetATTAINSByAUID(au.ref.mls, au_ref = au_ref, add_catch = add_catch)
     }
   }

   # ATTAINS supplied ref section
   # get attains crosswalk
   attains.cw <- TADA_GetATTAINSAUSiteCrosswalk(org_id = org_id)

   if(is.null(attains.cw)) {

      attains.matches <- list(
        "TADA_with_ATTAINS" = NULL,
        "ATTAINS_catchments" = NULL,
        "ATTAINS_points" = NULL,
        "ATTAINS_lines" = NULL,
        "ATTAINS_polygons" = NULL
      )
   }

   if(!is.null(attains.cw)) {
   # we could remove or make this step optional, but it is very helpful for making sure
     # monitoring location identifiers are WQP compatible
     attains.cw <- TADA_UpdateMonitoringLocationsInATTAINS(crosswalk = attains.cw,
                                                         org_id = org_id,
                                                         attains_replace = TRUE)

   attains.cw.mls <- .data %>%
     dplyr::filter(!TADA.MonitoringLocationIdentifier %in% au.ref.mls$TADA.MonitoringLocationIdentifier,
                   TADA.MonitoringLocationIdentifier %in% attains.cw$ATTAINS.MonitoringLocationIdentifier) %>%
     dplyr::mutate(TADA.AURefSource = "ATTAINS crosswalk")

   # get geospatial data for attains cw monitoring locations
   attains.matches <- TADA_GetATTAINSByAUID(attains.cw.mls, au_ref = attains.cw, add_catch = add_catch)
   }

   # TADA_GetATTAINS section
   get.attains.mls <- .data %>%
     dplyr::filter(!TADA.MonitoringLocationIdentifier %in% au.ref.mls$TADA.MonitoringLocationIdentifier,
                   !TADA.MonitoringLocationIdentifier %in% attains.cw.mls$TADA.MonitoringLocationIdentifier) %>%
     dplyr::mutate(TADA.AURefSource = "TADA_GetATTAINS")

   # add code here for if there are no remaning mls to match
   if(dim(get.attains.mls)[1] == 0) {

     get.attains.matches <- list(
       "TADA_with_ATTAINS" = NULL,
       "ATTAINS_catchments" = NULL,
       "ATTAINS_points" = NULL,
       "ATTAINS_lines" = NULL,
       "ATTAINS_polygons" = NULL
     )

   }

    if(dim(get.attains.mls)[1] > 0) {
   # use get attains for matching remaining monitoring locations
   get.attains.matches <- TADA_GetATTAINS(get.attains.mls,
                                          return_nearest = TRUE)
    }

   # need to figure out what happens here if no matches are found in ATTAINS

   # join all the resulting tables within each list to return as one large list
   #TADA_with_ATTAINS

   TADA_with_ATTAINS <- user.matches$TADA_with_ATTAINS %>%
     plyr::rbind.fill(attains.matches$TADA_with_ATTAINS) %>%
     plyr::rbind.fill(get.attains.matches$TADA_with_ATTAINS) %>%
     plyr::rbind.fill()

   ATTAINS_catchments <- user.matches$ATTAINS_catchments %>%
     plyr::rbind.fill(attains.matches$ATTAINS_catchments) %>%
     plyr::rbind.fill(get.attains.matches$ATTAINS_catchments) %>%
     dplyr::distinct()

   ATTAINS_lines <- user.matches$ATTAINS_lines %>%
     plyr::rbind.fill(attains.matches$ATTAINS_lines) %>%
     plyr::rbind.fill(get.attains.matches$ATTAINS_lines) %>%
     plyr::rbind.fill()

   ATTAINS_points <- user.matches$ATTAINS_points %>%
     plyr::rbind.fill(attains.matches$ATTAINS_points) %>%
     plyr::rbind.fill(get.attains.matches$ATTAINS_points) %>%
     dplyr::distinct()

   ATTAINS_polygons <- user.matches$ATTAINS_polygons %>%
     plyr::rbind.fill(attains.matches$ATTAINS_polygons) %>%
     plyr::rbind.fill(get.attains.matches$ATTAINS_polygons) %>%
     dplyr::distinct()


   final_list <- list(
     "TADA_with_ATTAINS" = TADA_with_ATTAINS,
     "ATTAINS_catchments" = ATTAINS_catchments,
     "ATTAINS_points" = ATTAINS_points,
     "ATTAINS_lines" = ATTAINS_lines,
     "ATTAINS_polygons" = ATTAINS_polygons
   )

   return(final_list)
}
