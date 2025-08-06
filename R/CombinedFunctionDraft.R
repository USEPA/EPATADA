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
 TADA_CreateAUMLRef <- function(.data, au_ref = NULL, org_id = NULL) {
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
   user.matches <- TADA_GetATTAINSByAUID(au.ref.mls, au_ref = au_ref)
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
   attains.cw <- TADA_UpdateMonitoringLocationsInATTAINS(crosswalk = attains.cw,
                                                         org_id = org_id,
                                                         attains_replace = TRUE)

   attains.cw.mls <- .data %>%
     dplyr::filter(!TADA.MonitoringLocationIdentifier %in% au.ref.mls$TADA.MonitoringLocationIdentifier,
                   TADA.MonitoringLocationIdentifier %in% attains.cw$ATTAINS.MonitoringLocationIdentifier) %>%
     dplyr::mutate(TADA.AURefSource = "ATTAINS crosswalk")

   # get geospatial data for attains cw monitoring locations
   attains.matches <- TADA_GetATTAINSByAUID(attains.cw.mls, au_ref = attains.cw)
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
     dplyr::bind_rows(attains.matches$TADA_with_ATTAINS) %>%
     dplyr::bind_rows(get.attains.matches$TADA_with_ATTAINS) %>%
     dplyr::distinct()

   ATTAINS_catchments <- user.matches$ATTAINS_catchments %>%
     dplyr::bind_rows(attains.matches$ATTAINS_catchments) %>%
     dplyr::bind_rows(get.attains.matches$ATTAINS_catchments) %>%
     dplyr::distinct()

   ATTAINS_lines <- user.matches$ATTAINS_lines %>%
     dplyr::bind_rows(attains.matches$ATTAINS_lines) %>%
     dplyr::bind_rows(get.attains.matches$ATTAINS_lines) %>%
     dplyr::distinct()

   ATTAINS_points <- user.matches$ATTAINS_points %>%
     dplyr::bind_rows(attains.matches$ATTAINS_points) %>%
     dplyr::bind_rows(get.attains.matches$ATTAINS_points) %>%
     dplyr::distinct()

   ATTAINS_polygons <- user.matches$ATTAINS_polygons %>%
     dplyr::bind_rows(attains.matches$ATTAINS_polygons) %>%
     dplyr::bind_rows(get.attains.matches$ATTAINS_polygons) %>%
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



# test data sets

 # get Montna bacteria data
 tada.MT <- TADA_DataRetrieval(
   startDate = "2020-01-01", endDate = "2022-12-31",
   statecode = "MT",
   characteristicName = c("Escherichia", "Escherichia coli", "pH"),
   ask = FALSE)

 # review comparable data identifiers
 sort(unique(tada.MT$TADA.ComparableDataIdentifier))

 # clean up data set (minimal)
 tada.MT.clean <- tada.MT %>%
   TADA_RunKeyFlagFunctions() %>%
   TADA_SimpleCensoredMethods() %>%
   # could separate this out and add to harmonization table?
   TADA_HarmonizeSynonyms()

 rm(tada.MT)

 # review comparable data identifiers
 sort(unique(tada.MT.clean$TADA.ComparableDataIdentifier))

 attains.existing.MT <- TADA_GetATTAINSAUSiteCrosswalk(org_id = "MTDEQ")

 clean.existing.attains.MT <- TADA_UpdateMonitoringLocationsInATTAINS(org_id = "MTDEQ")

 rm(attains.existing.MT)

 au_ref <- clean.existing.attains.MT %>%
   dplyr::slice_head(n = 50) %>%
   dplyr::rename(MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier,
                 AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier)

 attains.cw <- clean.existing.attains.MT %>%
   dplyr::anti_join(au_ref)

 rm(clean.existing.attains.MT)




