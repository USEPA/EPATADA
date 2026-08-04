testgis <- sf::read_sf("C:/Users/hmarler/OneDrive - Environmental Protection Agency (EPA)/Desktop/GIS_2020_PUEBLOOFTESUQUE/PUEBLOOFTESUQUE.shp")

testdat <- Data_TribalNations_Harmonized |>
  dplyr::filter(OrganizationFormalName == "Blackfeet Nation (Montana)")

testdat <- testdat |>
  dplyr::rename(ATTAINS.AssessmentUnitIdentifier = TADA.MonitoringLocationIdentifier)

testpoints <- TADA_CreatePointAUs(testdat)

#' Fill missing ATTAINS Assessment Unit Identifiers
#'
#' @keywords internal
#' @noRd
fill_missing_assessment_unit_id <- function(.data, auid_prefix = NULL) {
  has_mloc <- "TADA.MonitoringLocationIdentifier" %in% names(.data)
  has_auid <- "ATTAINS.AssessmentUnitIdentifier" %in% names(.data)

  if (!has_mloc && !has_auid) {
    stop(
      "At least one of 'TADA.MonitoringLocationIdentifier' or ",
      "'ATTAINS.AssessmentUnitIdentifier' must be present."
    )
  }

  # If target column doesn't exist, create it
  if (!has_auid) {
    .data$ATTAINS.AssessmentUnitIdentifier <- NA_character_
  }

  # If source column doesn't exist, we can still return .data unchanged
  # unless you want to error when filling is impossible.
  if (!has_mloc) {
    return(.data)
  }

  created_AUID <- is.na(.data$ATTAINS.AssessmentUnitIdentifier) |
    trimws(as.character(.data$ATTAINS.AssessmentUnitIdentifier)) == ""

  if (any(created_AUID)) {
    .data$ATTAINS.AssessmentUnitIdentifier[created_AUID] <-
      .data$TADA.MonitoringLocationIdentifier[created_AUID]
  }

  if (!is.null(auid_prefix) && nzchar(auid_prefix)) {
    .data$ATTAINS.AssessmentUnitIdentifier[created_AUID] <- paste0(
      auid_prefix,
      .data$ATTAINS.AssessmentUnitIdentifier[created_AUID]
    )
  }

  .data
}

#' Creates a GIS file of Assessment Unit point geometry to upload to ATTAINS
#'
#' @param .data A data frame containing:
#'
#'   - `ATTAINS.AssessmentUnitIdentifier` or `TADA.MonitoringLocationIdentifier`
#'   - `TADA.LongitudeMeasure`
#'   - `TADA.LatitudeMeasure`
#'   - `HorizontalCoordinateReferenceSystemDatumName`
#' @param target_crs Numeric. The target crs projection for upload to ATTAINS.
#' Default equals 4269 (NAD83).
#' @param download_geo Boolean argument. When download equals TRUE, the GIS file
#' containing the assessment unit identifier and point geometry will be
#' downloaded into the user's downloads folder. When download equals FALSE,
#' nothing is downloaded and the df containing the columns "AU_ID" (assessment
#' unit identifier) and geometry is returned. The default is download_geo
#' equals FALSE.
#' @param auid_prefix Character or `NULL`. If provided and non-empty, this
#' prefix is included only for newly created `ATTAINS.AssessmentUnitIdentifier`
#' values that were filled from `TADA.MonitoringLocationIdentifier`. Existing
#' non-missing AUIDs are not modified. Use `NULL` to skip prefixing.
#'
#' @return When download_geo equals FALSE, a df containing the columns:
#'   - `ATTAINS.MonitoringLocationIdentifier`
#'   - `geometry`
#'
#' When download_geo equals TRUE, a shp file in the user's downloads folder
#' with the columns:
#'  - `AU_ID`
#'  - `geometry`
#'
#' @details
#' - Missing `ATTAINS.AssessmentUnitIdentifier`
#'   values are replaced with `TADA.MonitoringLocationIdentifier`.
#' - If `auid_prefix` is supplied and non-empty, it is included for only
#'   newly created AUIDs.
#'
#' @seealso [TADA_CrosswalkATTAINSWaterTypes()]
#'
#' @examples
#' \dontrun{
#' # Example 1: Create missing AUIDs
#' ex_df <- data.frame(
#'   TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2", "LOC3"),
#'   TADA.MonitoringLocationTypeName   = c("Stream", "Lake", "Estuary"),
#'   ATTAINS.AssessmentUnitIdentifier  = c(NA_character_, "EXISTING_AU_001", ""),
#'   ATTAINS.WaterType                 = c(NA_character_, "", "ESTUARY"),
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- TADA_CreatePointAUs(ex_df)
#'
#' # Example 2: Prefix only newly created AUIDs
#' result_prefixed <- TADA_CreatePointAUs(
#'   ex_df,
#'   auid_prefix = "WQX_"
#' )
#'
#' # Example 3: AUID column is absent entirely
#' ex_df2 <- data.frame(
#'   TADA.MonitoringLocationIdentifier = c("SITE_A", "SITE_B"),
#'   TADA.MonitoringLocationTypeName   = c("River/Stream", "Lake, Reservoir, Impoundment"),
#'   ATTAINS.WaterType                 = c(NA_character_, NA_character_),
#'   stringsAsFactors = FALSE
#' )
#'
#' result_missing_auid <- TADA_CreatePointAUs(ex_df2)
#' }
#'
#' @export
TADA_CreatePointAUGeometry <- function(.data,
                                       target_crs = 4269,
                                       download_geo = FALSE) {

  # always required columns
  req <- c("TADA.LongitudeMeasure",
           "TADA.LatitudeMeasure",
           "HorizontalCoordinateReferenceSystemDatumName")

  # columns that can be used for AUID (at least one is required)
  # if both are present, function will use ATTAINS.AssessmentUnitIdentifier
  auid <- c("TADA.MonitoringLocationIdentifier",
            "ATTAINS.AssessmentUnitIdentifier")



  # check to see if all required columns and any id cols are in .data
  if (!all(req %in% names(.data)) & !any(auid %in% names(.data))) {
    stop(
      "TADA_CreatePointAUGeometry: Input data must contain ATTAINS.AssessmentUnitIdentifier, ",
      "TADA.LongitudeMeasure, TADA.LatitudeMeasure, and HorizontalCoordinateReferenceSystemDatumName"
    )
  }

  # determine id col
  if("ATTAINS.AssessmentUnitIdentifier" %in% names(.data)) {

    id.col <- "ATTAINS.AssessmentUnitIdentifier"
  } else {
    id.col <- "TADA.MonitoringLocationIdentifier"
  }

  # add AUID prefix if needed
  if(id.col == "TADA.MonitoringLocationIdentifier" & !is.null(auid_prefix)) {

    .data <- fill_missing_assessment_unit_id(.data,
                                             auid_prefix = auid_prefix)

    id.col <- "ATTAINS.AssessmentUnitIdentifier"

  }

  .data <- .data |>
    dplyr::select(rlang::sym(id.col), dplyr::all_of(req)) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(TADA.LongitudeMeasure), !is.na(TADA.LatitudeMeasure))

  # create point geometries
  sf_pts <- sf::st_as_sf(
    .data,
    coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
    crs = 4269,
    remove = TRUE
  ) |>
    sf::st_transform(crs = target_crs)

  # check for multipoints
  sf_out <- sf_pts |>
    dplyr::group_by(ATTAINS.AssessmentUnitIdentifier) |>
    dplyr::summarise(
      n_pts = dplyr::n(),
      geometry = {
        grp_geom <- geometry
        coords <- sf::st_coordinates(grp_geom)[, 1:2, drop = FALSE]

        if (n_pts[1] == 1) {
          sf::st_sfc(grp_geom[[1]], crs = sf::st_crs(sf_pts))
        } else {
          sf::st_sfc(sf::st_multipoint(coords), crs = sf::st_crs(sf_pts))
        }
      },
      .groups = "drop"
    ) |>
    dplyr::select(-n_pts)

  sf::st_as_sf(sf_out)

  if(isFALSE(download_geo)) {

    return(sf_out)
  } else {

    shp.path <- .get_downloads_path()
  }


}

# need to create AU batch upload file


# need to create AU with MLs batch upload file
