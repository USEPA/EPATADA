testgis <- sf::read_sf("C:/Users/hmarler/OneDrive - Environmental Protection Agency (EPA)/Desktop/GIS_2020_PUEBLOOFTESUQUE/PUEBLOOFTESUQUE.shp")

testdat <- Data_TribalNations_Harmonized |>
  dplyr::filter(OrganizationFormalName == "Blackfeet Nation (Montana)")

testpoints <- TADA_CreatePointAUs(testdat)

#' Creates a GIS file of Assessment Unit point geometry to upload to ATTAINS
#'
#' @param .data A data frame containing:
#'
#'   - `ATTAINS.AssessmentUnitIdentifier`
#'   - `TADA.LongitudeMeasure`
#'   - `TADA.LatitudeMeasure`
#' @param target_crs Numeric. The target crs projection for upload to ATTAINS.
#' Default equals 4269 (NAD83).
#'
#' @return A distinct AU–ML crosswalk data frame containing:
#'   - `ATTAINS.MonitoringLocationIdentifier`
#'   - `ATTAINS.AssessmentUnitIdentifier`
#'   - `ATTAINS.WaterType`
#'
#' @details
#' - Missing `ATTAINS.AssessmentUnitIdentifier`
#'   values are replaced with `TADA.MonitoringLocationIdentifier`.
#' - If `auid_prefix` is supplied and non-empty, it is included for only
#'   newly created AUIDs.
#' - `ATTAINS.MonitoringLocationIdentifier` is created from
#'   `TADA.MonitoringLocationIdentifier`.
#' - `ATTAINS.WaterType` is not overwritten unless it is missing or blank.
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
TADA_CreatePointAUGeometry <- function(.data, target_crs = 4269) {

  req <- c("ATTAINS.AssessmentUnitIdentifier",
           "TADA.LongitudeMeasure",
           "TADA.LatitudeMeasure",
           "HorizontalCoordinateReferenceSystemDatumName")

  if (!all(req %in% names(.data))) {
    stop(
      "TADA_CreatePointAUGeometry: Input data must contain ATTAINS.AssessmentUnitIdentifier, ",
      "TADA.LongitudeMeasure, TADA.LatitudeMeasure, and HorizontalCoordinateReferenceSystemDatumName"
    )
  }

  .data <- .data |>
    dplyr::select(dplyr::all_of(req)) |>
    dplyr::filter(!is.na(TADA.LongitudeMeasure), !is.na(TADA.LatitudeMeasure))

  sf_pts <- sf::st_as_sf(
    .data,
    coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
    crs = 4269,
    remove = TRUE
  ) |>
    sf::st_transform(crs = target_crs)

  sf_out <- sf_pts |>
    dplyr::group_by(ATTAINS.AssessmentUnitIdentifier) |>
    dplyr::summarise(
      n_pts = dplyr::n(),
      geometry = if (n_pts[1] == 1) {
        geometry[[1]]
      } else {
        sf::st_multipoint(do.call(rbind, sf::st_coordinates(geometry)[, 1:2, drop = FALSE]))
      },
      .groups = "drop"
    )

  sf::st_as_sf(sf_out)
}

# need to create AU batch upload file


# need to create AU with MLs batch upload file
