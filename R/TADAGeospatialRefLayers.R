#' Get path to the bundled tribal geopackage
#'
#' Internal helper to locate the Tribal.gpkg file stored in package extdata.
#'
#' @keywords internal
#' @noRd
get_tribal_gpkg_path <- function() {
  gpkg <- system.file("extdata", "Tribal.gpkg", package = "EPATADA")

  if (identical(gpkg, "")) {
    stop("Tribal.gpkg not found in package extdata.")
  }

  gpkg
}

# Used to store cached tribal feature layers

#' Function to update tribal layer shapefiles.
#' Tribal geopackage is stored in inst/extdata.
#' Existing layers with the same name will be deleted first if they exist.
#' @param tribal_gpkg full path to the tribal geopackage file. Default is "inst/extdata/Tribal.gpkg".

TADA_UpdateTribalLayers <- function(tribal_gpkg = NULL) {
  if (is.null(tribal_gpkg)) {
    tribal_gpkg <- get_tribal_gpkg_path()
  }

  writeLayerIfChanged(AKAllotmentsUrl, tribal_gpkg, "AKAllotments")
  writeLayerIfChanged(AKVillagesUrl, tribal_gpkg, "AKVillages")
  writeLayerIfChanged(AmericanIndianUrl, tribal_gpkg, "AmericanIndian")
  writeLayerIfChanged(OKTribeUrl, tribal_gpkg, "OKTribe")
  writeLayerIfChanged(OffReservationUrl, tribal_gpkg, "OffReservation")
  writeLayerIfChanged(VATribeUrl, tribal_gpkg, "VATribe")
}
