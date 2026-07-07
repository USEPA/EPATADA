# Used to store cached tribal feature layers

#' Function to update tribal layer shapefiles.
#' Tribal geopackage is stored in inst/extdata.
#' Existing layers with the same name will be deleted first if they exist.

TADA_UpdateTribalLayers <- function() {
  writeLayer(AKAllotmentsUrl, "inst/extdata/Tribal.gpkg", "AKAllotments")
  writeLayer(AKVillagesUrl, "inst/extdata/Tribal.gpkg", "AKVillages")
  writeLayer(AmericanIndianUrl, "inst/extdata/Tribal.gpkg","AmericanIndian")
  writeLayer(OffReservationUrl, "inst/extdata/Tribal.gpkg", "OffReservation")
  writeLayer(OKTribeUrl, "inst/extdata/Tribal.gpkg", "OKTribe")
  writeLayer(VATribeUrl, "inst/extdata/Tribal.gpkg", "VATribe")
}
