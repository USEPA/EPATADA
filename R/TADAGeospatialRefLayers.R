# Used to store cached tribal feature layers

#' Function to update tribal layer shapefiles.
#' Tribal geopackage is stored in inst/extdata.
#' Existing layers with the same name will be deleted first if they exist.

TADA_UpdateTribalLayers <- function() {
  writeLayerIfChanged(AKAllotmentsUrl, "inst/extdata/Tribal.gpkg", "AKAllotments")
  writeLayerIfChanged(AKVillagesUrl, "inst/extdata/Tribal.gpkg", "AKVillages")
  writeLayerIfChanged(AmericanIndianUrl, "inst/extdata/Tribal.gpkg", "AmericanIndian")
  writeLayerIfChanged(OKTribeUrl, "inst/extdata/Tribal.gpkg", "OKTribe")
  writeLayerIfChanged(OffReservationUrl, "inst/extdata/Tribal.gpkg", "OffReservation")
  writeLayerIfChanged(VATribeUrl, "inst/extdata/Tribal.gpkg", "VATribe")
}
