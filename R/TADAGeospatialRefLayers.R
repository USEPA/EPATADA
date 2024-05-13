# Used to store cached tribal feature layers

#' Function to update tribal layer shapefiles.
#' Shapefiles are stored in inst/extdata.
#' Existing shapefiles with the same name will be deleted first if they exist.

TADA_UpdateTribalLayers <- function() {
  writeLayer(AKAllotmentsUrl, "inst/extdata/AKAllotments.shp")
  writeLayer(AKVillagesUrl, "inst/extdata/AKVillages.shp")
  writeLayer(AmericanIndianUrl, "inst/extdata/AmericanIndian.shp")
  writeLayer(OffReservationUrl, "inst/extdata/OffReservation.shp")
  writeLayer(OKTribeUrl, "inst/extdata/OKTribe.shp")
  writeLayer(VATribeUrl, "inst/extdata/VATribe.shp")
}
