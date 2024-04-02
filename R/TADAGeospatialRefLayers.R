# Used to store cached tribal feature layers

#' Function to update tribal layer shapefiles.
#' Shapefiles are stored in inst/extdata/shapefiles.
#' Existing shapefiles with the same name will be deleted first if they exist. 

TADA_UpdateTribalLayers <- function() {
  writeLayer(AKAllotmentsUrl, "inst/extdata/shapefiles/AKAllotments.shp")
  writeLayer(AKVillagesUrl, "inst/extdata/shapefiles/AKVillages.shp")
  writeLayer(AmericanIndianUrl, "inst/extdata/shapefiles/AmericanIndian.shp")
  writeLayer(OffReservationUrl, "inst/extdata/shapefiles/OffReservation.shp")
  writeLayer(OKTribeUrl, "inst/extdata/shapefiles/OKTribe.shp")
  writeLayer(VATribeUrl, "inst/extdata/shapefiles/VATribe.shp")
}