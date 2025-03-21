% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utilities.R
\name{TADA_addPoints}
\alias{TADA_addPoints}
\title{Add points from an ArcGIS feature layer to a leaflet map}
\usage{
TADA_addPoints(map, layerfilepath, layergroup, layername, bbox = NULL)
}
\arguments{
\item{map}{A leaflet map}

\item{layerfilepath}{Local path to the .shp file for the layer}

\item{layergroup}{Name of the layer group}

\item{layername}{Name of the layer}

\item{bbox}{A bounding box from the sf function st_bbox; used to filter the query results. Optional; defaults to NULL.}
}
\value{
The original map with polygon from the feature layer added to it.
}
\description{
Add points from an ArcGIS feature layer to a leaflet map
}
\examples{
\dontrun{
# Create a leaflet map
lmap <- leaflet::leaflet() \%>\%
  leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo") \%>\%
  leaflet::addMapPane("featurelayers", zIndex = 300)
# Add the Virginia Federally Recognized Tribes feature layer to the map
lmap <- TADA_addPoints(
  lmap, "extdata/VATribe.shp",
  "Tribes", "Virginia Federally Recognized Tribes"
)
lmap
}
}
