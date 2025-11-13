# Download a shapefile from an API and save it to a local folder, overwriting existing file if it exists writeLayer is used by TADA_UpdateTribalLayers in TADAGeospatialRefLayers.R.

Download a shapefile from an API and save it to a local folder,
overwriting existing file if it exists writeLayer is used by
TADA_UpdateTribalLayers in TADAGeospatialRefLayers.R.

## Usage

``` r
writeLayer(url, layerfilepath)
```

## Arguments

- url:

  URL of the layer REST service, ending with "/query". Example:
  https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query
  (American Indian Reservations)

- layerfilepath:

  Local path to save the .shp file to

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the Oklahoma Tribal Statistical Areas feature layer and write
# local file to inst/extdata/OKTribe.shp
OKTribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query"
writeLayer(OKTribeUrl, "inst/extdata/OKTribe.shp")
} # }
```
