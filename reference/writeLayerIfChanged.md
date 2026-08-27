# Download a spatial file from an API and save it to a local folder, overwriting existing file if it exists and has changed. writeLayerIfChanged is used by TADA_UpdateTribalLayers in TADAGeospatialRefLayers.R.

Download a spatial file from an API and save it to a local folder,
overwriting existing file if it exists and has changed.
writeLayerIfChanged is used by TADA_UpdateTribalLayers in
TADAGeospatialRefLayers.R.

## Usage

``` r
writeLayerIfChanged(url, layerfilepath, layername)
```

## Arguments

- url:

  URL of the layer REST service, ending with "/query". Example:
  https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query
  (American Indian Reservations)

- layerfilepath:

  Local path to save the .gpkg file

- layername:

  Name of the layer within the .gpkg file

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the Oklahoma Tribal Statistical Areas feature layer and write
# local file to inst/extdata/Tribal.gpkg/OKTribe
OKTribeUrl <- "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query"
tribal_gpkg <- "inst/extdata/Tribal.gpkg"
writeLayerIfChanged(OKTribeUrl, tribal_gpkg,"OKTribe")
} # }
```
