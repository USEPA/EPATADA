# Add points from a spatial layer to a leaflet map

Add points from a spatial layer to a leaflet map

## Usage

``` r
TADA_addPoints(
  map,
  layerfilepath,
  gpkg,
  layer,
  layergroup,
  layername,
  bbox = NULL
)
```

## Arguments

- map:

  A leaflet map

- layerfilepath:

  Local path to the data folder containing the .gpkg file

- gpkg:

  name of the .gpkg file

- layer:

  name of the layer within the .gpkg file

- layergroup:

  Name of the layer group

- layername:

  Name of the layer

- bbox:

  A bounding box from the sf function st_bbox; used to filter the query
  results. Optional; defaults to NULL.

## Value

The original map with polygon from the feature layer added to it.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a leaflet map
lmap <- leaflet::leaflet() |>
  leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo") |>
  leaflet::addMapPane("featurelayers", zIndex = 300)
# Add the Virginia Federally Recognized Tribes feature layer to the map
lmap <- TADA_addPoints(lmap, "extdata", "Tribal.gpkg","VATribe",
    "Tribes", "Virginia Federally Recognized Tribes")
lmap
} # }
```
