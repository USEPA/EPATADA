# Add polygons from a spatial layer to a leaflet map

Add polygons from a spatial layer to a leaflet map

## Usage

``` r
TADA_addPolys(map, gpkg, layer, layergroup, layername, bbox = NULL)
```

## Arguments

- map:

  A leaflet map

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

The original map with polygons from the feature layer added to it.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a leaflet map
lmap <- leaflet::leaflet() |>
  leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo") |>
  leaflet::addMapPane("featurelayers", zIndex = 300)
# Add the American Indian Reservations feature layer to the map
tribal_gpkg <- "inst/extdata/Tribal.gpkg"
lmap <- TADA_addPolys(lmap, tribal_gpkg,"AmericanIndian",
"Tribes", "American Indian Reservations")
lmap
} # }
```
