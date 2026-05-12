# Get a shapefile from a local folder, optionally crop it by a bounding box, and return it as a sf object getLayer is used within TADA_addPolys and TADA_addPoints

Get a shapefile from a local folder, optionally crop it by a bounding
box, and return it as a sf object getLayer is used within TADA_addPolys
and TADA_addPoints

## Usage

``` r
getLayer(layerfilepath, bbox = NULL)
```

## Arguments

- layerfilepath:

  Local path to the .shp file for the layer

- bbox:

  A bounding box from the sf function st_bbox; used to filter the query
  results. Optional; defaults to NULL.

## Value

sf object containing the layer

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataset
utils::data(Data_TribalNations_Harmonized)
# Get the bounding box of the data
bbox <- sf::st_bbox(
  c(
    xmin = min(Data_TribalNations_Harmonized$TADA.LongitudeMeasure),
    ymin = min(Data_TribalNations_Harmonized$TADA.LatitudeMeasure),
    xmax = max(Data_TribalNations_Harmonized$TADA.LongitudeMeasure),
    ymax = max(Data_TribalNations_Harmonized$TADA.LatitudeMeasure)
  ),
  crs = sf::st_crs(Data_TribalNations_Harmonized)
)
# Get the American Indian Reservations feature layer,
# filtered by the bounding box for the Data_TribalNations_Harmonized
# example dataset
layerfilepath <- "extdata/AmericanIndian.shp"
getLayer(layerfilepath, bbox)
} # }
```
