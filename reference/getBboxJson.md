# Get bounding box JSON

Get bounding box JSON

## Usage

``` r
getBboxJson(bbox)
```

## Arguments

- bbox:

  A bounding box from the sf function st_bbox

## Value

A string containing bounding box JSON that can be passed to an ArcGIS
feature layer in the Input Geometry field

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataset
utils::data(Data_TribalNations)
# Get the bounding box of the data
bbox <- sf::st_bbox(
  c(
    xmin = min(Data_TribalNations$TADA.LongitudeMeasure),
    ymin = min(Data_TribalNations$TADA.LatitudeMeasure),
    xmax = max(Data_TribalNations$TADA.LongitudeMeasure),
    ymax = max(Data_TribalNations$TADA.LatitudeMeasure)
  ),
  crs = sf::st_crs(Data_TribalNations)
)
# Get a string containing the JSON of the bounding box
getBboxJson(bbox)
} # }
```
