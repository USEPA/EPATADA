# createBBox

Internal function to create bounding box for maps based on
TADA.LatitudeMeasure and TADA.LongitudeMeasure columns from a TADA df or
ATTAINS geometry.

## Usage

``` r
createBBox(.data, as_vector = TRUE, attains_geo = FALSE)
```

## Arguments

- .data:

  Data frame. Must contain the columns TADA.LatitudeMeasure and
  TADA.Longitude measure to set the extent of the map.

- as_vector:

  Boolean argument. When as_vector = TRUE, the bounding box values are
  returned as vector. When as_vector = FALSE, values are returned as a
  bounding box. Default is as_vector = TRUE.

- attains_geo:

  Boolean argument. When attains_geo = TRUE, the function must reference
  the geometry column of .data. When attains_geo = FALSE, the function
  references the TADA.LatitudeMeasure and TADA.LongitudeMeasure to find
  the bounding box. Default is attains_geo = FALSE.

## Value

A bounding box for use in leaflet mapping functions.
