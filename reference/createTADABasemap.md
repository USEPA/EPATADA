# createTADABasemap

Internal function to create base leaflet map for TADA mapping functions.

## Usage

``` r
createTADABasemap(.data)
```

## Arguments

- .data:

  A TADA data frame. Must contain the columns TADA.LatitudeMeasure and
  TADA.Longitude measure to set the extent of the map.

## Value

The basemap for TADA mapping functions.
