# Add Flagged Sites Layer to Leaflet Map

Internal helper that adds the flagged sites layer to an existing leaflet
map. Points are colored according to their suspect coordinates flag.

## Usage

``` r
.TADA_addFlaggedLayer(map, invalid)
```

## Arguments

- map:

  A leaflet map object.

- invalid:

  A data frame of prepared flagged sites.

## Value

A leaflet map with the flagged sites layer added.
