# Add Nearby Sites Layer to Leaflet Map

Internal helper that adds the nearby sites layer to an existing leaflet
map. It draws nearby site markers and radius circles using a group-based
color palette.

## Usage

``` r
.TADA_addNearbyLayer(map, nearby_data, dist_buffer = 100)
```

## Arguments

- map:

  A leaflet map object.

- nearby_data:

  A data frame of prepared nearby sites.

- dist_buffer:

  Numeric. Radius in meters used for the nearby-site circles. Default is
  `100`.

## Value

A leaflet map with the nearby sites layer added.
