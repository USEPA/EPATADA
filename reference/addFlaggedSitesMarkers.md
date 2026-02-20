# addFlaggedSitesMarkers Internal function to add flagged sites to map for review.

addFlaggedSitesMarkers Internal function to add flagged sites to map for
review.

## Usage

``` r
addFlaggedSitesMarkers(.data, map = NULL, flag_type = NULL)
```

## Arguments

- .data:

  A TADA data frame containing TADA.LatitudeMeasure and
  TADA.LongitudeMeasure for mapping.

- map:

  A leaflet map of TADA data to apply the symbology for flagged sites
  to.

- flag_type:

  Character argument. Flag types are "lowres" and "outsideusa".

## Value

A TADA leaflet map with flagged site markers added.
