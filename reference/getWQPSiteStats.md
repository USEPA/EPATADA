# getWQPSiteStats

Internal function to prepare site data for use in leaflet map popup.

## Usage

``` r
getWQPSiteStats(.data, attains = TRUE)
```

## Arguments

- .data:

  TADA data frame. This function can incorporate ATTAINS data in the
  TADA data frame (for example, using the TADA_with_ATTAINS df created
  with TADA_CreateATTAINSAUMLCrosswalk, TADA_CreateAUMLCrosswalk).

- attains:

  Boolean argument. If attains = TRUE and ATTAINS prefixed columns are
  included in .data, ATTAINS Assessment Units will be listed in the
  popup. If attains = FALSE (or there are no ATTAINS prefixed columns in
  .data), no assessment unit data is included in popup. Default is
  attains = TRUE.

## Value

A data frame formatted correctly for use in a leaflet pop up containing
WQP site data.
