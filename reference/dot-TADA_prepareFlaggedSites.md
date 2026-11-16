# Prepare Flagged Sites Data for Mapping

Internal helper that prepares a TADA data frame for flagged-sites
mapping. It calls `TADA_FlagCoordinates(flaggedonly = TRUE)` to retain
only flagged sites and assigns a map color to each site based on its
suspect coordinates flag.

## Usage

``` r
.TADA_prepareFlaggedSites(.data)
```

## Arguments

- .data:

  A TADA data frame to prepare for flagged sites mapping.

## Value

A flagged-site TADA data frame with an added `FlaggedColor` column.
