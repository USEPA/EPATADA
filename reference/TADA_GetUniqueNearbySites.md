# Get grouped monitoring stations that are near each other

This function takes a TADA dataset that contains grouped nearby
monitoring stations and returns a unique dataset of the original
MonitoringLocationIdentifier, the grouped
TADA.MonitoringLocationIdentifier, as well as the original and
TADA-prefixed LongitudeMeasure, LatitudeMeasure, MonitoringLocationName,
and MonitoringLocationTypeName, filtered for only those stations that
have a nearby station.

## Usage

``` r
TADA_GetUniqueNearbySites(.data)
```

## Arguments

- .data:

  TADA dataframe

## Value

New dataframe with unique combinations of original and TADA
MonitoringLocationIdentifier, LongitudeMeasure, LatitudeMeasure,
MonitoringLocationName, and MonitoringLocationTypeName.

## Examples

``` r
if (FALSE) { # \dontrun{
# use MT example data set
testdat <- Data_MT_MissoulaCounty

# find unique nearby sites
testdat.unique <- testdat |>
  TADA_FindNearbySites() |>
  TADA_GetUniqueNearbySites()
} # }
```
