# Create Flagged Sites Map

Create Flagged Sites Map

## Usage

``` r
TADA_FlaggedSitesMap(.data)
```

## Arguments

- .data:

  TADA dataframe containing the data downloaded from the WQP, where each
  row represents a unique data record. Dataframe must include the
  columns
  'MonitoringLocationIdentifier','MonitoringLocationName','TADA.LatitudeMeasure',
  and 'TADA.LongitudeMeasure' to run this function.

## Value

A leaflet map that shows all sites in the dataframe that contain flagged
data in the form of:

1.  imprecise coordinates - latitudes and/or longitudes that contain
    fewer then 3 decimal places.

2.  outside USA - coordinates that fall outside the bounds of the USA.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataframe:
utils::data(Data_Nutrients_UT)
utils::data(Data_TribalNations_Harmonized)

# Create maps:
TADA_FlaggedSitesMap(Data_Nutrients_UT)
TADA_FlaggedSitesMap(Data_TribalNations_Harmonized)
} # }
```
