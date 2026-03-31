# Create Nearby Sites Map

Create Nearby Sites Map

## Usage

``` r
TADA_NearbySitesMap(
  .data,
  dist_buffer = 100,
  attains = TRUE,
  catchment = FALSE
)
```

## Arguments

- .data:

  Either(1) a TADA dataframe or (2) the list of data frames created by
  TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk. If
  TADA_FindNearbySites has not been previously run, it will be as part
  of this function. In order for ATTAINS assessment units to be
  displayed on the nearby sites map, .data must be the list of data
  frames created by TADA_CreateATTAINSAUMLCrosswalk or
  TADA_CreateAUMLCrosswalk.

- dist_buffer:

  Distance in m to show a radius around each site marker.

- attains:

  Boolean. If attains = TRUE and assessment unit geometry is available
  in the list of data frames created by TADA_CreateATTAINSAUMLCrosswalk
  or TADA_CreateAUMLCrosswalk, assessment units will be added to the
  review map. If attains = FALSE, no assessment units will be shown.
  Default is attains = TRUE.

- catchment:

  Boolean. If catchment = TRUE, any catchment data available in .data
  are added to the review map. If catchment = FALSE, catchments are not
  added to the review map. Default is catchment = FALSE.

## Value

A leaflet map that shows all sites in the dataframe that contain flagged
data in the form of near other sites - groups of sites that are
spatially located within a threshold distance (defaulting to 100 m) from
each other and within the same catchment.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataframe:
utils::data(Data_Nutrients_UT)


# Create maps:
TADA_NearbySitesMap(Data_Nutrients_UT)
TADA_NearbySitesMap(Data_6Tribes_5y_Harmonized)
} # }
```
