# findATTAINSMissingRawFeatures

Check ATTAINS_catchment data to identify assessment unit data missing
from ATTAINS assessment units points, polygons, and lines layers that is
still preserved in the catchment layer.

## Usage

``` r
findATTAINSMissingRawFeatures(
  .data,
  points_layer = NULL,
  lines_layer = NULL,
  polygons_layer = NULL,
  auid_list = NULL
)
```

## Arguments

- .data:

  The ATTAINS_catchments data frame created as a result of
  TADA_CreateAUMLCrosswalk or TADA_CreateATTAINSAUMLCrosswalk.

- points_layer:

  Optional data frame argument. Contains the data required to map
  ATTAINS assessment unit point geometry. When points_layer = NULL, the
  point assessment units data are not used when searching for missing
  raw features. Default = NULL.

- lines_layer:

  Optional data frame argument. Contains the data required to map
  ATTAINS assessment unit line geometry. When lines_layer = NULL, the
  line assessment units data are not used when searching for missing raw
  features. Default = NULL.

- polygons_layer:

  Optional data frame argument. Contains the data required to map
  ATTAINS assessment unit polygon geometry. When polygons_layer = NULL,
  the polygon assessment units data are not used when searching for
  missing raw features. Default = NULL.

- auid_list:

  A list of any ATTAINS assessment unit identifiers that should be
  excluded from the output.

## Value

A data frame of assessment data that is missing from assessment units
points, lines, and polygons layers but still preserved in the catchment
layer.
