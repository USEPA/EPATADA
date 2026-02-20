# checkForATTAINSGeo

Check to see if any ATTAINS assessment unit geometry was return as a
result of TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk.
Will stop and return an error message if no ATTAINS assessment unit
geometry is present. For use in TADA leaflet mapping functions that rely
on ATTAINS geometry.

## Usage

``` r
checkForATTAINSGeo(
  points_layer = NULL,
  lines_layer = NULL,
  polygons_layer = NULL
)
```

## Arguments

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

## Value

The function print a message if there is no ATTAINS assessment unit
geometry.
