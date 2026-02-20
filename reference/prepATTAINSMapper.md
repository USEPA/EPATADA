# prepATTAINSMapper

Internal function to prepare ATTAINS geometry from
TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk for display
in a leaflet map.

## Usage

``` r
prepATTAINSMapper(.data, geo_type = NULL, color_ref = NULL, auid_list = NULL)
```

## Arguments

- .data:

  Data frame. One of the data frames containing geometry from the output
  of TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk
  (ATTAINS_catchments, ATTAINS_polygons, ATTAINS_points, or
  ATTAINS_lines).

- geo_type:

  Character string. Type of geometry to be prepared for mapping.
  Allowable values are "points", "lines", and "polygons". If no geo_type
  (geo_type = NULL) is supplied the function will attempt to determine
  the type from the "geometry" column. If geo_type is not supplied and
  cannot be determined, the function will stop with an error.

- color_ref:

  Data frame. A data frame containing the colors that should be applied
  to a feature based on its overallstatus in ATTAINS. Must contain the
  columns overallstatus, col, dark_col, and priority. Can be created
  with the internal function getATTAINSColorsRef. If color_ref = NULL,
  the function will run getATTAINSColorsRef to create a color ref data
  frame.

- auid_list:

  Character string. List of assessment unit identifiers to filter the
  data frame before returning. When a list is provided, only assessment
  unit identifiers included in the list will be shown on the map. When
  auid_list = NULL all assessment units in the source data set are show
  on the map. Default = NULL.

## Value

A data frame with the columns overallstatus, col, dark_col, and
priority.
