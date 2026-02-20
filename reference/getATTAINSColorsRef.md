# getATTAINSColorsRef

Internal function to return a data frames specifying the color the
feature should be displayed in for a leaflet map based on the value in
the "overallstatus" column in the ATTAINS_points, ATTAINS_polygons, or
ATTAINS_lines data frames created with TADA_CreateATTAINSAUMLCrosswalk
or TADA_CreateAUMLCrosswalk.

## Usage

``` r
getATTAINSColorsRef()
```

## Value

A data frame with the columns overallstatus, col, dark_col, and
priority.
