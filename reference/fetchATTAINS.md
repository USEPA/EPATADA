# fetchATTAINS

Fetches ATTAINS features (state- or tribe- or other entity- submitted
points, lines, and polygons representing their assessment units; and the
EPA snapshot of the associated NHDPlus HR catchments that the state- or
tribe- or other entity- submitted features fall within) within a
bounding box produced from a set of TADA spatial features.

## Usage

``` r
fetchATTAINS(.data, catchments_only = FALSE)
```

## Arguments

- .data:

  A dataframe developed using
  [`TADA_DataRetrieval()`](usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
  or
  [`TADA_MakeSpatial()`](usepa.github.io/EPATADA/reference/TADA_MakeSpatial.md).

- catchments_only:

  Whether to return just the summarized ATTAINS catchment features, or
  both the catchments and raw ATTAINS features. TRUE or FALSE.

## Value

Spatial features (ATTAINS_catchments, ATTAINS_points, ATTAINS_lines, and
ATTAINS_polygons) that are within the spatial bounding box of water
quality observations.

## See also

[`TADA_MakeSpatial()`](usepa.github.io/EPATADA/reference/TADA_MakeSpatial.md)

[`TADA_DataRetrieval()`](usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tada_data <- TADA_DataRetrieval(
  startDate = "1990-01-01",
  endDate = "1990-12-30",
  characteristicName = "pH",
  statecode = "NV",
  applyautoclean = TRUE,
  ask = FALSE
)

nv_attains_features <- EPATADA:::fetchATTAINS(tada_data, catchments_only = FALSE)
} # }
```
