# TADA_MakeSpatial

Transforms a Water Quality Portal dataframe into a geospatial `sf`
object.

## Usage

``` r
TADA_MakeSpatial(.data, crs = 4326)
```

## Arguments

- .data:

  A dataframe that has been processed using
  [`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
  and
  [`TADA_AutoClean()`](https://usepa.github.io/EPATADA/reference/TADA_AutoClean.md).

- crs:

  The coordinate reference system (CRS) for the returned point features.
  The default is CRS 4326 (WGS84).

## Value

An `sf` object, which is the original TADA Water Quality Portal
dataframe transformed into geospatial point objects.

## Details

This function adds a new column, 'geometry', to the input dataframe,
enabling mapping and additional geospatial capabilities. For an example
workflow, refer to the TADAModule2.Rmd file.

## See also

[`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve water quality data
tada_not_spatial <- TADA_DataRetrieval(
  characteristicName = "pH",
  statecode = "SC",
  countycode = "Abbeville",
  applyautoclean = TRUE,
  ask = FALSE
)

# Convert `tada_not_spatial` into an `sf` object, projected in CRS 4269 (NAD83)
tada_spatial <- TADA_MakeSpatial(tada_not_spatial, crs = 4269)
} # }
```
