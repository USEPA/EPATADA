# fetchNHD

Fetches NHD features from either the high resolution or medium
resolution version of the National Hydrography Dataset (NHD) that
intersect catchments containing TADA Water Quality Portal observations.

## Usage

``` r
fetchNHD(.data, resolution = "Hi", features = "catchments")
```

## Arguments

- .data:

  A dataframe created by
  [`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
  or the geospatial equivalent made by
  [`TADA_MakeSpatial()`](https://usepa.github.io/EPATADA/reference/TADA_MakeSpatial.md).

- resolution:

  Whether to download the NHDPlus HiRes resolution ("Hi") or medium
  NHDPlus V2 resolution ("Med") version of the National Hydrography
  Dataset (NHD). Default is "Hi".

- features:

  Which NHD features to return: "catchments", "flowlines",
  "waterbodies", or any combination.

## Value

A list containing all selected NHD features associated with the WQP
observations of interest. Or, if a single feature type is selected, a
single geospatial object instead of a list. Default is "catchments"
only.

## See also

[`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)

[`TADA_MakeSpatial()`](https://usepa.github.io/EPATADA/reference/TADA_MakeSpatial.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tada_data <- TADA_DataRetrieval(
  startDate = "1990-01-01",
  endDate = "1990-01-15",
  characteristicName = "pH",
  statecode = "CO",
  applyautoclean = TRUE,
  ask = FALSE
)

nhd_data <- fetchNHD(
  .data = tada_data, resolution = "Hi",
  features = c("catchments", "waterbodies", "flowlines")
)
} # }
```
