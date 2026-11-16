# Prepare Nearby Sites Data for Mapping

Internal helper that prepares a TADA data frame for nearby-sites
mapping. If `TADA.NearbySiteGroup` is not already present, it calls
[`TADA_FindNearbySites()`](https://usepa.github.io/EPATADA/reference/TADA_FindNearbySites.md)
to generate nearby site groups. It then filters to grouped nearby sites,
converts coordinate columns to numeric, selects the columns needed for
mapping, and removes duplicate rows.

## Usage

``` r
.TADA_prepareNearbySites(
  .data,
  catchment = TRUE,
  dist_buffer = 100,
  nhd_res = "Hi"
)
```

## Arguments

- .data:

  A TADA data frame to prepare for nearby sites mapping.

- catchment:

  Logical. Passed to
  [`TADA_FindNearbySites()`](https://usepa.github.io/EPATADA/reference/TADA_FindNearbySites.md)
  to determine whether nearby sites must also be in the same catchment.
  Default is `TRUE`.

- dist_buffer:

  Numeric. Distance buffer in meters used to identify nearby sites if
  `TADA.FindNearbySites()` has not already been run. Default is `100`.

- nhd_res:

  Character. NHD resolution passed to
  [`TADA_FindNearbySites()`](https://usepa.github.io/EPATADA/reference/TADA_FindNearbySites.md)
  when catchment-based nearby site grouping is requested. Default is
  `"Hi"`.

## Value

A filtered TADA data frame containing only grouped nearby sites and
columns required for nearby site mapping.
