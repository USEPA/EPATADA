# Create Toggleable Nearby and Flagged Sites Map

Create a leaflet map that allows users to toggle between two overlay
layers: nearby sites grouped by spatial proximity, and flagged sites
with suspect coordinates.

## Usage

``` r
TADA_SitesToggleMap(.data, dist_buffer = 100, catchment = TRUE, nhd_res = "Hi")
```

## Arguments

- .data:

  A TADA dataframe containing WQP-style columns used by both nearby-site
  and flagged-site mapping functions.

- dist_buffer:

  Numeric. Distance in meters used to define nearby site groupings.
  Default is `100`.

- catchment:

  Logical. If `TRUE`, nearby sites are grouped only when they are within
  `dist_buffer` and in the same catchment. If `FALSE`, only
  `dist_buffer` is used. Default is `TRUE`.

- nhd_res:

  Character. NHD resolution used when nearby-site grouping is performed
  with catchment constraints. Options are "Hi" or "Med". Passed to
  [`TADA_FindNearbySites()`](https://usepa.github.io/EPATADA/reference/TADA_FindNearbySites.md)
  through the internal helper
  [`.TADA_prepareNearbySites()`](https://usepa.github.io/EPATADA/reference/dot-TADA_prepareNearbySites.md).
  Default is `"Hi"`.

## Value

A leaflet map with two toggleable overlay layers:

- `Nearby sites`

- `Flagged sites`

## Details

This function combines the logic from nearby-site and flagged-site
mapping functions into a single interactive leaflet map. The layer
control is placed in the bottom-left corner of the map and allows users
to switch between the two overlays.

Nearby site groups are created using
[`TADA_FindNearbySites()`](https://usepa.github.io/EPATADA/reference/TADA_FindNearbySites.md)
through the internal helper
[`.TADA_prepareNearbySites()`](https://usepa.github.io/EPATADA/reference/dot-TADA_prepareNearbySites.md).
Flagged sites are created using
[`TADA_FlagCoordinates()`](https://usepa.github.io/EPATADA/reference/TADA_FlagCoordinates.md)
through the internal helper
[`.TADA_prepareFlaggedSites()`](https://usepa.github.io/EPATADA/reference/dot-TADA_prepareFlaggedSites.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataframe:
utils::data(Data_TribalNations_Harmonized)

# Create toggleable map:
TADA_SitesToggleMap(Data_TribalNations_Harmonized, catchment = FALSE)
} # }
```
