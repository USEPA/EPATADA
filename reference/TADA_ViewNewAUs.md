# Create Map for New Point AU Review

Create a leaflet map showing newly created point assessment units and
their monitoring locations.

## Usage

``` r
TADA_ViewNewAUs(.data)
```

## Arguments

- .data:

  The `tada_df` output of
  [`TADA_CreatePointAUs()`](https://usepa.github.io/EPATADA/reference/TADA_CreatePointAUs.md).

## Value

A leaflet map that shows all newly created point AUs with popups for the
monitoring location and new point assessment unit.

## Examples

``` r
if (FALSE) { # \dontrun{
testdat <- Data_TribalNations_Harmonized |>
  dplyr::filter(OrganizationIdentifier == "BLCKFEET") |>
  dplyr::slice_sample(n = 25)

testdat_newAUs <- TADA_CreatePointAUs(
  testdat,
  create_geo = TRUE,
  tada_df = TRUE
)

TADA_ViewNewAUs(testdat_newAUs$tada_df)
} # }
```
