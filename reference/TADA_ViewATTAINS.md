# TADA_ViewATTAINS

This function is designed to visualize the data included in the list
returned from TADA_CreateAUMLCrosswalk. The map can be used to review
different crosswalk sources used for the assignment of WQP Monitoring
Locations to ATTAINS Assessment Units. Please check out the
TADAModule2.Rmd for an example workflow.

## Usage

``` r
TADA_ViewATTAINS(.data, ref_icons = TRUE)
```

## Arguments

- .data:

  [`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
  and
  [`TADA_CreateAUMLCrosswalk()`](https://usepa.github.io/EPATADA/reference/TADA_CreateAUMLCrosswalk.md)
  can be run to get a list containing WQP monitoring locations and
  ATTAINS shapefile objects.

- ref_icons:

  Boolean argument. Determines whether custom icons are displayed to
  differentiate between different crosswalk sources for the assignment
  of WQP Monitoring Locations to Assessment Units if this information is
  included in the TADA_with_ATTAINS dataframe supplied to the function.
  When ref_icons = TRUE three different icons will be used for the map.

  1.  The circle with the user icon is for matches from the user
      supplied ref if that was supplied as an input to
      TADA_CreateAUMLCrosswalk().

  2.  The circle with a check mark is for matches from
      [`TADA_GetATTAINSAUMLCrosswalk()`](https://usepa.github.io/EPATADA/reference/TADA_GetATTAINSAUMLCrosswalk.md)
      which runs within TADA_CreateAUMLCrosswalk(). If an organization
      has recorded this information in ATTAINS, this gets the
      organizations crosswalk of known monitoring location identifiers
      and assessment unit associations.

  3.  The plain circle represents matches made with
      [`TADA_CreateATTAINSAUMLCrosswalk()`](https://usepa.github.io/EPATADA/reference/TADA_CreateATTAINSAUMLCrosswalk.md)
      which also runs within TADA_CreateAUMLCrosswalk() to link
      catchment-based ATTAINS assessment unit data to Water Quality
      Portal observations. When rec_icons = FALSE or the source is not
      provided in .data, all Monitoring Locations are show with a plain
      circle.

## Value

A leaflet map visualizing Monitoring Locations and linked ATTAINS
assessment units. All maps are in WGS84.

## See also

[`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
must be run first to get WQP monitoring locations and results.

[`TADA_CreateAUMLCrosswalk()`](https://usepa.github.io/EPATADA/reference/TADA_CreateAUMLCrosswalk.md)
which runs
[`TADA_CreateATTAINSAUMLCrosswalk()`](https://usepa.github.io/EPATADA/reference/TADA_CreateATTAINSAUMLCrosswalk.md)
with return_sf argument set to TRUE and
[`TADA_GetATTAINSAUMLCrosswalk()`](https://usepa.github.io/EPATADA/reference/TADA_GetATTAINSAUMLCrosswalk.md)
by default.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get WQP Monitoring Locations
tada_data <- TADA_DataRetrieval(
  startDate = "1990-01-01",
  endDate = "1995-12-31",
  characteristicName = "pH",
  statecode = "NV",
  applyautoclean = TRUE,
  ask = FALSE
)

# Match AUs using all available methods
all_sources <- TADA_CreateAUMLCrosswalk(tada_data, org_id = "21NEV1")

TADA_ViewATTAINS(all_sources)

# Only use ATTAINS catchments to match AUs
attains_catchments <- TADA_CreateATTAINSAUMLCrosswalk(tada_data,
  return_nearest = TRUE, resolution = "hi", return_sf = TRUE
)

TADA_ViewATTAINS(attains_catchments)
} # }
```
