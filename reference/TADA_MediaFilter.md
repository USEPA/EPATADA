# TADA_MediaFilter

Process a TADA profile object to flag or filter data based on media
type.

- If `clean = FALSE`, adds `TADA.Media.Flag` indicating the classified
  media for each row.

- If `clean = TRUE`, removes rows where the media type is set to `TRUE`
  via function arguments, and does not add `TADA.Media.Flag` to the
  output. A warning is issued if all media toggles are `TRUE` (which
  would remove all media types), and if the filter removes all rows.

## Usage

``` r
TADA_MediaFilter(
  .data,
  clean = FALSE,
  surface_water = FALSE,
  ground_water = FALSE,
  sediment = FALSE,
  other = FALSE
)
```

## Arguments

- .data:

  A data frame representing a TADA profile object.

- clean:

  Logical. If `TRUE`, remove rows according to the media toggles. If
  `FALSE`, only flag media. Default `FALSE`.

- surface_water:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove SURFACE
  WATER results. Default `FALSE`.

- ground_water:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove GROUNDWATER
  results. Default `FALSE`.

- sediment:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove SEDIMENT
  results. Default `FALSE`.

- other:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove OTHER
  results. Default `FALSE`.

## Value

A data frame.

- If `clean = FALSE`, returns all rows with `TADA.Media.Flag` added.

- If `clean = TRUE`, returns rows with selected media removed and no
  flag columns added.

## Details

The function utilizes various columns including
`MonitoringLocationTypeName`, `ActivityMediaName`,
`ActivityMediaSubdivisionName`, `AquiferName`, `LocalAqfrName`,
`ConstructionDateText`, `WellDepthMeasure.MeasureValue`,
`WellDepthMeasure.MeasureUnitCode`, `WellHoleDepthMeasure.MeasureValue`,
and `WellHoleDepthMeasure.MeasureUnitCode`, and others to determine the
media type. Users can specify which media types (surface water,
groundwater, sediment, other) should be included or excluded.

Media classification uses `MonitoringLocationTypeName` (joined to the
reference table's `Name`) along with `ActivityMediaSubdivisionName`,
`ActivityMediaName`, and groundwater-related fields. Certain media
values are normalized to `OTHER` (HABITAT, empty string, AIR,
BIOLOGICAL, and any non-core value).

## Examples

``` r
utils::data(Data_R5_TADAPackageDemo)

# Example 1: Do not clean; just classify media and add TADA.Media.Flag
Data_Flag <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = FALSE
)
#> TADA_MediaFilter: Returning all results with TADA.Media.Flag; media toggles ignored because clean = FALSE.
unique(Data_Flag$TADA.Media.Flag)
#> [1] "SURFACE WATER" "GROUNDWATER"   "SEDIMENT"      "OTHER"        

# Example 2: Clean the data by removing groundwater and sediment; no flag column returned
Data_Clean1 <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = TRUE,
  ground_water = TRUE,
  sediment = TRUE
)
#> TADA_MediaFilter: Removed media types: GROUNDWATER, SEDIMENT. Returning cleaned data without flag columns.
"TADA.Media.Flag" %in% names(Data_Clean1) # should be FALSE
#> [1] FALSE

# Example 3: Keep only surface water by removing groundwater, sediment, and other
Data_Clean2 <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = TRUE,
  ground_water = TRUE,
  sediment = TRUE,
  other = TRUE
)
#> TADA_MediaFilter: Removed media types: GROUNDWATER, SEDIMENT, OTHER. Returning cleaned data without flag columns.

# Example 4: Remove surface water only
Data_Clean3 <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = TRUE,
  surface_water = TRUE
)
#> TADA_MediaFilter: Removed media types: SURFACE WATER. Returning cleaned data without flag columns.
```
