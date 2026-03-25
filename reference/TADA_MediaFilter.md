# TADA_MediaFilter

Process a TADA dataframe to flag or filter data by media type.

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

  Logical. If `TRUE`, remove rows according to the media toggles and
  return data without flag columns. If `FALSE`, only flag media and
  return all rows with `TADA.Media.Flag`. Default `FALSE`.

- surface_water:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove
  `SURFACE WATER` results. Default `FALSE`.

- ground_water:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove
  `GROUNDWATER` results. Default `FALSE`.

- sediment:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove `SEDIMENT`
  results. Default `FALSE`.

- other:

  Logical (used only when `clean = TRUE`). If `TRUE`, remove `OTHER`
  results. Default `FALSE`.

## Value

A data frame.

- If `clean = FALSE`, returns all rows with the column `TADA.Media.Flag`
  added.

- If `clean = TRUE`, returns rows with the selected media removed and no
  flag columns added.

- If the input has 0 rows, returns `NULL`.

## Details

Behavior overview:

- If `clean = FALSE`, classify each row and add `TADA.Media.Flag`. The
  function prints a single message with counts by media and returns the
  original data with the flag.

- If `clean = TRUE`, classify and then remove rows whose media types are
  selected via function toggles. The function prints a single message
  with counts by media before filtering, and then a single summary
  message:

  - When no toggles are selected (all FALSE): informs the user and
    returns the original data without `TADA.Media.Flag`.

  - When some toggles are selected but no rows match (e.g., data already
    contain only SURFACE WATER): informs the user that nothing was
    removed and returns the data unchanged (without flag columns).

  - When rows are removed: reports how many rows were removed and
    returns cleaned data (without `TADA.Media.Flag`). Additionally, a
    warning is issued if all media toggles are `TRUE` (which would
    remove all media types), and if the filter removes all rows.

Inputs used for classification:

- `MonitoringLocationTypeName`, `ActivityMediaSubdivisionName` and
  `ActivityMediaName`

- Groundwater-related fields: `AquiferName`, `AquiferTypeName`,
  `LocalAqfrName`, `ConstructionDateText`,
  `WellDepthMeasure.MeasureValue`, `WellDepthMeasure.MeasureUnitCode`,
  `WellHoleDepthMeasure.MeasureValue`,
  `WellHoleDepthMeasure.MeasureUnitCode`

Classification details:

- ActivityMediaName of "Soil", "Sediment" (and common variants like
  "Soil or Sediment") map to `SEDIMENT` even if groundwater fields are
  present.

- `ActivityMediaSubdivisionName` is reviewed for identifying "Surface
  Water", "Groundwater", and "Sediment".

- If the subdivision is blank and `ActivityMediaName` is "Water" with no
  groundwater fields present, the row is classified as `SURFACE WATER`.

- Groundwater fields are considered present if they are non-NA and
  non-blank (for character/factor fields) or non-NA (for numeric
  fields).

- If a monitoring location type reference is available via
  [`TADA_GetMonLocTypeRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetMonLocTypeRef.md),
  its `TADA.Media.Flag` takes precedence when present.

- Media flags are normalized to the core set: `SURFACE WATER`,
  `GROUNDWATER`, `SEDIMENT`, `OTHER`. Values such as `HABITAT`, `AIR`,
  `BIOLOGICAL`, empty strings, or non-core values are coerced to
  `OTHER`.

Requirements and defaults:

- Required columns: `MonitoringLocationTypeName`,
  `ActivityMediaSubdivisionName`, and `AquiferName`. If any required
  columns are missing (rare), the function stops with an error of the
  form "Missing required columns: , , ...".

- Optional columns are created if missing (with appropriate types) and
  filled with `NA`: `ActivityMediaName`, `AquiferTypeName`,
  `LocalAqfrName`, `ConstructionDateText`,
  `WellDepthMeasure.MeasureValue` (numeric),
  `WellDepthMeasure.MeasureUnitCode`,
  `WellHoleDepthMeasure.MeasureValue` (numeric),
  `WellHoleDepthMeasure.MeasureUnitCode`.

- If the input data frame has 0 rows, the function emits a message and
  returns `NULL`.

## Messages and warnings

- Always prints counts by media before filtering.

- When `clean = TRUE`, prints a single summary message indicating
  whether rows were removed, whether no rows matched the selected media,
  or whether no toggles were selected.

- Warns if all media toggles are `TRUE`, and if all rows were removed by
  the filter.

## See also

[`TADA_GetMonLocTypeRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetMonLocTypeRef.md),
[`TADA_OrderCols()`](https://usepa.github.io/EPATADA/reference/TADA_OrderCols.md)

## Examples

``` r
utils::data(Data_R5_TADAPackageDemo)

# Example 1: Do not clean; classify media and add TADA.Media.Flag
Data_Flag <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = FALSE
)
#> TADA_MediaFilter: Counts by media - SURFACE WATER: 166,373 | GROUNDWATER:   3,315 | SEDIMENT:     245 | OTHER:   2,244
#> TADA_MediaFilter: Returning all results with TADA.Media.Flag; media toggles ignored because clean = FALSE.
unique(Data_Flag$TADA.Media.Flag)
#> [1] "SURFACE WATER" "GROUNDWATER"   "SEDIMENT"      "OTHER"        

# Example 2: Remove groundwater and sediment; no flag column in output
Data_Clean1 <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = TRUE,
  ground_water = TRUE,
  sediment = TRUE
)
#> TADA_MediaFilter: Counts by media (before filter) - SURFACE WATER: 166,373 | GROUNDWATER:   3,315 | SEDIMENT:     245 | OTHER:   2,244
#> TADA_MediaFilter: Removed 3,560 rows matching media types: GROUNDWATER, SEDIMENT. Returning cleaned data without flag columns.
"TADA.Media.Flag" %in% names(Data_Clean1) # FALSE
#> [1] FALSE

# Example 3: Keep only surface water by removing groundwater, sediment, and other
Data_Clean2 <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = TRUE,
  ground_water = TRUE,
  sediment = TRUE,
  other = TRUE
)
#> TADA_MediaFilter: Counts by media (before filter) - SURFACE WATER: 166,373 | GROUNDWATER:   3,315 | SEDIMENT:     245 | OTHER:   2,244
#> TADA_MediaFilter: Removed 5,804 rows matching media types: GROUNDWATER, SEDIMENT, OTHER. Returning cleaned data without flag columns.

# Example 4: Remove surface water only
Data_Clean3 <- TADA_MediaFilter(
  Data_R5_TADAPackageDemo,
  clean = TRUE,
  surface_water = TRUE
)
#> TADA_MediaFilter: Counts by media (before filter) - SURFACE WATER: 166,373 | GROUNDWATER:   3,315 | SEDIMENT:     245 | OTHER:   2,244
#> TADA_MediaFilter: Removed 166,373 rows matching media types: SURFACE WATER. Returning cleaned data without flag columns.
```
