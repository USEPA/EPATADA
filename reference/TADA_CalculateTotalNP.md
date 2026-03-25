# Calculate Total Nitrogen and Phosphorus

Apply nutrient aggregation logic from EPA's Enforcement and Compliance
History Online (ECHO) Water Pollutant Loading Tool to add nitrogen
subspecies and phosphorus forms together to approximate total nitrogen
(TN) and total phosphorus (TP) by site and day.

## Usage

``` r
TADA_CalculateTotalNP(.data, sum_ref, daily_agg = "max")
```

## Arguments

- .data:

  TADA dataframe.
  [`TADA_AutoClean()`](https://usepa.github.io/EPATADA/reference/TADA_AutoClean.md)
  should have been run and TADA.ResultMeasureValueDataTypes.Flag must be
  present. The function will run required flag functions if needed.
  Suspect/invalid combinations are excluded.

- sum_ref:

  Optional custom summation reference dataframe with the same columns as
  the internal reference; if omitted, the internal reference is used.

- daily_agg:

  Aggregation function used to collapse multiple measurements per
  day/site/metadata combination. One of "max", "min", or "mean".
  Defaults to "max".

## Value

The input dataframe plus additional rows representing TN and TP totals,
with explanatory flags: TADA.NutrientSummation.Flag,
TADA.NutrientSummationGroup, and TADA.NutrientSummationEquation.
Original rows not used in summations are preserved.

## Details

Before summing, the function aggregates measurements to a single daily
value per characteristic–unit–fraction–speciation–media combination
using the chosen `daily_agg`. Where needed, it converts nitrogen
subspecies to "AS N" and phosphorus forms to "AS P" using conversion
factors in the summation reference. The internal summation reference can
be customized and supplied via `sum_ref`.

- If required QA/QC flagging columns are absent, the function runs
  [`TADA_FindQCActivities()`](https://usepa.github.io/EPATADA/reference/TADA_FindQCActivities.md)
  (clean = FALSE),
  [`TADA_FlagResultUnit()`](https://usepa.github.io/EPATADA/reference/TADA_FlagResultUnit.md)
  (clean = "none"),
  [`TADA_FlagFraction()`](https://usepa.github.io/EPATADA/reference/TADA_FlagFraction.md)
  (clean = FALSE), and
  [`TADA_FlagSpeciation()`](https://usepa.github.io/EPATADA/reference/TADA_FlagSpeciation.md)
  (clean = "none"), and excludes invalid or suspect combinations from
  TN/TP summations.

- The function will not run a second time on the same data: if
  TADA.ResultMeasureValueDataTypes.Flag already contains "TN/TP
  estimated...", the input is returned unchanged.

- Daily aggregation uses `daily_agg` ("max", "min", or "mean") per
  site/day and metadata combination. Rows considered but not selected
  are preserved with explanatory flags.

- Keys are normalized (trim whitespace; `""` and `"NONE"` become `NA`)
  before matching to the summation reference; the join is NA-aware
  (dplyr \>= 1.1.0).

- Speciation conversions are applied where a conversion factor is
  provided.

- New rows for TN/TP totals are added with deterministic
  ResultIdentifier values based on site/date/group to ease testing and
  traceability.

## Note

Requires dplyr \>= 1.1.0 for NA-aware joins when matching to the
summation ref.

## See also

[`TADA_AggregateMeasurements()`](https://usepa.github.io/EPATADA/reference/TADA_AggregateMeasurements.md),
[`TADA_FlagResultUnit()`](https://usepa.github.io/EPATADA/reference/TADA_FlagResultUnit.md),
[`TADA_FlagFraction()`](https://usepa.github.io/EPATADA/reference/TADA_FlagFraction.md),
[`TADA_FlagSpeciation()`](https://usepa.github.io/EPATADA/reference/TADA_FlagSpeciation.md),
[`TADA_HarmonizeSynonyms()`](https://usepa.github.io/EPATADA/reference/TADA_HarmonizeSynonyms.md),
[`TADA_GetNutrientSummationRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetNutrientSummationRef.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- TADA_DataRetrieval(
  statecode = "UT",
  startDate = "2024-06-01",
  endDate = "2024-07-01",
  characteristicType = "Nutrient",
  applyautoclean = TRUE,
  ask = FALSE
)

df2 <- TADA_SimpleCensoredMethods(
  df, nd_method = "multiplier", nd_multiplier = 0.5,
  od_method = "as-is", od_multiplier = "null"
)

df2 <- TADA_RunKeyFlagFunctions(df2, clean = TRUE)
df2 <- TADA_HarmonizeSynonyms(df2)

out <- TADA_CalculateTotalNP(df2, daily_agg = "max")
} # }
```
