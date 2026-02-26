# Calculate Total Nitrogen and Phosphorus

This function applies the [Nutrient Aggregation
logic](https://echo.epa.gov/trends/loading-tool/resources/nutrient-aggregation#nitrogen)
from ECHO's Water Pollutant Loading Tool to add nitrogen subspecies
together to approximate a total nitrogen value on a single day at a
single site. Additional rows are added with the total nitrogen and total
phosphorous estimations. Before summing subspecies, this function runs
TADA_AggregateMeasurements to obtain the max value of a
characteristic-fraction-speciation-unit-media combination at a given
site and date. Where necessary, it uses conversion factors to convert
nitrogen subspecies expressed as nitrate, nitrite, ammonia, ammonium,
etc. to as nitrogen based on the atomic weights of the different
elements in the compound. Similarly, phosphate and other phosphorus
forms are converted to AS P where applicable. The reference table is
contained within the package but may be edited/customized by users.

## Usage

``` r
TADA_CalculateTotalNP(.data, sum_ref, daily_agg = "max")
```

## Arguments

- .data:

  TADA dataframe. TADA_AutoClean() must have been run (and the
  TADA.ResultMeasureValueDataTypes.Flag added) before this TM/TP
  summation function can be used. This function runs required flag
  functions (TADA_FindQCActivities, TADA_FlagResultUnit,
  TADA_FlagFraction, TADA_FlagSpeciation) if they have not yet been run.
  Suspect and invalid combinations are not included in TN and TP
  summation. While not required for this function to run, ideally, users
  should also run TADA_SimpleCensoredMethods and TADA_HarmonizeSynonyms
  before this function. If user wants to consider grouping N or P
  subspecies across multiple organizations, user should have also run
  TADA_FindNearbySites and grouped all nearby sites to one common
  TADA.MonitoringLocationIdentifier, TADA.LatitudeMeasure,
  TADA.LongitudeMeasure, etc.

- sum_ref:

  Optional. A custom summation reference dataframe the user has loaded
  into the R environment. Dataframe must have same columns as default
  TADA.summation reference table.

- daily_agg:

  If there are multiple measurements for the same
  characteristic-unit-fraction-speciation-media combination at the same
  location (TADA.MonitoringLocationIdentifier) on the same day
  (ActivityStartDate), this will select a single measurement to use in
  the Total N or Total P summation. Defaults to 'max', but can be set to
  'min' or 'mean'.

## Value

TADA dataframe with additional rows representing total N and P summation
values from adding up subspecies. Note that for total phosphorus, these
additional rows are simply a re-classification of phosphorus or
phosphate into the total phosphorus as P format. These new rows share
the same date and monitoring location as the subspecies, but an
additional note is added in the TADA.NutrientSummation.Flag column
describing how the total was derived. Also adds
TADA.NutrientSummationGroup and TADA.NutrientSummationEquation columns,
which can be used to trace how the total was calculated and from which
subspecies.

## Details

Nutrient equations are as follows:

NITROGEN:

1.  TOTAL N (UNFILTERED)

2.  TOTAL N (FILTERED) + TOTAL N (PARTICULATE)

3.  TOTAL KJELDAHL NITROGEN + NITRATE + NITRITE

4.  ORGANIC N + AMMONIA + NITRATE + NITRITE

5.  OTHER NITROGEN FORMS

PHOSPHORUS:

1.  TOTAL PHOSPHORUS

2.  PHOSPHATE

3.  OTHER PHOSPHORUS FORMS

Equations are applied in the order above. The function looks for groups
of nutrients that exactly match each equation before looking for every
combination within each equation (for example, a group of nitrogen
subspecies including AMMONIA and NITRATE will be passed over in an
initial sweep of groups of subspecies containing ORGANIC N, AMMONIA,
NITRATE, and NITRITE, but will be caught as the function moves down the
hierarchy of equations to fewer and fewer subspecies). Eventually, even
groups with only one subspecies will be used to represent a TOTAL N
value for that site/day/depth.

## See also

[`TADA_AggregateMeasurements()`](https://usepa.github.io/EPATADA/reference/TADA_AggregateMeasurements.md)

[`TADA_FlagDepthCategory()`](https://usepa.github.io/EPATADA/reference/TADA_FlagDepthCategory.md)

[`TADA_SimpleCensoredMethods()`](https://usepa.github.io/EPATADA/reference/TADA_SimpleCensoredMethods.md)

[`TADA_HarmonizeSynonyms()`](https://usepa.github.io/EPATADA/reference/TADA_HarmonizeSynonyms.md)

[`TADA_FindNearbySites()`](https://usepa.github.io/EPATADA/reference/TADA_FindNearbySites.md)

[`TADA_FindQCActivities()`](https://usepa.github.io/EPATADA/reference/TADA_FindQCActivities.md)

[`TADA_FlagResultUnit()`](https://usepa.github.io/EPATADA/reference/TADA_FlagResultUnit.md)

[`TADA_FlagFraction()`](https://usepa.github.io/EPATADA/reference/TADA_FlagFraction.md)

[`TADA_FlagSpeciation()`](https://usepa.github.io/EPATADA/reference/TADA_FlagSpeciation.md)

[`TADA_AutoClean()`](https://usepa.github.io/EPATADA/reference/TADA_AutoClean.md)

## Examples

``` r
df <- TADA_DataRetrieval(
  statecode = "UT", startDate = "2024-06-01",
  endDate = "2024-07-01", characteristicType = "Nutrient", applyautoclean = TRUE,
  ask = FALSE
)
#> Checking what data is available. This may take a moment.
#> The number of sites and/or records matched by the query terms is large, so the download may take some time.
#> [1] "Downloading data from sites with fewer than 350000 results by grouping them together."
#>   |                                                                              |                                                                      |   0%
#> Error in httr2::req_perform(obs_url): HTTP 500 Internal Server Error.

df2 <- TADA_SimpleCensoredMethods(df,
  nd_method = "multiplier",
  nd_multiplier = 0.5, od_method = "as-is", od_multiplier = "null"
)
#> Error in TADA_CheckType(.data, "data.frame", "Input object"): Input object must be of class 'data.frame'

df2 <- TADA_RunKeyFlagFunctions(df2, clean = TRUE)
#> Error: object 'df2' not found

df2 <- TADA_HarmonizeSynonyms(df2)
#> Error: object 'df2' not found

df3 <- TADA_CalculateTotalNP(df2, daily_agg = "max")
#> Error: object 'df2' not found
```
