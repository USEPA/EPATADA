# Aggregate multiple result values to a min, max, or mean

This function groups TADA data by user-defined columns and aggregates
the TADA.ResultMeasureValue to a minimum, maximum, or mean value.

## Usage

``` r
TADA_AggregateMeasurements(
  .data,
  grouping_cols = c("ActivityStartDate", "TADA.MonitoringLocationIdentifier",
    "TADA.ComparableDataIdentifier", "ResultDetectionConditionText", "ActivityTypeCode",
    "TADA.ResultMeasure.MeasureUnitCode"),
  agg_fun = c("max", "min", "mean"),
  clean = FALSE
)
```

## Arguments

- .data:

  A TADA dataframe

- grouping_cols:

  The column names used to group the data

- agg_fun:

  The aggregation function used on the grouped data. This can either be
  'min', 'max', or 'mean'.

- clean:

  Boolean. Determines whether other measurements from the group
  aggregation should be removed or kept in the dataframe. If clean =
  FALSE, additional measurements that were considered are indicated in
  the TADA.ResultValueAggregation.Flag. The default is clean = FALSE.

## Value

A TADA dataframe with aggregated values combined into one row. If the
agg_fun is 'min' or 'max', the function will select the row matching the
aggregation condition and flag it as the selected measurement. If the
agg_fun is 'mean', the function will select a random row from the
aggregated rows to represent the metadata associated with the mean
value, and gives the row a unique ResultIdentifier: the original
ResultIdentifier with the prefix "TADA-". Function adds a
TADA.ResultValueAggregation.Flag to indicate which rows have been
aggregated.

## Examples

``` r
# Load example dataset
utils::data(Data_TribalNations)
# Select maximum value per day, site, comparable data identifier,
# unit, result detection condition,
# and activity type code. Clean all non-maximum measurements from grouped data.
Data_TribalNations_max <- TADA_AggregateMeasurements(Data_TribalNations,
  grouping_cols = c(
    "ActivityStartDate",
    "TADA.MonitoringLocationIdentifier",
    "TADA.ComparableDataIdentifier",
    "ResultDetectionConditionText",
    "ActivityTypeCode",
    "TADA.ResultMeasure.MeasureUnitCode"
  ),
  agg_fun = "max",
  clean = TRUE
)
#> Warning: TADA_AggregateMeasurements: One or more rows have TADA.ResultMeasureValue = NA. These NAs are ignored in aggregation.
#> Warning: There were 245 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `TADA.ResultMeasureValue = if (...) NULL`.
#> ℹ In group 280: `ActivityStartDate = "2021-04-05"`,
#>   `TADA.MonitoringLocationIdentifier = "REDLAKE_WQX-BLAC-H"`,
#>   `TADA.ComparableDataIdentifier = "AMMONIA-NITROGEN_DISSOLVED_AS N_MG/L"`,
#>   `ResultDetectionConditionText = "Not Detected at Reporting Limit"`,
#>   `ActivityTypeCode = "Sample-Routine"`, `TADA.ResultMeasure.MeasureUnitCode =
#>   "MG/L"`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 244 remaining warnings.
#> Aggregation results:
#> 5489111648

# Calculate a mean value per day, site, comparable data identifier, unit,
# result detection condition,
# and activity type code. Keep all measurements used to calculate mean measurement.
Data_TribalNations_mean <- TADA_AggregateMeasurements(Data_TribalNations,
  grouping_cols = c(
    "ActivityStartDate", "TADA.MonitoringLocationIdentifier",
    "TADA.ComparableDataIdentifier", "ResultDetectionConditionText",
    "ActivityTypeCode", "TADA.ResultMeasure.MeasureUnitCode"
  ),
  agg_fun = "mean",
  clean = FALSE
)
#> Warning: TADA_AggregateMeasurements: One or more rows have TADA.ResultMeasureValue = NA. These NAs are ignored in aggregation.
#> Aggregation results:
#> 765805489111648
```
