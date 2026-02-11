# Generate Statistics Table

This function creates a summary table of the dataset based on grouping
columns. The 'TADA.ComparableDataIdentifier' column is the required and
default grouping column, but the user may include additional columns if
desired. The summary table includes the measurement count, location
count, censored data stats, min, max, and percentile stats, and a
suggested non-detect estimation method. The estimation method is based
on the following article: Baseline Assessment of Left-Censored
Environmental Data Using R Tech Note. More info can be found here:
https://www.epa.gov/sites/default/files/2016-05/documents/tech_notes_10_jun2014_r.pdf
Suggested methods are based on the measurement count, the number of
non-detects in the dataset, and the number of censoring levels
(detection limit types) and methods include Maximum Likelihood
Estimation, Robust ROS and Kaplan Meier.

## Usage

``` r
TADA_Stats(
  .data,
  group_cols = c("TADA.ComparableDataIdentifier"),
  sig_figs = 3,
  pct_digits = 1
)
```

## Arguments

- .data:

  TADA dataframe containing the data downloaded from the WQP, where each
  row represents a unique data record. Dataframe must include the
  columns 'TADA.ResultMeasureValue',
  'TADA.ResultMeasure.MeasureUnitCode', 'TADA.ResultSampleFractionText',
  'TADA.MethodSpeciationName', 'TADA.ComparableDataIdentifier',
  'TADA.CensoredData.Flag', 'DetectionQuantitationLimitTypeName', and
  'TADA.MonitoringLocationIdentifier' to run this function. The
  'TADA.ComparableDataIdentifier' can be added to the dataframe by
  running the function TADA_CreateComparableID().

- group_cols:

  Character vector of additional grouping columns to include along with
  'TADA.ComparableDataIdentifier'. For example:
  `group_cols = c("TADA.MonitoringLocationIdentifier")`.

- sig_figs:

  Integer. Number of significant figures to display for continuous
  statistics (UpperFence, LowerFence, Min, Mean, Max, and percentiles).
  Default is 3. Uses
  [`base::signif()`](https://rdrr.io/r/base/Round.html).

- pct_digits:

  Integer. Number of decimal places to display for percentage fields
  (Non_Detect_Pct, Over_Detect_Pct). Default is 1. Uses
  [`base::round()`](https://rdrr.io/r/base/Round.html).

## Value

A dataframe (stats table) with one row per group and the following
columns: Location_Count, Measurement_Count, Non_Detect_Count,
Non_Detect_Pct, Non_Detect_Lvls, Over_Detect_Count, Over_Detect_Pct,
UpperFence, LowerFence, Min, Mean, Max, Percentile_5th, Percentile_10th,
Percentile_15th, Percentile_25th, Percentile_50th_Median,
Percentile_75th, Percentile_85th, Percentile_95th, Percentile_98th, and
ND_Estimation_Method.

## Details

The output is formatted for readability:

- Continuous statistics (fences, min, mean, max, and percentiles) are
  rounded to a user-specified number of significant figures via
  [`base::signif()`](https://rdrr.io/r/base/Round.html).

- Percentage fields (Non_Detect_Pct, Over_Detect_Pct) are rounded to a
  user-specified number of decimal places via
  [`base::round()`](https://rdrr.io/r/base/Round.html).

- Missing values in `TADA.ResultMeasureValue` are excluded
  (`na.rm = TRUE`) when computing summary statistics.

- If `TADA.CensoredData.Flag` is not present, the function calls
  [`TADA_IDCensoredData()`](https://usepa.github.io/EPATADA/reference/TADA_IDCensoredData.md)
  to create it.

- If `TADA.NutrientSummation.Flag` is present, an informational note is
  printed.

- This function also suggests a ND_Estimation_Method following general
  guidance (Kaplan-Meier, ROS, MLE) based on censored percentage,
  censoring levels, and measurement count.

## Examples

``` r
# Load example dataset:
utils::data(Data_6Tribes_5y_Harmonized)

# Default rounding: 3 significant figures for continuous stats, 1 decimal for percentages
Data_6Tribes_5y_Harmonized_stats <- TADA_Stats(Data_6Tribes_5y_Harmonized)

# Custom rounding: 4 significant figures and whole-number percentages
Data_6Tribes_5y_Harmonized_stats_rounded <- TADA_Stats(
  Data_6Tribes_5y_Harmonized,
  sig_figs = 4,
  pct_digits = 0
)
```
