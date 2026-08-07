# TADA_SummarizeResultFrequency

Summarize result frequencies for each TADA.MonitoringLocationIdentifier
and TADA.ComparableDataIdentifier combination in the input data. Users
can choose whether or not to include continuous data, aggregate multiple
results from one day (min, max, mean), include sample depth as an
additional grouping factor, and select the time period (year, month,
week) at which result frequencies should be summarized.

## Usage

``` r
TADA_SummarizeResultFrequency(
  .data,
  depth = FALSE,
  daily_agg = "none",
  cont_data = FALSE,
  time_period = "none",
  group_by_year = TRUE
)
```

## Arguments

- .data:

  TADA dataframe which must include the columns:
  TADA.MonitoringLocationIdentifier, TADA.ComparableDataIdentifier,
  ActivityStartDate.

- depth:

  Boolean argument. When depth = TRUE, TADA.ConsolidatedDepth is
  factored into result summary groupings. If depth = TRUE and the
  ConsolidatedDepth column does not exist in the TADA df, it will be
  calculated with TADA_FlagDepthCategory. Default = FALSE, depth will
  not be taken into account when creating groupings to summarize result
  frequency.

- daily_agg:

  Character argument; with options "none", "mean", "min", or "max". The
  default is daily_agg = "none". When daily_agg = "none", all results
  will be retained. When daily_agg == "mean", the mean value in each
  group of results will be identified or calculated for each group. When
  daily_agg == "min" or when daily_agg == "max", the min or max value in
  each group of results (as determined by the depth category) will be
  identified or calculated for each group.

- cont_data:

  Boolean argument. When cont_data = TRUE, continuous data results will
  be included in the result summary. When cont_data = FALSE, continuous
  data will be excluded.

- time_period:

  Character string. Specifies which period of time the result
  frequencies should be summarized. Default equals "none" which means
  the selected time period is between the first and last
  ActivityStartDates for each group. Other options are "year", "month",
  and "week". Selecting a value other than "none" for time_period will
  add two additional columns: TADA.TimePeriodForSummary and
  TADA.ResultCount.

- group_by_year:

  Boolean argument. When TRUE, weekly or monthly time-period frequencies
  are grouped by both the selected week or month and the year. When
  FALSE, result frequencies are summarized by week or month across all
  years. Default is group_by_year equals TRUE. The group_by_year param
  does not apply when "year" or "none" is the selected time_period.

## Examples

``` r

# summarize result frequency by year
year <- TADA_SummarizeResultFrequency(Data_TribalNations_Harmonized,
time_period = "year")
#> TADA_SummarizeResultFrequency: QC samples were removed before summarizing result frequencies.

# summarize result frequency by month/year
month_year <- TADA_SummarizeResultFrequency(Data_TribalNations_Harmonized,
time_period = "month")
#> TADA_SummarizeResultFrequency: QC samples were removed before summarizing result frequencies.

# summarize result frequency by week/year
week_year <- TADA_SummarizeResultFrequency(Data_TribalNations_Harmonized,
time_period = "week")
#> TADA_SummarizeResultFrequency: QC samples were removed before summarizing result frequencies.

# summarize result frequency by month
month <- TADA_SummarizeResultFrequency(Data_TribalNations_Harmonized,
time_period = "month",
group_by_year = FALSE)
#> TADA_SummarizeResultFrequency: QC samples were removed before summarizing result frequencies.

# summarize result frequency by week
week <- TADA_SummarizeResultFrequency(Data_TribalNations_Harmonized,
time_period = "week",
group_by_year = FALSE)
#> TADA_SummarizeResultFrequency: QC samples were removed before summarizing result frequencies.
```
