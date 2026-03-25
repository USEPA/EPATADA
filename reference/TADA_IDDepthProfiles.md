# TADA_IDDepthProfiles

This function identifies depth profiles within a data frame to assist
the user in selecting params for TADA_DepthProfilePlot. A TADA
compatible data set is required. If TADA_FlagDepthCategory has not yet
been run, it will be run as part of this function. The output data frame
is grouped by TADA.MonitoringLocationIdentifier, OrganizationIdentifier,
and ActivityStartDate.

## Usage

``` r
TADA_IDDepthProfiles(.data, nresults = TRUE, nvalue = 2, aggregates = FALSE)
```

## Arguments

- .data:

  TADA dataframe which must include the columns ActivityStartDate,
  TADA.ConsolidatedDepth, TADA.ConsolidatedDepth.Unit,
  TADA.ConsolidatedDepth.Bottom, TADA.ResultMeasureValue,
  TADA.ResultMeasureValue.UnitCode, OrganizationIdentifier,
  TADA.MonitoringLocationName, TADA.MonitoringLocationIdentifier, and
  TADA.ComparableDataIdentifier.

- nresults:

  Boolean argument with options "TRUE" or "FALSE". The default is
  nresults = TRUE, which means that the number of results for each
  characteristic are added within the
  TADA.CharacteristicsForDepthProfile column. When nresults = FALSE.

- nvalue:

  numeric argument to specify the number of results required to identify
  a depth profile. The default is 2, which means that a depth profile
  will be identified if 2 or more results at different depths exists for
  the same ActivityStartDate, TADA.MonitoringLocationIdentifier,
  OrganizationIdentifier, and TADA.ComparableDataIdentifier. A few
  characteristics are excluded from this requirement because they are
  expected to have only a single result in depth units (ex: secchi disk
  depth).

- aggregates:

  Boolean argument with options "TRUE" or "FALSE". The default is
  aggregates = FALSE, which means that any aggregate values created
  (means) in TADA_FlagDepthCategory are excluded from identifying depth
  profile data. Aggregate values that were selected from the existing
  data set (max and min) remain. Only columns created/add by
  TADA_FlagDepthCategory are removed when aggregates = FALSE. When
  aggregates = TRUE, all aggregate values are included when identifying
  depth profile data.

## Value

A dataframe with the columns TADA.MonitoringLocationIdentifier,
TADA.MonitoringLocationName, OrganizationIdentifier, ActivityStartDate,
TADA.CharacteristicsForDepthProfile. Based on the user input for the
nresults param, TADA.CharacteristicsForDepthProfile may or may not
contain the number of results for each characteristic.

## Details

A new column, TADA.CharacteristicsForDepthProfile, is created which
lists the characteristics available for depth profile analysis. Using
the, nresults param, users can specify whether characteristic names
should be followed by the number of results available for the
characteristic in parentheses.

## Examples

``` r
# Load data frame
utils::data(Data_6Tribes_5y)

# find depth profile data without showing number of results
Data_6Tribes_5y_DepthProfileID_Nresults <-
  TADA_IDDepthProfiles(Data_6Tribes_5y, nresults = FALSE)
#> TADA_IDDepthProfiles: Necessary columns are being added to the data frame using TADA_DepthCatgegory.Flag function.
#> TADA_FlagDepthCategory: checking data set for depth values. 59647 results have depth values available.
#> TADA_FlagDepthCategory: assigning depth categories.
#> TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for entire water column.
#> TADA_FlagDepthCategory: No aggregation performed.

# find depth profile data showing number of results
Data_6Tribes_5y_DepthProfileID <- TADA_IDDepthProfiles(Data_6Tribes_5y)
#> TADA_IDDepthProfiles: Necessary columns are being added to the data frame using TADA_DepthCatgegory.Flag function.
#> TADA_FlagDepthCategory: checking data set for depth values. 59647 results have depth values available.
#> TADA_FlagDepthCategory: assigning depth categories.
#> TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for entire water column.
#> TADA_FlagDepthCategory: No aggregation performed.
```
