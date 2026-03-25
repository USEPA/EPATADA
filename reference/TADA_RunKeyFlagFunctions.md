# Run key flagging functions

This is a shortcut function to run all of the most important flagging
functions on a TADA dataset. See ?function documentation for
TADA_FlagResultUnit, TADA_FlagFraction, TADA_FindQCActivities,
TADA_FlagMeasureQualifierCode, and TADA_FlagSpeciation for more
information.

## Usage

``` r
TADA_RunKeyFlagFunctions(.data, clean = FALSE)
```

## Arguments

- .data:

  A TADA dataframe.

- clean:

  Boolean. Must be TRUE or FALSE. Determines whether to keep the suspect
  rows (or not). Defaults to FALSE.

## Value

A TADA dataframe with the following flagging columns:
TADA.ResultUnit.Flag, TADA.MethodSpeciation.Flag,
TADA.SampleFraction.Flag, TADA.MeasureQualifierCode.Flag and
TADA.ActivityType.Flag.

## Examples

``` r
# Run flagging functions but keep all results
keep_all <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, clean = FALSE)
#> TADA_FlagFraction: Rows with Suspect sample fractions have been flagged but retained. Review these rows using the TADA.SampleFraction.Flag column before proceeding and/or set clean = TRUE.
#> TADA_FlagSpeciation: Rows with Suspect speciations have been flagged but retained. Review these rows using the new TADA.MethodSpeciation.Flag column before proceeding and/or set clean = 'suspect_only' or 'both'.

# Run flagging functions and remove any suspect rows
remove_suspect <- TADA_RunKeyFlagFunctions(Data_6Tribes_5y, clean = TRUE)
#> TADA_FindQCActivities: Quality control samples have been removed or were not present in the input dataframe. Returning dataframe with TADA.ActivityType.Flag column for tracking.
```
