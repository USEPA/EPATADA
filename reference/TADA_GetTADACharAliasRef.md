# ATTAINS Parameter, CST Pollutant and WQP Characteristic Alias Reference Table

Function downloads and returns the newest available crosswalk of alias
matches between ATTAINS.ParameterName, CST.PollutantName and
TADA.CharacteristicName from the WQX Characteristic alias domain and
joins in additional potential matches using TADA logic between the 3
sources for review. Note: A 'review' column is included which will be
kept in the inst/extdata folder and must be labeled as 'approved' to
keep a running list of alias that has been reviewed - otherwise,
updating this alias table will not remember what has already been
reviewed.

## Usage

``` r
TADA_GetTADACharAliasRef(
  ATTAINS.CST.tolerance = 1,
  CST.ATTAINS.tolerance = 1,
  ATTAINS.WQX.tolerance = 1,
  WQX.ATTAINS.tolerance = 1,
  CST.WQX.tolerance = 1,
  WQX.CST.tolerance = 1,
  set.all.tolerance = NA
)
```

## Arguments

- ATTAINS.CST.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with CST.ATTAINS.tolerance which defines
  the minimum percentage of the number of words that must be found in an
  ATTAINS parameter to a CST pollutant name for it to be considered an
  alias match.

- CST.ATTAINS.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with ATTAINS.CST.tolerance which defines
  the minimum percentage of the number of words that must be found in a
  CST pollutant name to an ATTAINS parameter for it to be considered an
  alias match.

- ATTAINS.WQX.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with ATTAINS.WQX.tolerance which defines
  the minimum percentage of the number of words that must be found in an
  ATTAINS parameter to a WQX characteristic name for it to be considered
  an alias match.

- WQX.ATTAINS.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with ATTAINS.WQX.tolerance which defines
  the minimum percentage of the number of words that must be found in a
  WQX characteristic name to an ATTAINS parameter to for it to be
  considered an alias match.

- CST.WQX.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with WQX.CST.tolerance which defines the
  minimum percentage of the number of words that must be found in an WQX
  characteristic to a CST pollutant name for it to be considered an
  alias match.

- WQX.CST.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with CST.WQX.tolerance which defines the
  minimum percentage of the number of words that must be found in a CST
  pollutant name to a WQX characteristic for it to be considered an
  alias match.

- set.all.tolerance:

  optional: default is NA, if a user specifies a numeric value ranging
  from 0 to 1 (0% to 100%), this will populate all tolerances to this
  value.

## Value

updated rda consisting of potential additional ATTAINS.ParameterName to
WQX.CharacteristicName alias for review. TADA team will review and
decide if these are appropriate aliases.

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
