# checkColNames

This function checks column names using partial string matches. It is
designed to facilitate the use of user-supplied refs with differently
prefixed columns in Module 2 and 3 functions.

## Usage

``` r
checkColName(.data, partial.string = NULL)
```

## Arguments

- .data:

  A user-supplied ref data frame containing AssessmentUnitIdentifier,
  MonitoringLocationIdentifier, and WaterType columns. It is permitted
  (but not required) for these columns to use ATTAINS, TADA or other
  prefixes.

- partial.string:

  The character string used for partial string matching when checking
  column names.

## Value

A data frame with two columns identifying the exact column names for the
AssessmentUnitIdentifier, MonitoringLocationIdentifier, and WaterType
columns in a user-supplied ref file.
