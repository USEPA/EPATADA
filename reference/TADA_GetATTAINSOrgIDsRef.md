# ATTAINS Organization Identifier Reference Table

Function downloads and returns the newest available crosswalk of
state/tribe/territory codes and their respective organization names and
organization identifiers.

## Usage

``` r
TADA_GetATTAINSOrgIDsRef()
```

## Value

Updated sysdata.rda with updated ATTAINSOrgIDsRef object

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
