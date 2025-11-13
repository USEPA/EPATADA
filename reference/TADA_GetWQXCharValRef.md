# WQX QAQC Characteristic Validation Reference Table

Function downloads and returns the newest available (cleaned) raw Water
Quality Exchange (WQX) QAQC Characteristic Validation reference table.
The WQXcharValRef dataframe contains information for four functions:
InvalidFraction, InvalidResultUnit, InvalidSpeciation, and
UncommonAnalyticalMethodID.

## Usage

``` r
TADA_GetWQXCharValRef()
```

## Value

Updated sysdata.rda with updated WQXcharValRef object

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
