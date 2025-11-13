# Update Result Detection Condition Reference Table

Function downloads and returns in the latest WQX
ResultDetectionCondition Domain table, adds additional target unit
information, and writes the data to sysdata.rda.

## Usage

``` r
TADA_GetDetCondRef()
```

## Value

sysdata.rda with updated WQXResultDetectionConditionRef object
(detection condition reference table for censored data)

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
