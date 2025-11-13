# Update Detection Quantitation Limit Type Reference Table

Function downloads and returns in the latest WQX
DetectionQuantitationLimitType Domain table, adds additional target unit
information, and writes the data to sysdata.rda.

## Usage

``` r
TADA_GetDetLimitRef()
```

## Value

sysdata.rda with updated WQXDetectionQuantitationLimitTypeRef object
(detection limit type reference table for censored data)

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
