# Update result Measure Qualifier Code Reference Table

Function downloads and returns in the latest WQX ResultMeasureQualifier
Domain table, adds category information, and writes the data to
sysdata.rda.

## Usage

``` r
TADA_GetMeasureQualifierCodeRef()
```

## Value

sysdata.rda with updated WQXMeasureQualifierCodeRef object

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
