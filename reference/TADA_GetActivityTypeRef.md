# Update Activity Type Reference Table

Function downloads and returns in the latest WQX ActivityType Domain
table, adds QC category information, and writes the data to sysdata.rda.

## Usage

``` r
TADA_GetActivityTypeRef()
```

## Value

sysdata.rda with updated WQXActivityTypeRef object

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
