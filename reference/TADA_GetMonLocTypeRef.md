# Update Monitoring Location Type Name Reference Table

Function downloads and returns in the latest WQX
MonitoringLocationTypeName Domain table, adds additional information to
assist in identifying groundwater and surface water samples, and writes
the data to sysdata.rda.

## Usage

``` r
TADA_GetMonLocTypeRef()
```

## Value

sysdata.rda with updated WQXMonitoringLocationTypeName object (reference
table for identifying surface water samples by
MonitoringLocationTypeName)

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
