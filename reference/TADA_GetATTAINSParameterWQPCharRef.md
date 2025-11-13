# ATTAINS Parameter and WQP Characteristic Exact Match Reference Table

Function downloads and returns the newest available crosswalk of exact
matches between ATTAINS.ParameterName and TADA.CharacteristicName.

## Usage

``` r
TADA_GetATTAINSParameterWQPCharRef()
```

## Value

Updated sysdata.rda with updated ATTAINSParameterWQPCharRef object

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
