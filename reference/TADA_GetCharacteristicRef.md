# Update Characteristic Reference Table

Function downloads and returns in the latest WQX Characteristic Domain
table and writes the data to sysdata.rda.

## Usage

``` r
TADA_GetCharacteristicRef()
```

## Value

sysdata.rda with updated WQXCharacteristicRef object (characteristic
reference table)

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
