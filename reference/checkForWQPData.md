# checkForWQPData

Check the results of TADA_CreateATTAINSAUMLCrosswalk and
TADA_CreateAUMLCrosswalk to verify the WQP data frame contains
observations. For use in TADA leaflet mapping functions that utilize
ATTAINS data.

## Usage

``` r
checkForWQPData(.data = NULL)
```

## Arguments

- .data:

  The "TADA_with_ATTAINS" data frame that is part of the output of
  TADA_CreateATTAINSAUMLCrosswalk or TADA_CreateAUMLCrosswalk.

## Value

The function will stop and provide an error message if no WQP
observations are present.
