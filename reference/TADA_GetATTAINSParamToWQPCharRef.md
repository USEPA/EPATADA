# ATTAINS Parameter and WQP Characteristic Alias Reference Table

Function downloads and returns the newest available crosswalk of alias
matches between ATTAINS.ParameterName and TADA.CharacteristicName.

## Usage

``` r
TADA_GetATTAINSParamToWQPCharRef(charAliasType = c("All", "ATTAINS"))
```

## Arguments

- charAliasType:

  A string value to indicate the WQX data source to use for finding an
  ATTAINS parameter name to WQX characteritic name alias. An alias may
  have been determined from another data source outside of ATTAINS which
  has an exact spelling to an ATTAINS parameter name that could be used
  for finding a match.

## Value

Updated sysdata.rda with updated ATTAINSParamToWQPCharRef object

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
