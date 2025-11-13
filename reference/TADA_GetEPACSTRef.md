# Get EPA Criteria Search Tool (CST) Data

Function downloads and returns the newest available Criteria Search Tool
and associated EPA 304a Criteria pollutant names as a reference
dataframe. This dataframe is used in TADA_CreateParamRef() and
TADA_CreateUseParamRef() as the basis for the pulling in EPA304a
recommended pollutant names and use names.

## Usage

``` r
TADA_GetEPACSTRef()
```

## Value

Dataframe of EPA304a recommended criteria from EPA's Criteria Search
Tool (CST) for a pollutant and use name.

## Details

Currently only characteristics identified by the TADA Working Group as
priorities are included in the TADA crosswalk of WQP/TADA
characteristics and CST pollutant names. Run the following code in the
console to review the crosswalk: 'utils::read.csv(system.file("extdata",
"TADAPriorityCharUnitRef.csv", package = "EPATADA"))'
