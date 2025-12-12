# ATTAINS Parameter and Use Name by Organization Reference Key

Function downloads and returns the newest available ATTAINS domain
values reference dataframe which includes all parameters and uses listed
as a cause by ATTAINS organizations in previous assessments. This
dataframe is used in TADA_ParametersForAnalysis() and
TADA_UsesForAnalysis() as the basis for pulling in prior ATTAINS
parameter names and use names by organization name. This helps to filter
selections in the Excel drop down menu.

## Usage

``` r
TADA_GetATTAINSParamUseOrgRef()
```

## Value

Dataframe including ATTAINS parameters and uses for each organization.
