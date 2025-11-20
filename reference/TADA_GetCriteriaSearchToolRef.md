# Criteria Search Tool Reference Table

This function downloads the latest criteria search tool from the EPA OST
and returns it. Before use, the downloaded data is cleaned and formatted
as the the initial ~200 rows contain the legend and data dictionary,
which need to be removed.

## Usage

``` r
TADA_GetCriteriaSearchToolRef()
```

## Value

Updated sysdata.rda with updated ATTAINSParamToWQPCharRef object

## Details

This function caches the table after it has been called once so
subsequent calls will be faster.
