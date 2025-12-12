# Criteria Search Tool (CST) Reference Table

This function downloads State-Specific Water Quality Standards Effective
under the Clean Water Act (CWA) from EPA's Criteria Search Tool. This
file is reformatted as a data frame for use in R. This function caches
the table after it has been called once so subsequent calls will be
faster. To get the data dictionary for the CST see TADA_GetLegendCSTRef.
For the WQS document sources see TADA_GetSourcesCSTRef.

## Usage

``` r
TADA_GetCriteriaSearchToolRef()
```

## Value

Updated sysdata.rda with updated CriteriaSearchToolRef object

## Examples

``` r
CWACriteria <- TADA_GetCriteriaSearchToolRef()
```
