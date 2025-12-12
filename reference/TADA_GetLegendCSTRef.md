# Legend for the Criteria Search Tool Reference Table

This function downloads the legend from the EPA's Criteria Search Tool
which contains State-Specific Water Quality Standards Effective under
the Clean Water Act (CWA). This function caches the table after it has
been called once so subsequent calls will be faster.

## Usage

``` r
TADA_GetLegendCSTRef()
```

## Value

Updated sysdata.rda with updated CriteriaSearchToolRef object

## Examples

``` r
CWACriteria <- TADA_GetLegendCSTRef()
```
