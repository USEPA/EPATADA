# Create downloadable table

This function creates a data table that can be downloaded as a .csv,
.xlsx or .pdf.

## Usage

``` r
TADA_TableExport(.data = NULL)
```

## Arguments

- .data:

  A data frame

## Value

A data table with multiple download options (.csv, .xlsx or .pdf).

## Examples

``` r
if (FALSE) { # \dontrun{
# return ATTAINS parameter domain values
TADA_TableExport(rExpertQuery::EQ_DomainValues("param_name", api_key = .setEQKey()))
} # }
```
