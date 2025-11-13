# Check Columns

This function checks if the expected column names are in the dataframe.
It is used at the beginning of TADA functions to ensure the input data
frame is suitable (i.e. is either the full physical/chemical results
profile downloaded from WQP or the TADA profile template downloaded from
the EPA TADA webpage.)

## Usage

``` r
TADA_CheckColumns(.data, expected_cols)
```

## Arguments

- .data:

  A dataframe

- expected_cols:

  A vector of expected column names as strings

## Value

Invisible `NULL` if all expected columns are present; otherwise, an
error is thrown.
