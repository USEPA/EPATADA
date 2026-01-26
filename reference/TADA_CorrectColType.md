# TADA_CorrectColType

Correct column data types for TADA, ATTAINS, and user reference data
using the TADA column-type reference file bundled with EPATADA. This
ensures downstream TADA functions operate with expected classes.

## Usage

``` r
TADA_CorrectColType(.data)
```

## Arguments

- .data:

  A data.frame (or tibble) containing columns required for TADA
  functions.

## Value

A data.frame with corrected column classes.

## Details

The mapping of column names to target classes is read from:
inst/extdata/TADAColTypeRef.csv within the EPATADA package.

Supported types in the reference file are:

- character

- numeric

- integer

- logical

- factor

- date

Unrecognized or missing types are left unchanged.

## Examples

``` r
# df <- TADA_CorrectColType()
```
