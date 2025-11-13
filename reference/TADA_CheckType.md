# Check Type

This function checks if the inputs to a function are of the expected
type. It is used at the beginning of TADA functions to ensure the inputs
are suitable.

## Usage

``` r
TADA_CheckType(arg, type, paramName = deparse(substitute(arg)))
```

## Arguments

- arg:

  An input argument to check

- type:

  Expected class of input argument

- paramName:

  Optional name for argument to use in error message
