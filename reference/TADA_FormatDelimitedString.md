# Convert a delimited string to the format used by WQX 3.0 profiles for one-to-manys

This utility function takes a delimited string of entities, and a
delimiter (which defaults to a comma) and returns a new string in the
WQX 3.0 format of c("StringA","StringB").

## Usage

``` r
TADA_FormatDelimitedString(delimited_string, delimiter = ",")
```

## Arguments

- delimited_string:

  Character argument. Should be a string delimited by the character
  passed in the delimiter parameter.

- delimiter:

  Character argument The character used to delimit the string passed in
  delimited_string. Defaults to a comma.

## Value

String.
