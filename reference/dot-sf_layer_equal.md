# Compare two sf layers for equality

Compares two `sf` objects by checking attribute names, attribute values,
and geometry text representation after normalizing column order and row
order. This is intended for maintenance workflows where unchanged layers
should not be rewritten.

## Usage

``` r
.sf_layer_equal(old_feature, new_feature)
```

## Arguments

- old_feature:

  an existing `sf` object read from a layer

- new_feature:

  a new `sf` object to compare against the existing layer

## Value

`TRUE` if the layers are considered equal, otherwise `FALSE`

## Examples

``` r
if (FALSE) { # \dontrun{
same <- .sf_layer_equal(old_feature, new_feature)
} # }
```
