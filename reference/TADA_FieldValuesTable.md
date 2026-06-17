# Field Values Summary Table

Function creates a dataframe containing the relative proportions of
values in a given field in a TADA dataset.

## Usage

``` r
TADA_FieldValuesTable(.data, field = "null", characteristicName = "null")
```

## Arguments

- .data:

  TADA dataframe

- field:

  The field (column) the user would like to see represented in a pie
  chart.

- characteristicName:

  Optional. Defaults to "null". A vector of TADA-converted (all caps)
  WQP characteristics a user may provide to filter the results to one or
  more characteristics of interest. "null" will show a summary table for
  the whole dataset.

## Value

A summary dataframe.

## Examples

``` r
# Load example dataset:
utils::data(Data_Nutrients_UT)

# Create a list of parameters in the dataset and the number of records of
# each parameter:
TADA_FieldValuesTable(Data_Nutrients_UT, field = "TADA.CharacteristicName")
#>      Value Count
#> 1 NITROGEN 12544
#> 2  NITRATE  1927
#> 3  AMMONIA   118
```
