# Summarize data downloaded by specified column

Function to summarize the number of sites and records downloaded from
the WQP for each unique column group.

## Usage

``` r
TADA_SummarizeColumn(.data, col = "TADA.CharacteristicName")
```

## Arguments

- .data:

  TADA dataframe containing the data downloaded from the WQP, where each
  row represents a unique data record.

- col:

  A text string name of the column the user would like summarized.

## Value

A dataframe containing a column for each unique element, the number of
sites with that element populated, and the number of records with that
element populated.
