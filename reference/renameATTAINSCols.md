# renameATTAINSCols

This function adds the ATTAINS prefix and changes column name
capitalization to match the TADA format.

## Usage

``` r
renameATTAINSCols(.data, return_list = FALSE, format = "tada")
```

## Arguments

- .data:

  A data frame containing columns from ATTAINS geospatial web services.

- return_list:

  Boolean argument. When return_list = TRUE, the function returns a list
  of the TADA formatted names for ATTAINS columns. When return_list =
  FALSE, the input .data data frame is updated so column names from
  ATTAINS geospatial web services match the TADA format. Default is
  return_list = FALSE.

- format:

  Character argument. The format the user wants to switch the column
  names too. When format = "tada", the ATTAINS prefix and TADA
  capitalization will be applied. When format = "attains", TADA
  formatted columns will be renamed to the original ATTAINS names.
  Default = "tada".

## Value

A data frame with column name from ATTAINS geospatial web service
updated to match the TADA format. Or when return_list = TRUE, a list of
all TADA formatted ATTAINS column names.
