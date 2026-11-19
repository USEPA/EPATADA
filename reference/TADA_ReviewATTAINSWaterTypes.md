# Review ATTAINS Water Types

Validates ATTAINS.WaterType against allowable ATTAINS domain values. Can
either flag invalid values or update them using the crosswalk.

## Usage

``` r
TADA_ReviewATTAINSWaterTypes(.data, review_action = c("flag", "update"))
```

## Arguments

- .data:

  A TADA data frame.

- review_action:

  Character string. One of "flag" or "update".

## Value

A TADA data frame with `TADA.ATTAINSWaterType.Flag` added. If
`review_action = "update"`, invalid values may also be replaced if an
ATTAINS water type match is available.

## Examples

``` r

if (FALSE) { # \dontrun{

# example of updating invalid ATTAINS water types
example.df <- tibble::tibble(
TADA.MonitoringLocationIdentifier = c("id1", "id2"),
TADA.MonitoringLocationTypeName = c("RIVER/STREAM", "LAKE"),
ATTAINS.WaterType = c("INVALID WATER TYPE 1", "INVALID WATER TYPE 2")
)

review.df <- TADA_ReviewATTAINSWaterTypes(df, review_action = "update")
} # }
```
