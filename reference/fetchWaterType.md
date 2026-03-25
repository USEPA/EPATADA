# fetchWaterType

Use Expert Query web services to create a crosswalk of assessment unit
identifier to water type.

## Usage

``` r
fetchWaterType(au_list, api_key = NULL)
```

## Arguments

- au_list:

  A list of assessment units to fetch water types for.

- api_key:

  Optional character string. An api key for Expert Query web services.
  If not supplied, the default TADA api key will be used. For best
  performance, it is recommended that users obtain and use their own api
  key. Request an api key here:
  https://owapps.epa.gov/expertquery/api-documentation

## Value

The function returns a data frame with an assessment unit/water type
crosswalk. If no water type matches are found, a message explaining this
is printed.
