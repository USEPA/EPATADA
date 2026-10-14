# .safe_fetchNHD

Helper for use with
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) when
calling
[`fetchNHD()`](https://usepa.github.io/EPATADA/reference/fetchNHD.md).
Adds a pause between requests, retries on server-side failures, and
stops after the maximum number of retries is reached.

## Usage

``` r
.safe_fetchNHD(.data, nhd_res, pause_sec = 1, max_tries = 3)
```

## Arguments

- .data:

  Object passed to
  [`fetchNHD()`](https://usepa.github.io/EPATADA/reference/fetchNHD.md).

- nhd_res:

  Character. NHD resolution to use, either `"Hi"` or `"Med"`.

- pause_sec:

  Numeric. Number of seconds to pause between queries.

- max_tries:

  Integer. Number of attempts to make before stopping.

## Value

The result of
[`fetchNHD()`](https://usepa.github.io/EPATADA/reference/fetchNHD.md) if
successful.

## Examples

``` r
if (FALSE) { # \dontrun{
.safe_fetchNHD(.data, nhd_res = "Hi")
} # }
```
