# Get CST Criteria table

Reads the Criteria table from the CST workbook (supports classic
"Criteria" or "Search Tool Criteria Data (3)"), normalizes the table,
and caches the result. Falls back to the installed workbook under
inst/extdata/cst-workbook.xlsx on download failure.

## Usage

``` r
TADA_CST_GetCriteria(download_only = FALSE, refresh = FALSE)
```

## Arguments

- download_only:

  Logical. If TRUE, bypasses the cache and package fallback and attempts
  to download the latest workbook directly from EPA, returning the
  requested sheet without updating the cache. Errors if the download
  fails.

- refresh:

  Logical. Only used when download_only = FALSE. If TRUE, ignore any
  cached copy and attempt to retrieve a fresh workbook (download,
  falling back to the package’s raw workbook on failure), then update
  the cache. If FALSE (default), return the cached table when available.
  Ignored when download_only = TRUE.

## Value

data.frame
