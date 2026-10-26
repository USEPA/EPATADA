# Get WQP 3.0 Schema Reference Table

Retrieves the WQP 3.0 column-name schema mapping WQP Quick Reference
Guide.

## Usage

``` r
TADA_GetWQP3SchemaRef(download_only = FALSE, refresh = FALSE)
```

## Arguments

- download_only:

  Logical. If TRUE, bypasses the cache and package fallback and attempts
  to download the latest schema reference table directly, returning it
  without updating the cache. Errors if the download fails. If FALSE
  (default), uses a cached copy when available and updates the cache; on
  download failure, falls back to the package’s internal file.

- refresh:

  Logical. Only used when download_only = FALSE. If TRUE, ignore any
  cached copy and attempt to retrieve a fresh table (download, falling
  back to the package’s internal file on failure), then update the
  cache. If FALSE (default), return the cached table when available.
  Ignored when download_only = TRUE.

## Value

data.frame containing the WQP 3.0 to legacy column-name mapping
