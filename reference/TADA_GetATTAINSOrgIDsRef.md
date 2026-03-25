# ATTAINS Organization Identifier Reference Table

Retrieve the latest ATTAINS domain values for org identifiers. Attempts
a live query via rExpertQuery::EQ_DomainValues("org_id"); on failure (or
if rExpertQuery isn't installed), falls back to the package’s installed
extdata (.rda). Results are cached in the current session.

## Usage

``` r
TADA_GetATTAINSOrgIDsRef(download_only = FALSE, refresh = FALSE, quiet = FALSE)
```

## Arguments

- download_only:

  Logical. If TRUE, bypasses cache and fallback and queries ATTAINS
  live. Errors if rExpertQuery is unavailable or the query fails.
  Default FALSE.

- refresh:

  Logical. Only used when download_only = FALSE. If TRUE, ignore any
  cached copy and attempt a fresh retrieval (download with extdata
  fallback), then update the cache.

- quiet:

  Logical. If TRUE, suppress non-critical fallback messages.

## Value

data.frame of organization domain values (de-duplicated)

## Details

The returned columns mirror the source endpoint. Content is
de-duplicated.
