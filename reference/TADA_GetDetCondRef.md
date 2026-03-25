# Get WQX Result Detection Condition Reference Table

Retrieve the WQX Result Detection Condition domain table, normalize it,
and add a classification column, TADA.Detection_Type. The classifier
assigns each detection condition to one of: "Non-Detect", "Over-Detect",
"Other", or "Not Reviewed".

## Usage

``` r
TADA_GetDetCondRef(download_only = FALSE, refresh = FALSE, quiet = FALSE)
```

## Arguments

- download_only:

  Logical. If TRUE, bypasses the cache and package fallback and attempts
  to download the latest Detection Condition table directly from WQX,
  returning it without updating the cache. Errors if the download fails.
  If FALSE (default), uses a cached copy when available and updates the
  cache; on download failure (or unexpected structure), falls back to
  the package’s internal file.

- refresh:

  Logical. Only used when download_only = FALSE. If TRUE, ignore any
  cached copy and attempt to retrieve a fresh table (download, falling
  back to the package’s internal file on failure), then update the
  cache. If FALSE (default), return the cached table when available.
  Ignored when download_only = TRUE.

- quiet:

  Logical. If TRUE, suppresses fallback messages when a live download
  fails and the function reverts to the installed extdata. Default is
  FALSE.

## Value

A data.frame that includes at least:

- Name: the detection condition name (from WQX)

- TADA.Detection_Type: the assigned classification

- Additional columns such as Description and Last.Change.Date are
  returned when provided by WQX

## Details

When download_only = FALSE (default), the function first attempts to
download the latest table from WQX; if the download fails or the
structure is unexpected (e.g., missing the required "Name" column), it
falls back to the package’s installed extdata (.rda). The resulting
table is cached for the current R session.

## Examples

``` r
# Cached retrieval (download with fallback as needed)
ref <- TADA_GetDetCondRef()

# Force a fresh retrieval (ignores cache)
ref_fresh <- TADA_GetDetCondRef(refresh = TRUE)

# Download only: no cache update and no fallback (errors if offline)
if (FALSE) { # \dontrun{
ref_live <- TADA_GetDetCondRef(download_only = TRUE)
} # }
```
