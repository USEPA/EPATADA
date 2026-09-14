# Generate a candidate crosswalk of ATTAINS and CST use aliases

Creates a review table of potential alias matches between ATTAINS use
names and Criteria Search Tool (CST) uses.

## Usage

``` r
TADA_GetTADAUsesAliasRef(
  ATTAINS.CST.tolerance = 0.15,
  CST.ATTAINS.tolerance = 0.15,
  set.all.tolerance = NA,
  download_only = FALSE,
  refresh = FALSE
)
```

## Arguments

- ATTAINS.CST.tolerance:

  Numeric value between 0 and 1. Default is `0.15`. This is the minimum
  proportion of words that must match from an ATTAINS use name to a CST
  use name for a candidate alias to be returned.

- CST.ATTAINS.tolerance:

  Numeric value between 0 and 1. Default is `0.15`. This is the minimum
  proportion of words that must match from a CST use name to an ATTAINS
  use name for a candidate alias to be returned.

- set.all.tolerance:

  Optional numeric value between 0 and 1. If supplied, this value is
  applied to both `ATTAINS.CST.tolerance` and `CST.ATTAINS.tolerance`.

- download_only:

  Logical. If TRUE, bypasses the cache and rebuilds the table directly
  from live sources, returning it without updating the cache. If FALSE
  (default), uses a cached copy when available and updates the cache.

- refresh:

  Logical. Only used when `download_only = FALSE`. If TRUE, ignore any
  cached copy and rebuild a fresh table, then update the cache. If FALSE
  (default), return the cached table when available. Ignored when
  `download_only = TRUE`.

## Value

A data frame containing candidate ATTAINS-CST use alias matches for
review. Returned columns include CST entity and use information, ATTAINS
organization and use name information, review status, and supporting
match metadata.

## Details

The function uses two matching strategies:

1.  **Domain/context matching**: ATTAINS `use_name` values are aligned
    to CST use categories using the ATTAINS `context2` field and the CST
    human-health/aquatic-life indicator and water/water and organism
    indicator.

2.  **Token-based matching**: ATTAINS and CST use names are split into
    words and compared by shared terms. The proportion of matching words
    is used to determine whether a pair is returned.

If no direct use match is found, but an ATTAINS parameter matches a CST
pollutant name, all CST uses for the matching organization are returned
so the user can review and select the appropriate CST magnitude values.

The output is intended for manual review. Many-to-many matches are
expected, and a CST use may appear multiple times for a single ATTAINS
use name.

Rows marked `APPROVED` or `REJECTED` in
`inst/extdata/TADAUsesAliasRef.csv` are retained in the output when that
file is available. New candidate matches are appended with
`review = "New row: Needs Review"`.

To document a review decision, update the `review` column in the CSV
file: `APPROVED` means the alias match is accepted, and `REJECTED` means
the alias match is not accepted. If desired, update `Last.Change.Date`
to record the date of the decision.

## Examples

``` r
if (FALSE) { # \dontrun{
# Default tolerances
TADA_GetTADAUsesAliasRef()

# Use the same tolerance in both directions
TADA_GetTADAUsesAliasRef(set.all.tolerance = 0.2)

# More selective matching
TADA_GetTADAUsesAliasRef(
  ATTAINS.CST.tolerance = 0.30,
  CST.ATTAINS.tolerance = 0.30
)

# Less selective matching
TADA_GetTADAUsesAliasRef(
  ATTAINS.CST.tolerance = 0.10,
  CST.ATTAINS.tolerance = 0.10
)
} # }
```
