# ATTAINS Parameter and Use Name by Organization Reference

Builds a compact crosswalk of parameter and use names by ATTAINS
organization, de-duplicated and cached for the session. Attempts live
retrieval via rExpertQuery::EQ_NationalExtract("assessments"). If the
live query is unavailable or empty, falls back to the package’s
installed extdata (.rda).

## Usage

``` r
TADA_GetATTAINSParamUseOrgRef(
  latest_cycle_only = TRUE,
  download_only = FALSE,
  refresh = FALSE,
  quiet = FALSE
)
```

## Arguments

- latest_cycle_only:

  Logical; TRUE (default) keeps only each organization’s latest
  reporting cycle when possible; FALSE returns all cycles.

- download_only:

  Logical. If TRUE, bypasses cache and fallback and queries ATTAINS
  live. Errors if rExpertQuery is unavailable or the query fails.

- refresh:

  Logical. Only used when download_only = FALSE. If TRUE, ignore cache
  and attempt a fresh retrieval (download with extdata fallback), then
  cache.

- quiet:

  Logical. If TRUE, suppress non-critical fallback messages.

## Value

data.frame with the columns listed above, de-duplicated

## Details

Output columns:

- ATTAINS.OrganizationIdentifier

- ATTAINS.OrganizationName

- ATTAINS.OrganizationType

- ATTAINS.ParameterName

- ATTAINS.UseName

- ATTAINS.WaterType

By default, only the latest reporting cycle per organization is used
when reportingCycle is available. Set latest_cycle_only = FALSE to
include all cycles.
