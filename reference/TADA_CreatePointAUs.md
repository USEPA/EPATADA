# Create an ATTAINS AU–ML Crosswalk from WQP Monitoring Location IDs

Build a distinct crosswalk between WQP Monitoring Locations and ATTAINS
Assessment Units. For rows where `ATTAINS.AssessmentUnitIdentifier` is
missing or blank, the value is filled with
`TADA.MonitoringLocationIdentifier`, optionally with an `auid_prefix`
appended. Existing non-missing, non-blank AUIDs are left unchanged.

## Usage

``` r
TADA_CreatePointAUs(.data, auid_prefix = NULL)
```

## Arguments

- .data:

  A data frame containing, at minimum:

  - `TADA.MonitoringLocationIdentifier`

  If missing water-type values need to be crosswalked, the input must
  also contain:

  - `TADA.MonitoringLocationTypeName`

  Optionally, the input may already include:

  - `ATTAINS.AssessmentUnitIdentifier`

  - `ATTAINS.WaterType`

  If `ATTAINS.AssessmentUnitIdentifier` is absent, it will be added.

- auid_prefix:

  Character or `NULL`. If provided and non-empty, this prefix is
  included only for newly created `ATTAINS.AssessmentUnitIdentifier`
  values that were filled from `TADA.MonitoringLocationIdentifier`.
  Existing non-missing AUIDs are not modified. Use `NULL` to skip
  prefixing.

## Value

A distinct AU–ML crosswalk data frame containing:

- `ATTAINS.MonitoringLocationIdentifier`

- `ATTAINS.AssessmentUnitIdentifier`

- `ATTAINS.WaterType`

## Details

If `ATTAINS.WaterType` is missing or contains any blank values, the
function will attempt to populate it by calling
[`TADA_CrosswalkATTAINSWaterTypes()`](https://usepa.github.io/EPATADA/reference/TADA_CrosswalkATTAINSWaterTypes.md)
internally with `overwrite_existing = FALSE` and `validation = "none"`.

- Missing `ATTAINS.AssessmentUnitIdentifier` values are replaced with
  `TADA.MonitoringLocationIdentifier`.

- If `auid_prefix` is supplied and non-empty, it is included for only
  newly created AUIDs.

- `ATTAINS.MonitoringLocationIdentifier` is created from
  `TADA.MonitoringLocationIdentifier`.

- `ATTAINS.WaterType` is not overwritten unless it is missing or blank.

## See also

[`TADA_CrosswalkATTAINSWaterTypes()`](https://usepa.github.io/EPATADA/reference/TADA_CrosswalkATTAINSWaterTypes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Create missing AUIDs
ex_df <- data.frame(
  TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2", "LOC3"),
  TADA.MonitoringLocationTypeName   = c("Stream", "Lake", "Estuary"),
  ATTAINS.AssessmentUnitIdentifier  = c(NA_character_, "EXISTING_AU_001", ""),
  ATTAINS.WaterType                 = c(NA_character_, "", "ESTUARY"),
  stringsAsFactors = FALSE
)

result <- TADA_CreatePointAUs(ex_df)

# Example 2: Prefix only newly created AUIDs
result_prefixed <- TADA_CreatePointAUs(
  ex_df,
  auid_prefix = "WQX_"
)

# Example 3: AUID column is absent entirely
ex_df2 <- data.frame(
  TADA.MonitoringLocationIdentifier = c("SITE_A", "SITE_B"),
  TADA.MonitoringLocationTypeName   = c("River/Stream", "Lake, Reservoir, Impoundment"),
  ATTAINS.WaterType                 = c(NA_character_, NA_character_),
  stringsAsFactors = FALSE
)

result_missing_auid <- TADA_CreatePointAUs(ex_df2)
} # }
```
