# Identify Potentially Duplicated Data by Multiple Organizations

Identifies records that may be duplicated across different
organizations. Records are flagged as potential duplicates when they
share the same date, time, characteristic name, activity type, and exact
result value across organizations, and the associated monitoring
locations are within a specified distance.

## Usage

``` r
TADA_FindPotentialDuplicatesMultipleOrgs(
  .data,
  dist_buffer = 100,
  org_hierarchy = "none",
  clean = FALSE
)
```

## Arguments

- .data:

  A TADA dataframe. If needed, this function may internally convert the
  input to an sf object for spatial processing.

- dist_buffer:

  Numeric. Distance in meters used to define nearby sites. Records from
  different organizations are considered for duplicate checking only
  when their monitoring locations are within this distance. Default is
  100.

- org_hierarchy:

  A character vector of organization identifiers used to prioritize
  which record is selected within each duplicate group. If set to "none"
  or not provided, one record is selected at random from each group.
  Otherwise, the first organization in the vector that appears in a
  group is selected as the representative row.

- clean:

  Logical. If TRUE, rows flagged as "Duplicate Not Selected" are removed
  before returning the result. Default is FALSE.

## Value

The input dataframe with two additional columns:
TADA.MultipleOrgDup.Flag and TADA.MultipleOrgDupGroupID.

TADA.MultipleOrgDup.Flag indicates duplicate status for each row and
will be one of:

- "Not a Duplicate"

- "Duplicate Selected"

- "Duplicate Not Selected"

TADA.MultipleOrgDupGroupID identifies potential multi-organization
duplicate groups. Rows not assigned to a duplicate group are labeled
"Not a Duplicate". Rows with the same group ID belong to the same
potential duplicate set.

When clean = TRUE, rows flagged as "Duplicate Not Selected" are removed,
while rows flagged as "Duplicate Selected" and "Not a Duplicate" are
retained. If the duplicate flag columns already exist and clean = TRUE,
the function assumes those columns were created by this function and are
current; in that case, it skips recomputation and filters the existing
flags.

## Details

This function calls
[`TADA_FindNearbySites()`](https://usepa.github.io/EPATADA/reference/TADA_FindNearbySites.md)
internally to identify nearby monitoring locations. Records are then
compared across organizations within each nearby-site group. Because
this is a heuristic, users should review flagged records to determine
whether they are true duplicates or legitimate separate observations.

It is recommended to run
[`TADA_FindPotentialDuplicatesSingleOrg()`](https://usepa.github.io/EPATADA/reference/TADA_FindPotentialDuplicatesSingleOrg.md)
first to address within-organization duplicates before checking for
duplicates across organizations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataset with known multiple-organization duplicates
dat <- TADA_DataRetrieval(
  startDate = "2023-04-01",
  endDate = "2023-04-30",
  statecode = "PA",
  organization = c("21PA_WQX", "USGS-PA"),
  ask = FALSE
)

# Review unique organizations and consider prioritizing for
# duplicate selection (using optional org_hierarchy input)
unique(dat$OrganizationIdentifier)

# Identify potential duplicates and prioritize "21PA_WQX" over "USGS-PA"
dat1 <- TADA_FindPotentialDuplicatesMultipleOrgs(
  dat,
  dist_buffer = 100,
  org_hierarchy = c("21PA_WQX", "USGS-PA")
)
table(dat1$TADA.MultipleOrgDup.Flag)

# Review duplicate groupings
dat_review <- dat1 |>
  dplyr::select(
    OrganizationIdentifier,
    MonitoringLocationIdentifier,
    ActivityTypeCode,
    ActivityStartDate,
    ActivityStartTime.Time,
    TADA.ComparableDataIdentifier,
    SubjectTaxonomicName,
    TADA.ResultMeasureValue,
    TADA.ResultDepthHeightMeasure.MeasureValue,
    TADA.ResultDepthHeightMeasure.MeasureUnitCode,
    TADA.MultipleOrgDupGroupID,
    TADA.MultipleOrgDup.Flag
  ) |>
  dplyr::arrange(TADA.MultipleOrgDupGroupID)

# Re-run and keep only non-duplicate / representative rows
# Relies on existing duplicate flag columns to avoid re-computation
dat_clean <- TADA_FindPotentialDuplicatesMultipleOrgs(dat1, clean = TRUE)
} # }
```
