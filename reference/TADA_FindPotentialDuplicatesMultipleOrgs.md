# Identify Potentially Duplicated Data by Multiple Organizations

This function identifies potential duplicate data records uploaded by
different organizations. It flags records with the same date, time,
characteristic name, and result value within a specified distance. The
flagged records are marked as potential duplicates, but users should
verify if they are truly duplicates or unique records.

## Usage

``` r
TADA_FindPotentialDuplicatesMultipleOrgs(
  .data,
  dist_buffer = 100,
  org_hierarchy = "none"
)
```

## Arguments

- .data:

  A TADA dataframe. This function runs `TADA_FindNearbySites` within it,
  which will transform .data into an `sf` object for spatial operations
  if needed.

- dist_buffer:

  Numeric. The distance in meters within which two sites with similar
  records are flagged as potential duplicates. Default is 100 meters.

- org_hierarchy:

  A vector of organization identifiers to prioritize when selecting
  representative records. If not specified, a random selection is made.

## Value

Returns the input dataframe with additional columns indicating potential
duplicates and their groupings:

- `TADA.MultipleOrgDuplicate`: A column that indicates if there is
  evidence of duplication due to submissions by multiple organizations
  ('Y' for yes, 'N' for no).

- `TADA.MultipleOrgDupGroupID`: A column containing a unique identifier
  for results that may represent duplicated measurement events.

- `TADA.ResultSelectedMultipleOrgs`: A column indicating which rows are
  selected to keep ('Y') or remove ('N') based on the organization
  hierarchy.

- `TADA.MonitoringLocationIdentifier`: A column indicating which
  monitoring locations are within the distance buffer from each other.

## Details

The function performs spatial operations by running
`TADA_FindNearbySites` within it. Duplicates are flagged if the distance
between sites is less than the specified `dist_buffer` (default is 100
meters). Each group in the `TADA.MultipleOrgDupGroupID` field indicates
that the `TADA.MonitoringLocationIdentifier` within each group are
within the specified distance from each other.

It is recommended to run this function after
`TADA_FindPotentialDuplicatesSingleOrg` to first address potential
duplicates within a single organization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load dataset
dat <- TADA_DataRetrieval(
  startDate = "2022-09-01",
  endDate = "2023-05-01", statecode = "PA", sampleMedia = "Water", ask = FALSE
)
unique(dat$OrganizationIdentifier)
# Identify potential duplicates and prioritize "21PA_WQX" organization
dat1 <- TADA_FindPotentialDuplicatesMultipleOrgs(dat,
  dist_buffer = 100, org_hierarchy = c("21PA_WQX")
)
table(dat1$TADA.ResultSelectedMultipleOrgs)
} # }
```
