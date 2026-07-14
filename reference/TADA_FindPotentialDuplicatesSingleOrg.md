# Identify Potentially Duplicated Data Uploads by a Single Organization

Identifies records that may be duplicated within the same organization.
Records are flagged when they share the same organization, monitoring
location, date, time, activity type, characteristic name, taxonomic
name, sample fraction, depth/height measure columns, and exact result
value.

## Usage

``` r
TADA_FindPotentialDuplicatesSingleOrg(.data, clean = FALSE)
```

## Arguments

- .data:

  A TADA dataframe.

- clean:

  Logical. If TRUE, rows flagged as `"Duplicate Not Selected"` are
  removed before returning the result. If the duplicate flag columns
  already exist, they are reused when `clean = TRUE`; otherwise they are
  computed first. If `clean = FALSE`, duplicate flags are always
  recomputed. Default is FALSE.

## Value

The input TADA dataframe with these additional columns:
`TADA.SingleOrgDupGroupID` and `TADA.SingleOrgDup.Flag`.

`TADA.SingleOrgDupGroupID`: Identifier for a group of potentially
duplicated results within a single organization. Rows not in a duplicate
group are labeled `"Not a Duplicate"`.

`TADA.SingleOrgDup.Flag`: Duplicate status for each row. For each
duplicate group, one row is randomly selected to represent the group and
is marked `"Duplicate Selected"`. The remaining rows in that group are
marked `"Duplicate Not Selected"`. Rows outside duplicate groups are
marked `"Not a Duplicate"`. When `clean = TRUE`, rows flagged as
`"Duplicate Not Selected"` are removed and only representative rows plus
non-duplicate rows are returned.

## Details

The duplicate detection is performed independently within each
organization. This is a heuristic intended to identify likely duplicate
uploads, not to determine with certainty whether records are truly
duplicated. Users should review flagged records before removing them.

If the input data already contains the columns
`TADA.SingleOrgDupGroupID` and `TADA.SingleOrgDup.Flag`, then
`clean = TRUE` will reuse those columns and simply remove rows flagged
as `"Duplicate Not Selected"` without recomputing duplicate groups. If
`clean = FALSE`, duplicate flags are recomputed and any existing values
in those columns are overwritten.

## Examples

``` r
# Load example dataset
utils::data(Data_TribalNations)

# Identify duplicates and flag them for review
Data_TribalNations_dups <- TADA_FindPotentialDuplicatesSingleOrg(Data_TribalNations)
#> TADA_FindPotentialDuplicatesSingleOrg: 905 groups of potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.SingleOrgDupGroupID column and one result from each group was randomly selected to represent a single, unduplicated value. Selected values are indicated in the TADA.SingleOrgDup.Flag as 'Duplicate Selected', while duplicates are flagged as 'Duplicate Not Selected' for easy filtering.
table(Data_TribalNations_dups$TADA.SingleOrgDup.Flag)
#> 
#> Duplicate Not Selected     Duplicate Selected        Not a Duplicate 
#>                   1565                    905                 140649 

# Review duplicate rows
Data_TribalNations_review <- Data_TribalNations_dups |>
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
    TADA.SingleOrgDupGroupID,
    TADA.SingleOrgDup.Flag
  ) |>
  dplyr::arrange(TADA.SingleOrgDupGroupID)

# Keep only representative rows
Data_TribalNations_clean <- TADA_FindPotentialDuplicatesSingleOrg(
  Data_TribalNations,
  clean = TRUE
)
#> TADA_FindPotentialDuplicatesSingleOrg: 905 groups of potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.SingleOrgDupGroupID column and one result from each group was randomly selected to represent a single, unduplicated value. Selected values are indicated in the TADA.SingleOrgDup.Flag as 'Duplicate Selected', while duplicates are flagged as 'Duplicate Not Selected' for easy filtering.

# Clean an already-flagged dataset without recomputing flags
Data_TribalNations_clean2 <- TADA_FindPotentialDuplicatesSingleOrg(
  Data_TribalNations_dups,
  clean = TRUE
)
```
