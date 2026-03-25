# Identify Potentially Duplicated Data Uploads by a Single Organization

Identifies data records uploaded by the same organization with the same
date, time, monitoring location, activity type, characteristic name,
fraction, taxonomic name, depth columns, and result value and flags as
potential duplicates. However, it is at the discretion of the data user
to determine if the data records are unique or represent overlap that
could cause issues in the data analysis. Note, the dataset may contain
data from multiple organizations: the function performs the same
analysis on data from each organization.

## Usage

``` r
TADA_FindPotentialDuplicatesSingleOrg(.data)
```

## Arguments

- .data:

  TADA dataframe

## Value

The same input TADA dataframe with additional columns: a
TADA.SingleOrgDupGroupID column indicating whether a result is part of a
group that shares the same date, time, location, characteristic, etc. If
multiple rows include duplicates within a single organization, the rows
will have the same number identifier in the TADA.SingleOrgDupGroupID
column. In addition, the column TADA.SingleOrgDup.Flag is added, which
randomly flags rows within each TADA.SingleOrgDupGroupID group for
removal. Rows randomly selected for potential removal within a duplicate
group will have the TADA.SingleOrgDup.Flag = 'Duplicate' and all other
results in the group will have the value TADA.SingleOrgDup.Flag =
'Unique'.

## Examples

``` r
# Load dataset
utils::data(Data_6Tribes_5y)
# If duplicates exist, identify and flag them for removal
Data_6Tribes_5y_dups <- TADA_FindPotentialDuplicatesSingleOrg(Data_6Tribes_5y)
#> TADA_FindPotentialDuplicatesSingleOrg: 994 groups of potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.SingleOrgDupGroupID column and the function randomly selected one result from each group to represent a single, unduplicated value. Selected values are indicated in the TADA.SingleOrgDup.Flag as 'Unique', while duplicates are flagged as 'Duplicate' for easy filtering.
table(Data_6Tribes_5y_dups$TADA.SingleOrgDup.Flag)
#> 
#> Duplicate    Unique 
#>      1757    134175 
```
