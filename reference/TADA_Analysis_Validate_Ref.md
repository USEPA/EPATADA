# Validate Reference Tables Against WQP Data and Criteria

Checks for mismatching combinations between a WQP data frame, a criteria
table, and optional spatial reference tables. When reference tables are
supplied, the function compares key identifying fields and issues
warnings if values present in one table are not found in another.

## Usage

``` r
TADA_Analysis_Validate_Ref(.data, criteria, AUMLRef = NULL, AU_UsesRef = NULL)
```

## Arguments

- .data:

  A data frame containing WQP data. Must include
  `TADA.CharacteristicName` and, if applicable, spatial columns such as
  `ATTAINS.WaterType`, `SaltFresh`, `UniqueSpatialCriteria`, and/or
  `DepthCategory`.

- criteria:

  A data frame containing the final criteria table. Must include
  `TADA.CharacteristicName` and any other fields needed for matching and
  validation.

- AUMLRef:

  Optional. A reference table for assessment unit–level water type
  mappings. If provided, the function checks for mismatches in
  `ATTAINS.WaterType`.

- AU_UsesRef:

  Optional. A reference table for assessment unit use mappings. If
  provided, the function checks for mismatches in `ATTAINS.UseName`.

## Value

Invisibly returns `NULL`. The function is called for its side effect of
issuing warnings when mismatches are detected.

## Details

This function is primarily used as a pre-check before joining criteria
and reference tables to WQP data for analysis.

The function performs the following checks:

1.  Compares `criteria` and `AU_UsesRef` on `TADA.CharacteristicName`
    and `ATTAINS.UseName` when `AU_UsesRef` is provided.

2.  Compares `criteria` and `AUMLRef` on `ATTAINS.WaterType` when
    `AUMLRef` is provided.

3.  Checks whether spatial combinations present in `criteria` also exist
    in `.data` for the overlapping characteristic names.

Character columns used in comparison are converted to uppercase and
trimmed before matching to reduce false mismatches due to case
differences or extra whitespace.

## Note

This function does not modify the input objects. It only validates them
and generates warnings when inconsistencies are found.

## Examples

``` r
if (FALSE) { # \dontrun{
# load example data.frame
utils::data("Data_MT_MissoulaCounty", package = "EPATADA")
MT_data <- Data_MT_MissoulaCounty

# load example criteria table from community hub
criteria_MT <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")

TADA_Analysis_Validate_Ref(
 Data_MT_MissoulaCounty,
 criteria = criteria_MT,
 AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
 AU_UsesRef = Data_MT_AU_UsesRef_Water)
} # }
```
