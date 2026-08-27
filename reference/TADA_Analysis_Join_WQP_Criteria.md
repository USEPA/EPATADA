# Join WQP data to criteria and spatial MLSummaryRef (UNDER ACTIVE DEVELOPMENT)

Join WQP results to a criteria table by the best available key:

1.  TADA.ComparableDataIdentifier (if present in both and non-NA in
    criteria)

2.  TADA.CharacteristicName + TADA.ResultSampleFractionText +
    TADA.MethodSpeciationName

3.  TADA.CharacteristicName + TADA.ResultSampleFractionText

4.  TADA.CharacteristicName + TADA.MethodSpeciationName

5.  TADA.CharacteristicName (or when byChar = TRUE)

## Usage

``` r
TADA_Analysis_Join_WQP_Criteria(
  .data,
  criteria,
  byChar = FALSE,
  MLSummaryRef = NULL
)
```

## Arguments

- .data:

  A TADA data frame.

- criteria:

  data.frame of TADA compatible criteria table for any of either
  TADA.ComparableDataIdentifier and a combination of
  TADA.CharacteristicName, TADA.ResultSampleFractionText, and
  TADA.MethodSpeciationName

- byChar:

  A boolean value. If byChar = TRUE, this function will join the WQP
  data frame with the criteria table by only CharacteristicName,
  regardless of what has been filled out in the criteria table.

- MLSummaryRef:

  An optional data frame which contains the completed spatial crosswalk
  to assign any unique spatial criteria to a parameter, use, waterbody
  or monitoring site/assessment unit. This table is populated based on
  the inputs from the users and their desired level of analysis. If
  provided the data frame must contain these columns:
  "ATTAINS.OrganizationIdentifier", "ATTAINS.AssessmentUnitIdentifier",
  "MonitoringLocationIdentifier", "MonitoringLocationTypeName",
  "TADA.ComparableDataIdentifier", "ATTAINS.ParameterName",
  "ATTAINS.UseName", "ATTAINS.WaterType", "SaltFresh", "DepthCategory",
  "LongitudeMeasure", "LatitudeMeasure", "IncludeOrExclude" and
  "UniqueSpatialCriteria".

## Value

data.frame with WQP rows and matching criteria columns.

## Details

For each fallback pass, rows with NA in any of the pass keys are dropped
from both inputs for that pass. Left-join semantics are preserved
overall.

When MLSummaryRef is provided (optional), this function first joins the
WQP .data to the MLSummaryRef by MonitoringLocationIdentifier. NOTE:
MLSummaryRef is in active development and joins the ref tables of the
spatial summary, parameters and uses for analysis.

## Examples

``` r
# load example data.frame
utils::data("Data_MT_MissoulaCounty", package = "EPATADA")
MT_data <- Data_MT_MissoulaCounty

# load example criteria table from community hub
criteria_MT <- EPATADA::TADA_GetCriteriaFile(org_id = "MTDEQ")

# join the table by best match from what is filled out from the criteria table
MT_data_criteria <- TADA_Analysis_Join_WQP_Criteria(MT_data, criteria_MT)
#> Warning: Failed to coerce column 'SeasonStartDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonEndDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonStartDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonEndDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonStartDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonEndDate' to type 'date'; leaving unchanged.

# create the MLSummaryRef (ML only - no AU or other spatial columns)
params <- TADA_ParametersForAnalysis(
  Data_MT_MissoulaCounty, org_id = "MTDEQ", auto_assign = "Org")
#> TADA_ParametersForAnalysis: auto_assign == 'Org' was selected, finding an alias ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found.

uses <- TADA_UsesForAnalysis(Data_MT_MissoulaCounty,
 org_id = "MTDEQ", paramRef = params, auto_assign = TRUE)
#> TADA_UsesForAnalysis: auto_assign == TRUE was selected, assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed.

mlsummary <- TADA_MLSummary(
  Data_MT_MissoulaCounty,
  org_id = "MTDEQ",
  usesRef = uses)
#> displayNA = FALSE: This MLSummaryRef table will only display parameters and uses for a ML if it contains data collected for that TADA.CharacteristicName in your TADA data frame.

# join the table by best match, along with the MLSummaryRef
MT_data_criteria2 <- TADA_Analysis_Join_WQP_Criteria(
  MT_data,
  criteria_MT,
  MLSummaryRef = mlsummary)
#> Warning: Failed to coerce column 'SeasonStartDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonEndDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonStartDate' to type 'date'; leaving unchanged.
#> Warning: Failed to coerce column 'SeasonEndDate' to type 'date'; leaving unchanged.
```
