# Criteria and Methodology Template

Assessment criteria and methodologies used to evaluate water quality
vary across the country. TADA users can fill out this template to define
the specific criteria and methodologies for each parameter and use
combination they are interested in analyzing. This table can be filled
out manually, auto-populated with uses and parameters from ATTAINS and
the input WQP dataframe, or developed with TADA helper functions
(recommended). It is recommended to run these three TADA helper
functions,
[`TADA_ParametersForAnalysis()`](https://usepa.github.io/EPATADA/reference/TADA_ParametersForAnalysis.md),
[TADA_UsesForAnalysis](https://usepa.github.io/EPATADA/reference/TADA_UsesForAnalysis.md),
and
[TADA_MLSummary](https://usepa.github.io/EPATADA/reference/TADA_MLSummary.md),
in that order to generate the Criteria and Methodology table specific
for your organization.

## Usage

``` r
TADA_DefineCriteriaMethodology(
  .data,
  org_id = NULL,
  MLSummaryRef = NULL,
  criteriaMethods = NULL,
  auto_assign = FALSE,
  AUMLRef = NULL,
  AU_UsesRef = NULL,
  displayUniqueId = FALSE,
  excel = FALSE,
  overwrite = FALSE
)
```

## Arguments

- .data:

  A TADA data frame. The user should run all desired data cleaning,
  processing, harmonization, filtering, and handling of censored data
  functions prior to running this function.

- org_id:

  The ATTAINS organization identifier must be supplied by the user.
  "USEPA" may be included as an org_id which will populate the EPA
  304(a) recommended criteria for any TADA.CharacteristicName if one is
  found. "All" or "NULL" are also allowable values and may be helpful
  for new ATTAINS users or those performing assessments for multiple
  states and tribes. If "All" is selected, this will return all prior
  ATTAINS information from all ATTAINS organizations in prior ATTAINS
  assessment cycles as individual rows for each organization. If "NULL"
  is selected all unique prior ATTAINS information from any ATTAINS
  organizations are returned but are not labeled and can be manually
  edited. Enter `rExpertQuery::EQ_DomainValues("org_id")` into the
  console to get a list of valid organization identifiers. A list of
  organization identifiers can also be found by downloading the ATTAINS
  Domains Excel file:
  https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
  Organization identifiers are listed in the "code" column of the
  "OrgName" tab.

- MLSummaryRef:

  An optional data frame which contains the completed spatial crosswalk
  to assign any unique spatial criteria to a parameter, use, waterbody
  or monitoring site/assessment unit. If provided the data frame must
  contain these columns: "ATTAINS.OrganizationIdentifier",
  "ATTAINS.AssessmentUnitIdentifier", "MonitoringLocationIdentifier",
  "MonitoringLocationTypeName", "TADA.ComparableDataIdentifier",
  "ATTAINS.ParameterName", "ATTAINS.UseName", "ATTAINS.WaterType",
  "SaltFresh", "DepthCategory", "LongitudeMeasure", "LatitudeMeasure",
  "IncludeOrExclude" and "UniqueSpatialCriteria".

- criteriaMethods:

  An optional data frame which contains the completed criteria and
  methodology table. This will be a user supplied table and any inputs
  in this table will be prioritized. Additional rows for any
  parameter(s) that are not found in the user supplied table will be
  included in the output. These rows will need to have the
  ATTAINS.ParameterName, ATTAINS.UseName, and the criteria and
  methodology inputs filled out manually if you would like analysis to
  be done for it.

- auto_assign:

  Boolean argument with two possible values: TRUE and FALSE. The default
  value is FALSE. If TRUE, a draft criteria and methods table is
  generated using default function inputs for
  [`TADA_ParametersForAnalysis()`](https://usepa.github.io/EPATADA/reference/TADA_ParametersForAnalysis.md),
  [TADA_UsesForAnalysis](https://usepa.github.io/EPATADA/reference/TADA_UsesForAnalysis.md),
  and
  [TADA_MLSummary](https://usepa.github.io/EPATADA/reference/TADA_MLSummary.md).
  .data and org_id are required inputs for this function if auto_assign
  = TRUE. It is also recommended to set excel = TRUE when auto_assign =
  TRUE. The criteria and methodology template should be reviewed
  carefully and edits can be made manually in Excel. When your review is
  complete, read the file back into R and re-run this function,
  TADA_DefineCriteriaMethodology, again. This time, use the
  criteriaMethods function input to specify the criteria and methodology
  table that has already been filled out.

- AUMLRef:

  An optional data frame input. If provided, this data frame should
  contain a completed crosswalk of monitoring location sites associated
  with an assessment unit. This data frame must contain the following
  column names which can be generated from the output of
  TADA_CreateAUMLCrosswalk: ATTAINS.OrganizationIdentifier,
  TADA.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
  and ATTAINS.WaterType.

- AU_UsesRef:

  An optional data frame input. If provided, the ATTAINS.UseName will be
  populated from the ATTAINS.UseName found in this data frame rather
  than the ATTAINS assessment profile. This data frame must contain the
  following column names which can be generated from the output of
  TADA_AssignUsesToAU: ATTAINS.OrganizationIdentifier,
  ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName, and
  ATTAINS.WaterType.

- displayUniqueId:

  A Boolean value. If TRUE, this will print all unique
  TADA.ComparableDataIdentifier in the criteria and methods table
  output. If your analysis needs are dependent on differing fractions or
  speciations, displaying the unique TADA.ComparableDataIdentifier will
  ensure you specify the correct crosswalk between ATTAINS.ParameterName
  that each individual TADA.ComparableDataIdentifier groups to in your
  TADA data frame. This is useful in the alternative options to generate
  the criteria and methods table without the reference tables.

- excel:

  A Boolean value that returns an excel spreadsheet if excel = TRUE.
  This spreadsheet is created in the user's downloads folder path. If
  you have any trouble locating the file, please type the following into
  your R console to locate it: file.path(Sys.getenv("USERPROFILE"),
  "Downloads"). The file will be named "myfileRef.xlsx". The excel
  spreadsheet will highlight the cells in which users should input
  information.

- overwrite:

  A Boolean value. If overwrite = TRUE, the excel file will be replaced
  (overwritten) by the new file you create if you re-run this function.
  Users should only specify overwrite = TRUE once they are ready to
  re-run this function if they have already ran it once.

## Value

A data frame with the criteria and methodology table in TADA format.

## Details

This criteria and methodology table will be in a TADA compatible format
and contain a list of allowable values within each column. For each
ATTAINS parameter name and use name, users may choose to define the full
criteria and methodologies information or magnitude values only. For
example, if there are separate criteria and methods for acute versus
chronic, rivers versus estuaries, different seasons, etc., then a user
will need to create additional rows to reflect this. Additional columns
are included in this output to capture data sufficiency information such
as minimum sample sizes, assessment period dates, and seasonality.

Allowable values for ATTAINS.UseName, ATTAINS.ParameterName, and
ATTAINS.OrganizationIdentifier: ATTAINS.uses =
rExpertQuery::EQ_DomainValues("use_name") ATTAINS.parameters \<-
rExpertQuery::EQ_DomainValues("param_name") ATTAINS.organizations \<-
rExpertQuery::EQ_DomainValues("org_id")

## Examples

``` r
# Example 1
# First, generate and fill out a parameter crosswalk (see TADA_ParametersForAnalysis()):
paramRef_UT <- TADA_ParametersForAnalysis(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = FALSE)
#> TADA_ParametersForAnalysis: auto_assign == 'All' was selected, 
#> finding an ATTAINS.ParameterName alias match for each TADA.ComparableDataIdentifier - by WQP CharacteristicName, if one is found.
paramRef_UT2 <- dplyr::mutate(paramRef_UT, ATTAINS.ParameterName = dplyr::case_when(
  grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
  grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
  grepl("NITROGEN", TADA.ComparableDataIdentifier) ~ "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
))
paramRef_UT3 <- TADA_ParametersForAnalysis(
  Data_Nutrients_UT,
  paramRef = paramRef_UT2, org_id = "UTAHDWQ", excel = FALSE
)
#> TADA_ParametersForAnalysis: auto_assign == 'All' was selected, 
#> finding an ATTAINS.ParameterName alias match for each TADA.ComparableDataIdentifier - by WQP CharacteristicName, if one is found.

# Next, enter the crosswalk generated above as the paramRef function input
# for TADA_UsesForAnalysis():
usesRef_UT <- TADA_UsesForAnalysis(
  Data_Nutrients_UT,
  paramRef = paramRef_UT3, org_id = c("UTAHDWQ"), excel = FALSE
)

# Now, run TADA_MLSummary()
MLSummaryRef_UT <- TADA_MLSummary(
  Data_Nutrients_UT,
  org_id = c("UTAHDWQ"),
  AU_UsesRef = NULL, AUMLRef = NULL,
  usesRef = usesRef_UT,
  excel = FALSE
)
#> displayNA = FALSE: This MLSummaryRef table will only display parameters and uses for a ML if it contains data collected for that TADA.CharacteristicName in your TADA data frame.

DefineCriteriaMethodology_UT <- TADA_DefineCriteriaMethodology(
  Data_Nutrients_UT,
  MLSummaryRef = MLSummaryRef_UT,
  displayUniqueId = TRUE,
  excel = FALSE
)

# Example 2: fill template with EPA304(a) criteria
epa_only <- TADA_DefineCriteriaMethodology(
  Data_MT_MissoulaCounty,
  org_id = "USEPA",
  auto_assign = TRUE
)
#> TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected but no MLSummaryRef. Generating TADA_MLSummary with default assignment.
#> TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment.
#> TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_UsesForAnalysis with default assignment.
#> NAs were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_ParametersForAnalysis() function.
#> TADA_UsesForAnalysis: auto_assign == TRUE was selected, assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed.
#> TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis.
#> TADA_DefineCriteriaMethodology: USEPA was included in your 'org_id': Including EPA304a recommended criteria by each unique TADA.CharacteristicName if one is found.

# Example 3: fill template with EPA304(a)
# and ATTAINS parameters and uses for MTDEQ:
epa_MT <- TADA_DefineCriteriaMethodology(Data_MT_MissoulaCounty,
  org_id = c("USEPA", "MTDEQ"), auto_assign = TRUE
)
#> TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected but no MLSummaryRef. Generating TADA_MLSummary with default assignment.
#> TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment.
#> TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_UsesForAnalysis with default assignment.
#> NAs were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_ParametersForAnalysis() function.
#> TADA_UsesForAnalysis: auto_assign == TRUE was selected, assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed.
#> TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected.
#> Finding an alias match between ATTAINS parameter name and Criteria Search Tool (CST) standardized pollutant names.
#> Finding an alias match between ATTAINS use name and Criteria Search Tool (CST) uses.
#> If an ATTAINS.ParameterName and ATTAINS.UseName alias was found, populating these rows with the CST magnitude values.
#> A many-to-many match is likely. User review is needed to ensure the proper parameter and uses from ATTAINS and CST alias crosswalk was accomplished (remove or add rows as needed).
#> 
#> TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'. TADA functions cannot currently handle analysis for these instances.
#> Warning in TADA_DefineCriteriaMethodology:  There are 2 TADA.CharacteristicName units that do not match with the CST autoassign MagnitudeUnit values. Converting these MagnitudeUnit Values from the CST to match the TADA.ResultMeasure.MeasureUnitCode in your dataframe. Please review these conversions.
#> TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis.
#> TADA_DefineCriteriaMethodology: USEPA was included in your 'org_id': Including EPA304a recommended criteria by each unique TADA.CharacteristicName if one is found.
```
