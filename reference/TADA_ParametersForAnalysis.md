# Create or Update ATTAINS, TADA/WQP/WQX, and EPA Criteria Search Tool (CST) Parameter Name Crosswalk

Use this function to help generate a crosswalk between each
ATTAINS.ParameterName used by a specific state or tribal nation and each
TADA.ComparableDataIdentifier present in the input TADA dataframe. The
crosswalk can be filled out by users within R or Excel. By default this
function will generate a user friendly Excel spreadsheet that includes a
drop down list of all ATTAINS parameters that have been listed as a
cause in prior ATTAINS cycle for the organization selected in the
function input 'org_id'. It also highlights the cells in which users
should input information. The excel spreadsheet will be automatically
downloaded to a user's downloads folder path. Users may need to insert
additional rows into the crosswalk if:

1.  an ATTAINS.ParameterName corresponds with multiple
    TADA.ComparableDataIdentifiers Example: An organization uses
    "ALUMINUM" for all aluminum related parameter causes but this
    ATTAINS.ParameterName may crosswalk to "ALUMINUM_TOTAL_NA_UG/L" for
    one use and "ALUMINUM_DISSOLVED_NA_UG/L" for another use; or

2.  an TADA.ComparableDataIdentifiers corresponds with multiple
    ATTAINS.ParameterNames. Example: An organization uses both "pH,
    HIGH" and "pH, LOW" as ATTAINS.ParameterNames, but both crosswalk to
    the same TADA.ComparableDataIdentifier, "PH_NA_NA_STD UNITS".

## Usage

``` r
TADA_ParametersForAnalysis(
  .data,
  org_id = NULL,
  paramRef = NULL,
  auto_assign = "All",
  AUMLRef = NULL,
  excel = FALSE,
  overwrite = FALSE
)
```

## Arguments

- .data:

  A TADA dataframe after all desired data cleaning, processing,
  harmonization, filtering, and censored data handling functions have
  been applied.

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

- paramRef:

  A data frame which contains a completed crosswalk between
  TADA_ComparableDataIdentifier(s) and ATTAINS.ParameterName(s). This
  data frame must contain at least these two column names:
  TADA.ComparableDataIdentifier and ATTAINS.ParameterName. Users who are
  interested in performing analyses for more than one organization
  (multiple states and/or tribes) also need to include an additional
  column name: 'ATTAINS.OrganizationIdentifier'.

- auto_assign:

  Character string with value of "None", "All", or "Org". Default is
  "All". If a user selects "All" this provides a match between
  ATTAINS.ParameterName(s) and
  TADA.CharacteristicName(s)/TADA.ComparableDataIdentifier(s) using a
  TADA reviewed characteristic alias table. If "Org" is selected then
  this only returns the ATTAINS.ParameterName(s) and
  TADA.CharacteristicName(s)/ TADA.ComparableDataIdentifier(s) match if
  the specified ATTAINS organization has included that ATTAINS parameter
  name in past assessment cycles. If "None" is selected, users will be
  required to fill the crosswalk on their own completely or provide
  their own paramRef crosswalk which contains the crosswalk of
  ATTAINS.ParameterName(s) to
  TADA.CharacteristicName(s)/TADA.ComparableDataIdentifier(s).

- AUMLRef:

  An optional data frame input. If provided, this data frame should
  contain a completed crosswalk of monitoring location sites associated
  with an assessment unit. This data frame must contain the following
  column names which can be generated from the output of
  TADA_CreateAUMLCrosswalk: ATTAINS.OrganizationIdentifier,
  TADA.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
  and ATTAINS.WaterType.

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

An excel file or data frame which contains the columns:
TADA.ComparableDataIdentifier, ATTAINS.OrganizationIdentifier,
ATTAINS.ParameterName, and ATTAINS.FlagParameterName. Users will need to
complete the crosswalk between ATTAINS.ParameterName and
TADA.ComparableDataIdentifier.

## Details

Users who have already created an ATTAINS parameter and TADA/WQP
characteristic crosswalk can provide it as an input to this function.
The user-supplied crosswalk (dataframe entered into paramRef function
input) must contain the two required columns:
TADA.ComparableDataIdentifier and ATTAINS.ParameterName. In addition,
users who are interested in performing analyses for more than one
organization (multiple states and/or tribes) also need to include an
additional column name: 'ATTAINS.OrganizationIdentifier'. This ensures
that the crosswalk between TADA.ComparableDataIdentifier and
ATTAINS.ParameterName is specific and accurate for each organization. If
a crosswalk has already been created in the past and is entered into
this function as a starting point, then any
TADA.ComparableDataIdentifiers that were previously matched with ATTAINS
parameters will be retained in the crosswalk, and any new
TADA.ComparableDataIdentifiers from the new input data frame will be
added to the crosswalk. Users can then focus on matching only the new
TADA.ComparableDataIdentifiers with applicable ATTAINS parameter names.

Future development efforts may allow users to pull in magnitude values
for an ATTAINS parameter through the Criteria Search Tool depending on a
users quality control and review of these metrics. The EPA TADA team
created a draft crosswalk between characteristic names
(TADA.ComparableDataIdentifier) and EPA 304A pollutant names (sourced
from the Criteria Search Tool:
https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa)
This crosswalk only includes priority characteristics identified by the
TADA Working Group. You are welcome to reach out to the TADA team to ask
for additional matches to be included. You may run the following line of
code in the console to review this crosswalk: 'TADAPriorityChar \<-
utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv",
package = "EPATADA"))'.

If no existing ATTAINS parameter name corresponds with a specific
TADA.ComparableDataIdentifier, users may contact the ATTAINS helpdesk
<attains@epa.gov> to inquire about adding the parameter. Users are free
to use any ATTAINS parameter name found in the ATTAINS parameter domain
value list, even if the parameter name has not previously been listed as
a cause by the specific organization in the past. The full list of
ATTAINS parameter names can be found by downloading the ATTAINS Domains
Excel file:
https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
In the meantime, users can proceed by overriding the data validation in
Excel by value pasting. In that case, users will be warned in the
ATTAINS.FlagParameterName column that they choose to include an
ATTAINS.ParameterName that was not used by the selected organization in
prior ATTAINS assessment cycles.

## Examples

``` r
if (FALSE) { # \dontrun{
# This creates a blank paramRef template of UT Nutrients data.
# Users will need to fill this template out.
# Uncomment example below to generate Excel file
# (we recommended working on this in Excel):
# TADA_ParametersForAnalysis(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = TRUE)
# Example below generates the same output as a dataframe
paramRef_UT <- TADA_ParametersForAnalysis(
  Data_Nutrients_UT,
  org_id = "UTAHDWQ", auto_assign = "None", excel = FALSE
)
# Users can choose to edit the paramRef_UT through the R environment or in
# the excel spreadsheet. Users should be aware that any updates done only
# in the R environment will not reflect the 'ATTAINS.FlagParameterName' values
# correctly. If completed in R, we recommend users rerun this function
# to update the 'ATTAINS.FlagParameterName'.
# See below for a simple example of this workflow:

# Manually add ATTAINS parameters to crosswalk using R
paramRef_UT2 <- dplyr::mutate(paramRef_UT,
  ATTAINS.ParameterName = dplyr::case_when(
    grepl("AMMONIA", TADA.ComparableDataIdentifier) ~ "AMMONIA, TOTAL",
    grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITRATE",
    grepl("NITROGEN", TADA.ComparableDataIdentifier) ~
      "NITRATE/NITRITE (NITRITE + NITRATE AS N)"
  )
)
# Update the 'ATTAINS.FlagParameterName' values
paramRef_UT3 <- TADA_ParametersForAnalysis(Data_Nutrients_UT,
  paramRef = paramRef_UT2,
  org_id = "UTAHDWQ", auto_assign = "None", excel = FALSE
)

# How does auto_assign = "All" compare to paramRef_UT3?
paramRef_UT4 <- TADA_ParametersForAnalysis(Data_Nutrients_UT,
  org_id = "UTAHDWQ", auto_assign = "All", excel = FALSE
)

# Example where multiple org_id's are selected
# Retrieve data
shepherdstown <- TADA_DataRetrieval(
  startDate = "2022-01-01",
  endDate = "2025-12-31",
  huc = "02070004",
  applyautoclean = TRUE,
  ask = FALSE
)
# First, run key flag functions and harmonize synonyms across
# characteristic, fraction, and speciation columns
shepherdstown2 <- TADA_RunKeyFlagFunctions(shepherdstown, clean = TRUE)
shepherdstown3 <- TADA_HarmonizeSynonyms(shepherdstown2)
# Create ATTAINS parameter crosswalk for MD, VA, and PA
paramRef_shepherdstown <- TADA_ParametersForAnalysis(shepherdstown3,
  org_id = c("MDE_EASP", "21VASWCB", "21PA"),
  auto_assign = "All",
  excel = FALSE
)
} # }
```
