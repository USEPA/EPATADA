% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ATTAINSCrosswalks.R
\name{TADA_CreateParamRef}
\alias{TADA_CreateParamRef}
\title{Create or Update ATTAINS, TADA/WQP/WQX, and EPA Criteria Search Tool (CST)
Parameter Name Crosswalk}
\usage{
TADA_CreateParamRef(
  .data,
  org_id = NULL,
  paramRef = NULL,
  excel = TRUE,
  overwrite = FALSE
)
}
\arguments{
\item{.data}{A TADA dataframe. We recommend running all desired data
cleaning, processing, harmonization, filtering, QAQC, and handling of
censored data prior to running TADA_CreateParamRef.}

\item{org_id}{The ATTAINS organization identifier must be supplied by the
user. A list of organization identifiers can be found by downloading
the ATTAINS Domains Excel file:
https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
Organization identifiers are listed in the "OrgName" tab.
The "code" column contains the organization identifiers that
should be used for this parameter. If a user supplied crosswalk is entered
into paramRef AND a user does not provide an org_id argument,
the function can identify which organization identifier(s) to include
based on the unique ATTAINS organization identifiers found in the dataframe.}

\item{paramRef}{A dataframe which contains a completed crosswalk between
TADA_ComparableDataIdentifier and ATTAINS.ParameterName. Users will need to
ensure this crosswalk contains the appropriate column names in order to
run the function. paramRef must contain at least these two column names:
TADA.ComparableDataIdentifier and ATTAINS.ParameterName. Users who are
interested in performing analyses for more than
one organization (multiple states or tribes, or a single state/tribe and
EPA 304a criteria) also need to include an additional column name:
'organization_identifier'.}

\item{excel}{A Boolean value that returns an excel spreadsheet if
excel = TRUE. This spreadsheet is created in the user's downloads folder path.
If needed, please type the following into your R console:
file.path(Sys.getenv("USERPROFILE"), "Downloads") to ensure the file is downloaded
to the correct location. The file will be named "myfileRef.xlsx".}

\item{overwrite}{A Boolean value that ensures the function will not overwrite
the user supplied crosswalk entered into this function via the paramRef
function input. This helps prevent users from overwriting their progress.}
}
\value{
A excel file or data frame which contains the columns:
TADA.ComparableDataIdentifier, organization_identifier, EPA304A.PollutantName,
ATTAINS.ParameterName, and ATTAINS.FlagParameterName. Users will need to
complete the crosswalk between ATTAINS.ParameterName and
TADA.ComparableDataIdentifier.
}
\description{
Use this function to help generate a crosswalk between each
ATTAINS.ParameterName used by a specific state or tribal nation and each
TADA.ComparableDataIdentifier present in the input TADA dataframe. The
crosswalk can be filled out by users within R or Excel. By default this
function will generate a user friendly Excel spreadsheet that includes a
drop down list list of all ATTAINS parameters that are applicable to the
organization selected by the function input 'org_id'. It also
highlights the cells in which users should input information. The excel
spreadsheet will be automatically downloaded to a user's downloads folder path.
Users may need to insert additional rows into the crosswalk if:
\enumerate{
\item an ATTAINS.ParameterName corresponds with multiple TADA.ComparableDataIdentifiers
Example: An organization uses "ALUMINUM" for all aluminum related parameter causes
but this ATTAINS.ParameterName may crosswalk to "ALUMINUM_TOTAL_NA_UG/L"
for one use and "ALUMINUM_DISSOLVED_NA_UG/L" for another use; or
\item an TADA.ComparableDataIdentifiers corresponds with multiple ATTAINS.ParameterNames.
Example: An organization uses both "pH, HIGH" and "pH, LOW" as ATTAINS.ParameterNames,
but both crosswalk to the same TADA.ComparableDataIdentifier, "PH_NA_NA_STD UNITS".
}
}
\details{
Users who have already created an ATTAINS parameter and TADA/WQP characteristic
crosswalk can provide it as an input to this function. The user-supplied
crosswalk (dataframe entered into paramRef function input) must contain the
two required columns: TADA.ComparableDataIdentifier and ATTAINS.ParameterName.
In addition, users who are interested in performing analyses for more than
one organization (multiple states or tribes, or a single state/tribe and
EPA 304a criteria) also need to include an additional column name:
'organization_identifier'. This ensures that the crosswalk between
TADA.ComparableDataIdentifier and ATTAINS.ParameterName are specific and
accurate for each organization. If a crosswalk has already been created in the
past and is entered into this function as a starting point, then any
TADA.ComparableDataIdentifiers that were previously matched
with ATTAINS parameters will be retained in the crosswalk, and any new
TADA.ComparableDataIdentifiers from the new input data frame will be added
to the crosswalk. Users can then focus on matching only the new
TADA.ComparableDataIdentifiers with applicable ATTAINS parameter names.

The EPA TADA team created a draft crosswalk between characteristic
names (TADA.ComparableDataIdentifier) and EPA 304A pollutant names
(sourced from the Criteria Search Tool:
https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa)
This crosswalk only includes priority characteristics identified by the TADA
Working Group. You are welcome to reach out to the TADA team to ask for
additional matches to be included. You may run the following line of code
in the console to review this crosswalk:
'CSTtoATTAINSParamCrosswalk <- utils::read.csv(system.file("extdata", "TADAPriorityCharUnitRef.csv", package = "EPATADA"))'.

If no existing ATTAINS parameter name corresponds with a specific
TADA.ComparableDataIdentifier, users may contact the ATTAINS helpdesk
\email{attains@epa.gov} to inquire about adding the parameter. Users are
free to use any ATTAINS parameter name found in the ATTAINS parameter domain
value list, even if the parameter name  has not previously
been listed as a cause by the specific organization in the
past. The full list of ATTAINS parameter names can be found by downloading
the ATTAINS Domains Excel file:
https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
In the meantime, users can proceed by overriding the data validation in Excel
by value pasting. In that case, users will be warned in the
ATTAINS.FlagParameterName column that they choose to include an
ATTAINS.ParameterName that was not used by the selected organization in prior
ATTAINS assessment cycles.
}
\examples{
\dontrun{
# This creates a blank paramRef template of UT Nutrients data.
# Users will need to fill this template out.
# Uncomment example below to generate Excel file
# (we recommended working on this in Excel):
# TADA_CreateParamRef(Data_Nutrients_UT, org_id = "UTAHDWQ", excel = TRUE)
# Example below generates the same output as a dataframe
paramRef_UT <- TADA_CreateParamRef(
  Data_Nutrients_UT,
  org_id = "UTAHDWQ", excel = FALSE
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
paramRef_UT3 <- TADA_CreateParamRef(Data_Nutrients_UT,
  paramRef = paramRef_UT2,
  org_id = "UTAHDWQ", excel = FALSE
)

# Example where multiple org_id's are selected
# First, run key flag functions and harmonize synonyms across
# characteristic, fraction, and speciation columns
Data_NCTCShepherdstown <-
  TADA_RunKeyFlagFunctions(Data_NCTCShepherdstown_HUC12)
Data_NCTCShepherdstown2 <- TADA_HarmonizeSynonyms(Data_NCTCShepherdstown)
# Create ATTAINS parameter crosswalk for MD, VA, and PA
paramRef_NCTC <- TADA_CreateParamRef(Data_NCTCShepherdstown2,
  org_id =
    c("MDE_EASP", "21VASWCB", "21PA"), excel = FALSE
)
}

}
