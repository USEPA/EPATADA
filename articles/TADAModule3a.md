# TADA Module 3A: Associating WQP Characteristic Names with Applicable ATTAINS or EPA 304(a) Criteria Search Tool (CST) Parameter and Use Names

## Welcome!

Thank you for your interest in Tools for Automated Data Analysis (TADA).
TADA is an open-source tool set built in the R programming language. The
EPATADA R Package is still under development. New functionality is added
weekly, and sometimes we need to make bug fixes in response to user
feedback. We appreciate your feedback, patience, and interest in these
helpful tools. If you are interested in contributing to TADA
development, more information is available
[here](https://usepa.github.io/EPATADA/articles/CONTRIBUTING.html). We
welcome collaboration with external partners!

## Install and load packages

First, install and load the remotes package specifying the repo. This is
needed before installing the EPATADA R package because it is only
available on GitHub.

``` r
install.packages("remotes",
  repos = "http://cran.us.r-project.org"
)
library(remotes)
```

Next, install and load the EPATADA R Package using the remotes package.
Dependency packages will also be downloaded automatically from CRAN. You
may be prompted in the console to update dependencies that have more
recent versions available. If you see this prompt, it is recommended to
update all of them (enter 1 into the console).

``` r
remotes::install_github("USEPA/EPATADA",
  ref = "develop",
  dependencies = TRUE
)
```

Finally, use the **library()** function to load the TADA R Package into
your R session.

``` r
library(EPATADA)
```

All EPATADA R package functions have their own individual help pages
(see [Function
Reference](https://usepa.github.io/EPATADA/reference/index.html)). Users
can also access function help pages from RStudio by entering
`?[name of TADA function]` into the console (example below).

``` r
# Access help page for TADA_DataRetrieval
?TADA_DataRetrieval
```

## Introduction to TADA Module 3

This [RMarkdown](https://yihui.org/rmarkdown/) document walks users
through how to create WQP and ATTAINS crosswalks that are needed prior
to defining and capturing organization specific water quality analysis
criteria and methodologies. It is the first of several vignettes that
are being developed as part of TADA Module 3.

Specifically, this vignette provides an overview of two functions that
can assist users with:

- Creating a crosswalk (reference table) between ATTAINS parameter names
  and WQP/TADA characteristic names.

- Creating a crosswalk (reference table) of all unique combinations of
  ATTAINS uses and parameters applicable to ATTAINS Assessment Units of
  interest.

These Module 3 functions are being designed with the flexibility for
users from states, tribes, or territories to input organization-specific
information (and link directly to information in ATTAINS). While TADA
functions help generate these crosswalks, users must review and modify
the tables generated in each step of the process to ensure their
accuracy. Additionally, the TADA team has also incorporated national
recommended Clean Water Act (CWA) 304(a) numeric criteria for optional
use in Module 3 functions by leveraging data from EPA’s [Criteria Search
Tool
(CST)](https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa).
In the next vignette, TADAModule3_PartB , which is coming soon, this
will allow users to analyze their data against the 304(a) criteria; and
to easily compare results when when using 304(a) criteria vs. their own
organization’s criteria.

## Example data from WQP

Let’s start with an example data frame that has gone through data
cleaning, wrangling, harmonization, handling of censored data, removal
of suspect results, and other important TADA Module 1 functions. This
process should be completed by users before utilizing Module 3
functions. The example TADA data frame used in this vignette
(Data_HUC8_02070004_Mod1Output) has already been through this process.

``` r
# import example data set (output of Module 1 Workflow)
Data_NCTC <- Data_HUC8_02070004_Mod1Output
```

This example includes results from multiple states. In the following
sections, for demonstration purposes, we will focus only on Maryland.
The remainder of this vignette will walk through how to fill out two
crosswalk tables (TADA_ParametersForAnalysis and TADA_UsesForAnalysis)
that are needed before we can start assigning applicable criteria and
methodologies information for this specific ATTAINS organization
(Maryland).

## ATTAINS Domains and Allowable Values

A crosswalk between TADA characteristics and ATTAINS parameters is
needed before we can integrate information from these two datasets.
Before creating the TADA and ATTAINS parameter crosswalk table
(TADA_ParametersForAnalysis), let’s review unique
TADA.ComparableDataIdentifier’s (characteristic, fraction and speciation
combinations) in the example data. How many results are available for
each within the example data set (Data_NCTC)?

``` r
# create table with counts of TADA.ComparableDataIdentifiers
TADA_FieldValuesTable(Data_NCTC, field = "TADA.ComparableDataIdentifier")
```

In this vignette, we will crosswalk each of these
TADA.ComparableDataIdentifier’s to ATTAINS parameter names. ATTAINS has
multiple domains, including the parameter names domain, that have
allowable values. Using a function from rExpertQuery, which provides
functions for downloading tidy data from the ATTAINS public web services
(<https://github.com/USEPA/rExpertQuery>), we can review all ATTAINS
domains (see example below).

``` r
# return ATTAINS domain names
rExpertQuery::EQ_DomainValues()
```

ParameterName is one of the ATTAINS domains that has a specific list of
allowable values. Keep in mind that the ATTAINS parameters domain list
only includes parameters that have been listed as causes (of water
quality impairment) in prior ATTAINS CWA assessment cycles. Let’s review
the complete list:

``` r
# return ATTAINS parameter domain values
TADA_TableExport(rExpertQuery::EQ_DomainValues("param_name"))
```

In the next section, we will review which parameters have been listed in
ATTAINS in the past for a specific organization. In order to select a
specific organization in the TADA_ParametersForAnalysis() function, we
must first review the ATTAINS organization domain list to get the
ATTAINS organization id’s to use for the function input.

``` r
# return ATTAINS organization domain values
TADA_TableExport(rExpertQuery::EQ_DomainValues("org_id"))
```

## TADA_ParametersForAnalysis() Basics

ATTAINS Parameter Names are more general than the TADA Comparable Data
Identifiers. Therefore, we recommend users manually provide a crosswalk.
TADA_ParametersForAnalysis() can help generate this crosswalk, but if
desired, users can provide a pre-filled crosswalk with the required
column names (“TADA.ComparableDataIdentifier” and
“ATTAINS.ParameterName”). TADA_ParametersForAnalysis() includes an
argument input ‘auto_assign’ to assist users in doing an exact match
between TADA Characteristic Names to ATTAINS Parameter Name. There are
roughly 598/1180 unique ATTAINS Parameter Names that have an exact
match. \*\*It is important to note that even with these exact matches,
there are additional complexities related to the fraction, speciation
and units that may be unique to your organization. For example, some
organizations may consider any of Nitrate/Nitrite, Ammonia as “Nitrogen”
as the ATTAINS.ParameterName and may not consider the speciation.

Let’s go through a few examples.

TADA_ParametersForAnalysis() creates a template which includes all
TADA.ComparableDataIdentifiers from the TADA data frame and provides a
blank column, “ATTAINS.ParameterName” for users to input the
corresponding ATTAINS parameter name.

Setting “excel = TRUE” in TADA_ParametersForAnalysis tells the function
to create an Excel spreadsheet of the reference table for easy user
review and editing. The excel output includes logic to help a user
select ATTAINS parameter names that have been used in the past by the
selected organization. If users do not want to work in Excel, they can
use “excel = FALSE” to return a data frame. The data frame can be edited
directly in R if desired, although there are no TADA-specific functions
designed to facilitate this.

When using TADA_ParametersForAnalysis(), users should specify the
organization(s) of interested in the “org_names” argument. This ensures
that the correct number of rows will be created in the reference table.
When “excel = TRUE”, specifying the organization(s) also creates a
separate tab in the Excel file which contains the parameter names used
in prior assessment cycles by the specified ATTAINS organization(s).
This will allow users to decide whether to continue to use the same
ATTAINS parameter names from prior assessments that their
organizations(s) have used in the past, or to use other valid parameter
names from the entire ATTAINS domain list. If there is not a suitable
ATTAINS parameter name to match a TADA.ComparableDataIdentifier, users
should contact the ATTAINS team (attains@epa.gov) for assistance.

For our first example, let’s start of with a single organization. In
this example TADA data frame, this spatial area consists of WQP data
from several different states. We will specify Maryland “MDE_EASP” as
our ATTAINS organization identifier of interest. By default, auto_assign
= “None” which will not populate any entries for the
ATTAINS.ParameterName column, and users will need to manually fill this
crosswalk out.

``` r
# create TADA parameter reference table for specified organization
NCTC_ParamRef_None <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  auto_assign = "None",
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)

# export TADA parameter reference table as csv
TADA_TableExport(NCTC_ParamRef_None)
```

Now, let’s go through an example in which users auto-populate the
crosswalk with exact matches. We will use auto_assign = ‘All’ to
indicate we will use exact matches regardless of if your organization
has used this “ATTAINS.ParameterName” domain value in the past. i.e.
Your organization is fine with using an ATTAINS Parameter Name that has
been submitted to ATTAINS outside of your organization.

``` r
# create TADA parameter reference table for specified organization
NCTC_ParamRef_All <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  auto_assign = "All",
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)

# export TADA parameter reference table as csv
TADA_TableExport(NCTC_ParamRef_All)
```

We can also use auto_assign = ‘Org’ to indicate we will use exact
matches ONLY if your organization has used this “ATTAINS.ParameterName”
domain value in the past.

``` r
# create TADA parameter reference table for specified organization
NCTC_ParamRef_Org <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  auto_assign = "Org",
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)

# export TADA parameter reference table as csv
TADA_TableExport(NCTC_ParamRef_Org)
```

The code chunk below demonstrates how the parameter reference table can
be modified in R. Once modifications are complete, we can re-run
TADA_ParametersForAnalysis using the modified parameter reference table
as the input for “paramRef”. This will update the
ATTAINS.FlagParameterName column in the parameter reference table (see
NCTC_ParamRef2 in example below). This column provides information about
whether or not the organization specified in the
“ATTAINS.OrganizationIdentifier” column has listed this parameter as a
cause in prior assessment cycles. It also flags rows where the
TADA.ComparableDataIdentifier as not been assigned an ATTAINS parameter
name.

``` r
# only run the code chunks below if you make edits to the excel file. Please note you must open and save the excel file at least once to reflect all Excel formula based values.

# downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "myfileRef.xlsx")
# ParamRef <- openxlsx::read.xlsx(downloads_path, sheet = "CreateParamRef")

# NCTC_ParamRef2 <- TADA_ParametersForAnalysis(
#   Data_NCTC,
#   org_names = c("MDE_EASP"),
#   paramRef = ParamRef,
#   excel = FALSE
#   # uncomment excel = TRUE, overwrite = TRUE to run the excel file
#   # excel = TRUE, overwrite = TRUE
#   )

ParamRef_Manual <- dplyr::mutate(NCTC_ParamRef_None, ATTAINS.ParameterName = dplyr::case_when(
  grepl("PH", TADA.ComparableDataIdentifier) ~ "PH, HIGH",
  TADA.ComparableDataIdentifier == "ZINC_DISSOLVED_NA_UG/L" ~ "ZINC",
  grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITROGEN, TOTAL"
))

NCTC_ParamRef_Manual <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  paramRef = ParamRef_Manual,
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)
TADA_TableExport(NCTC_ParamRef_Manual)
```

We can now save and reuse NCTC_ParamRef_Manual as the paramRef argument
input in TADA_ParametersForAnalysis(). This is useful to go through a
review process if new WQX characteristic names are added as a domain
value.

``` r
NCTC_ParamRef_AutoCreate <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  paramRef = NCTC_ParamRef_Manual,
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)
TADA_TableExport(NCTC_ParamRef_AutoCreate)
```

Your ‘paramRef’ is prioritized over the ‘auto_assign’ argument inputs.
However, auto_assign will fill in any remaining blanks that you have not
filled in. For example, this ‘paramRef’ argument input that was provided
matched ‘Nitrate’ to ‘NITROGEN, TOTAL’ rather than ‘NITRATE’, thus even
if you specified ‘auto_assign = ’All’, this crosswalk will prioritize
‘NITROGEN, TOTAL’ as the ATTAINS.ParameterName. However, by specifying
auto_assign = ‘All’ for any values that have not been crosswalk,
TADA_ParametersForAnalysis() will fill those if there is an exact match,
in the example below, no crosswalk of pH in your ParamRef2 was provided,
so NCTC_ParamRef_Manual2 will crosswalk the TADA Characteristic name to
‘PH’ as the exact match for ATTAINS parameter name.

``` r
ParamRef2 <- dplyr::mutate(NCTC_ParamRef_None, ATTAINS.ParameterName = dplyr::case_when(
  # grepl("PH", TADA.ComparableDataIdentifier) ~ "PH",
  TADA.ComparableDataIdentifier == "ZINC_DISSOLVED_NA_UG/L" ~ "ZINC",
  grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITROGEN, TOTAL"
))

NCTC_ParamRef_Final <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  paramRef = ParamRef2,
  auto_assign = "All",
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)
TADA_TableExport(NCTC_ParamRef_Final)
```

Once the parameter reference table has been reviewed and modified, it
can be saved and reused for analysis of future TADA data frames.

## TADA_UsesForAnalysis() Basics

Our first example will pull in all prior ATTAINS uses (ATTAINS.UseNames)
and parameter names (ATTAINS.ParameterName) specific to the specified
ATTAINS organization (defined by the org_names function argument for
TADA_UsesForAnalysis), which in this case is “MDE_EASP” (Maryland).
Users will review the output and choose which ATTAINS.UseName’s are
applicable to their analysis. The default functionality is to “Include”
all ATTAINS use_names (see the “IncludeOrExclude” column) that has an
ATTAINS.UseName associated with it. Later in the data analysis process,
users will be asked to define criteria and methodologies that is
applicable to each ATTAINS use_name and parameter name labeled as
“Include”. If a ATTAINS.UseName is not applicable, users should choose
“Exclude” for that ATTAINS parameter name and use name.

For any ATTAINS parameter name(s) that have not been used by an
organization in prior assessment cycles, there will be no prior use
names associated with them. In this case, users will be responsible for
manually assigning the appropriate ATTAINS.UseName applicable to their
org under the column ‘ATTAINS.UseName’, and for inputting additional
rows as needed if the parameter applies to multiples uses. Users can
also choose to ‘auto_assign’ all unique ATTAINS.UseName by
ATTAINS.OrganizationName to any ATTAINS.ParameterName missing a use name
associated with it.

In the example below, we can see only “ZINC” and “NITROGEN, TOTAL” were
the ATTAINS.ParameterNames assessed in prior assessment cycles for
MDE_EASP (see ATTAINS.FlagUseName column in NCTC_usesRef). “PH” was not
listed in ATTAINS for MDE_EASP in the prior assessment cycle, but was
included in MDE_EASP paramRef argument input.

``` r
NCTC_usesRef_Manual <- TADA_UsesForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  paramRef = NCTC_ParamRef_Final,
  auto_assign = FALSE,
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)
```

If desired, a user can manually assign this parameter to any applicable
uses (disclaimer: this is for demonstration purposes only and does not
reflect MDE_EASP’s process).

``` r
add_data <- data.frame(
  "ATTAINS.OrganizationIdentifier" = "MDE_EASP",
  "ATTAINS.ParameterName" = rep("PH", 3),
  "ATTAINS.UseName" = c(
    "Aquatic Life and Wildlife", "Water Contact Sports",
    "Seasonal Migratory Fish Spawning and Nursery Subcategory"
  )
)

# The output of this will not reflect changes to the ATTAINS.FlagUseName column. To do so, we need to re run TADA_UsesForAnalysis() with usesRef = add_data as an argument input.
usesRef <- NCTC_usesRef_Manual |>
  dplyr::left_join(add_data, by = c("ATTAINS.OrganizationIdentifier", "ATTAINS.ParameterName"), keep = FALSE) |>
  dplyr::mutate(ATTAINS.UseName = dplyr::coalesce(ATTAINS.UseName.x, ATTAINS.UseName.y)) |>
  dplyr::select(-c(ATTAINS.UseName.x, ATTAINS.UseName.y)) |>
  dplyr::mutate(IncludeOrExclude = "Include")

# PH will now reflect the changes
NCTC_usesRef_Manual_Update <- TADA_UsesForAnalysis(
  Data_NCTC,
  paramRef = NCTC_ParamRef_Final,
  usesRef = usesRef, # Edits were made to usesRef, updates flag column
  org_id = c("MDE_EASP"),
  auto_assign = FALSE,
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)

TADA_TableExport(NCTC_usesRef_Manual_Update)
```

We can choose to assign all unique use names found by your organization
to those ATTAINS.ParameterName without any associated ATTAINS.UseName.

``` r
NCTC_usesRef_AutoAssign <- TADA_UsesForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  paramRef = NCTC_ParamRef_Final,
  auto_assign = TRUE,
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)
TADA_TableExport(NCTC_usesRef_AutoAssign)
```

### Multiple Organization TADA_ParametersForAnalysis and TADA_UsesForAnalysis Workflow

If a user includes wants to include additional organizations beyond
“MDE_EASP” , they will need to have defined those organizations in
TADA_ParametersForAnalysis with the org_id argument input. We will
create another example paramRef from TADA_ParametersForAnalysis that
would allow us to pull in this information for more than one
organization. We will also run TADA_UsesForAnalysis for multiple
organizations using autoassign = TRUE.

``` r
# create TADA parameter reference table for specified organization
NCTC_ParamRef_MultipleOrgs <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP", "21VASWCB", "21PA", "WVDEP"),
  auto_assign = "Org",
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)

ParamRef_MultipleOrgs <- dplyr::mutate(NCTC_ParamRef_MultipleOrgs, ATTAINS.ParameterName = dplyr::case_when(
  grepl("PH", TADA.ComparableDataIdentifier) ~ "PH",
  TADA.ComparableDataIdentifier == "ZINC_DISSOLVED_NA_UG/L" ~ "ZINC",
  grepl("NITRATE", TADA.ComparableDataIdentifier) ~ "NITROGEN, TOTAL"
))

# We must refresh the flagging columns as ParamRef_MultipleOrgs was modified in RStudio and not directly in Excel.
NCTC_ParamRef_MultipleOrgs_Final <- TADA_ParametersForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP"),
  paramRef = ParamRef_MultipleOrgs,
  auto_assign = "None"
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)

NCTC_usesRef_MultipleOrgs_AutoAssign <- TADA_UsesForAnalysis(
  Data_NCTC,
  org_id = c("MDE_EASP", "21VASWCB", "21PA", "WVDEP"),
  paramRef = NCTC_ParamRef_MultipleOrgs_Final,
  auto_assign = TRUE,
  excel = FALSE
  # uncomment excel = TRUE, overwrite = TRUE to run the excel file
  # excel = TRUE, overwrite = TRUE
)
TADA_TableExport(NCTC_usesRef_MultipleOrgs_AutoAssign)
```

## Next steps

Stay tuned! Future vignettes aim to assist with next steps in the
analysis process, including:

- Defining the water quality criteria and methodologies used for each
  unique combination of ATTAINS assessment unit, use, and parameter.

- Summarizing results by: (1) individual magnitude excursions.

Disclaimer: The EPATADA Module 3 functions are under active development.
EPATADA functions do not constitute current EPA recommendations, policy
or regulatory requirements. Organizations may optionally choose to use
EPATADA as a a tool in their process. Use of EPATADA is not required.
