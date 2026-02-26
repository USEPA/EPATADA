# TADA Module 3: Alternative Options to Generate the Criteria and Methodology template for User Input

### Welcome!

Thank you for your interest in Tools for Automated Data Analysis (TADA).
TADA is an open-source tool set built in the R programming language.
This [RMarkdown](https://bookdown.org/yihui/rmarkdown/) document walks
users through how to download the EPATADA R package from GitHub, access
and parameterize several important functions, and create basic
visualizations with a sample data set.

**Note: EPATADA is still under development. New functionality is added
weekly, and sometimes we need to make bug fixes in response to tester
and user feedback. We appreciate your feedback, patience, and interest
in these helpful tools.**

**If you are interested in contributing to EPATADA development, more
information is available at:**

[**Contributing**](https://usepa.github.io/EPATADA/articles/CONTRIBUTING.html)

**We welcome collaboration with external partners.**

### Install and load packages

First, install and load the remotes package specifying the repo. This is
needed before installing EPATADA because it is only available on GitHub.

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

### Help pages

All TADA R package functions have their own individual help pages,
listed on the [Function
reference](https://usepa.github.io/EPATADA/reference/index.html) page on
the GitHub site. Users can also access the help page for a given
function in R or RStudio using the following format (example below):
`?[name of TADA function]`

``` r
# Access help page for TADA_DataRetrieval
?TADA_DataRetrieval
```

### Module 3 Functions in TADA

Disclaimer: The EPATADA Module 3 functions were designed to: (1) assist
users with associating Water Quality Portal monitoring locations with
assessment units and designated uses from ATTAINS and (2) compare Water
Quality Portal results with numeric water quality criteria. EPATADA
functions do not constitute current EPA policy or regulatory
requirements. Organizations may choose to use EPATADA as a a tool in
their decision making processes. Use of EPATADA is not required.

### Get WQP Monitoring Data in Montana Using TADA_DataRetrieval()

Get bacteria and pH data from Missoula County, Montana.

``` r
# get MT data
tada.MT <- TADA_DataRetrieval(
  startDate = "2020-01-01",
  endDate = "2022-12-31",
  statecode = "MT",
  characteristicName = c(
    "Escherichia",
    "Escherichia coli",
    "pH"
  ),
  countycode = "Missoula County",
  ask = FALSE
)
```

    ## [1] "Downloading WQP query results. This may take some time depending upon the query size."
    ## $statecode
    ## [1] "US:30"
    ## 
    ## $startDate
    ## [1] "2020-01-01"
    ## 
    ## $countycode
    ## [1] "Missoula County"
    ## 
    ## $characteristicName
    ## [1] "Escherichia"      "Escherichia coli" "pH"              
    ## 
    ## $endDate
    ## [1] "2022-12-31"
    ## 
    ## [1] "Data successfully downloaded. Running TADA_AutoClean function."
    ## [1] "TADA_Autoclean: creating TADA-specific columns."
    ## [1] "TADA_Autoclean: handling special characters and coverting TADA.ResultMeasureValue and TADA.DetectionQuantitationLimitMeasure.MeasureValue value fields to numeric."
    ## [1] "TADA_Autoclean: converting TADA.LatitudeMeasure and TADA.LongitudeMeasure fields to numeric."
    ## [1] "TADA_Autoclean: harmonizing synonymous unit names (m and meters) to m."
    ## [1] "TADA_Autoclean: updating deprecated (i.e. retired) characteristic names."

    ## [1] "TADA_Autoclean: harmonizing result and depth units."
    ## [1] "TADA_Autoclean: creating TADA.ComparableDataIdentifier field for use when generating visualizations and analyses."
    ## [1] "NOTE: This version of the TADA package is designed to work with numeric data with media name: 'WATER'. TADA_AutoClean does not currently remove (filter) data with non-water media types. If desired, the user must make this specification on their own outside of package functions. Example: dplyr::filter(.data, TADA.ActivityMediaName == 'WATER')"

``` r
# clean up data set (minimal)
tada.MT.clean <- tada.MT |>
  TADA_RunKeyFlagFunctions() |>
  TADA_SimpleCensoredMethods() |>
  TADA_HarmonizeSynonyms()
```

    ## [1] "TADA_FlagFraction: Rows with Suspect sample fractions have been flagged but retained. Review these rows using the TADA.SampleFraction.Flag column before proceeding and/or set clean = TRUE."
    ## [1] "TADA_FlagSpeciation: Rows with Suspect speciations have been flagged but retained. Review these rows using the new TADA.MethodSpeciation.Flag column before proceeding and/or set clean = 'suspect_only' or 'both'."
    ## [1] "TADA_FlagMeasureQualifierCode: Dataframe does not include any information (all NAs) in MeasureQualifierCode."
    ## [1] "TADA_IDCensoredData: No censored data detected in your dataframe. Returning input dataframe with new column TADA.CensoredData.Flag set to Uncensored"
    ## [1] "Cannot apply simple censored methods to dataframe with no censored data results. Returning input dataframe."
    ## [1] "Warning: Your dataframe contains suspect metadata combinations in the following flag columns:"
    ##                Flag_Column Result Count
    ## 1 TADA.SampleFraction.Flag          135

``` r
# remove intermediate objects
rm(tada.MT)

# or uncomment the code below and load internal copy of TADA df from EPATADA
# tada.MT.clean <- Data_MT_MissoulaCounty
```

## Defining Criteria - Magnitude Methodology

Users can proceed with a few different options for generating their
criteria table

1.  Generate a blank criteria and methods table and fill it out from
    scratch.

2.  Provide a user-supplied criteria and methods table partially/fully
    filled out.

    A.) The default option in this scenario will display all unique
    **TADA.ComparableDataIdentifiers** (or WQP CharacteristicName) in
    your TADA/WQP data frame to ensure you review any missing WQP
    Characteristic, speciation and fraction combinations.

    B.) Alternatively, users can choose to display all unique **TADA
    Characteristic name** rather than TADA.ComparableDataIdentifier. In
    this scenario, each ATTAINS.ParameterName in the analysis summary
    output will be grouped to any of these TADA/WQP CharacteristicName,
    unless a fraction or speciation is defined.

3.  Users can also choose to provide an autofill option, which will help
    to fill out any missing rows with ATTAINS.ParameterName and
    ATTAINS.UseName that are pulled in from ATTAINS as the default.

    A.) If a user has supplied a list of new or updated use names to AU
    that may not be retrievable from the prior ATTAINS assessment cycle,
    they should provide a AU_UsesRef crosswalk table in this function.
    This should only be provided when auto_assign = TRUE.

4.  (Recommended) Go through the step-by-step review process with the 3
    TADA crosswalk reference file generation for
    TADA_ParametersForAnalysis, TADA_UsesForAnalysis, and
    TADA_MLSummaryRef. This vignette does not go through this
    recommended workflow. Please see ExampleMod3Workflow.Rmd for this
    guided workflow.

Each option will allow the option to append additional rows to summarize
EPA304(a) recommended criteria, if one has been defined. To view the
criteria, go to Option B of this vignette. Please contact the TADA team
if you believe there are additional entries or modification to these
defined standards.

### Option A: Fully blank template

A blank template is generated. This can be generated and filled out in
the excel file.

``` r
MT.Criteria.blank <- TADA_DefineCriteriaMethodology()

TADA_TableExport(MT.Criteria.blank)
```

### Option B: View EPA304(a) Recommended Criteria

User can choose to view the full list of EPA304(a) criteria that has
been filled out in TADA format for most priority WQP Characteristic Name
by including “USEPA” as part of the org_id argument input.

``` r
MT.Criteria.epa <- TADA_DefineCriteriaMethodology(
  org_id = "USEPA"
)
```

    ## [1] "TADA_DefineCriteriaMethodology: USEPA was included in your 'org_id': Including EPA304a recommended criteria by each unique TADA.CharacteristicName if one is found."

``` r
TADA_TableExport(MT.Criteria.epa)
```

If a user is only interested in showing TADA.CharacteristicName(s) that
are in their WQP data frame, user can provide their .data in
TADA_DefineCriteriaMethodology. Any EPA304(a) criteria not defined for a
TADA.CharacteristicName(s) will show up as an unfilled row. Users can
choose to fill this value out as desired or leave this row
unfilled/removed.

``` r
MT.Criteria.epa2 <- TADA_DefineCriteriaMethodology(
  .data = tada.MT.clean,
  org_id = "USEPA"
)
```

    ## [1] "NAs were found in ATTAINS.ParameterName. Please ensure that you have inputted all field values of interest in the ATTAINS.ParameterName column generated from TADA_ParametersForAnalysis() function."
    ## [1] "displayNA = FALSE: This MLSummaryRef table will only display parameters and uses for a ML if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
    ## [1] "TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."
    ## [1] "TADA_DefineCriteriaMethodology: USEPA was included in your 'org_id': Including EPA304a recommended criteria by each unique TADA.CharacteristicName if one is found."

``` r
TADA_TableExport(MT.Criteria.epa2)
```

### Option C: Auto Fill option (Intermediate Tabs are Hidden)

You can also generate auto_assigned value(s) of ATTAINS.ParameterName,
ATTAINS.UseName to TADA.CharacteristicName using default options that
are based on prior ATTAINS assessment cycles by each ATTAINS
organization. Users should be aware that this will only return rows for
any matching values from a WQP characteristic to ATTAINS parameter alias
table. It is likely that these value(s) will require a thorough review
process during each step of the process with the recommended workflow of
TADA_ParametersForAnalysis, TADA_UsesForAnalysis and TADA_MLSummaryRef.

``` r
MT.Criteria.auto <- TADA_DefineCriteriaMethodology(
  tada.MT.clean,
  org_id = "MTDEQ",
  auto_assign = TRUE,
  # displayUniqueId = FALSE,
  excel = FALSE
  # uncomment to run the excel file
  # excel = TRUE, overwrite = TRUE
)
```

    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment."
    ## [1] "TADA_ParametersForAnalysis: auto_assign == 'Org' was selected, finding an exact ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_UsesForAnalysis with default assignment."
    ## [1] "TADA_UsesForAnalysis: auto_assign == TRUE was selected, assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_MLSummary with default assignment."
    ## [1] "TADA_MLSummary: displayNA = TRUE: This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
    ## [1] "EQ_DomainValues: For param_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."

    ## TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected.
    ## Finding an alias match between ATTAINS parameter name and Criteria Search Tool (CST) standardized pollutant names.
    ## Finding an alias match between ATTAINS use name and Criteria Search Tool (CST) uses.
    ## If an ATTAINS.ParameterName and ATTAINS.UseName alias was found, populating these rows with the CST magnitude values.
    ## A many-to-many match is likely. User review is needed to ensure the proper parameter and uses from ATTAINS and CST alias crosswalk was accomplished (remove or add rows as needed).

    ## [1] "EQ_DomainValues: For use_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."
    ## [1] "TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'. TADA functions cannot currently handle analysis for these instances."
    ## [1] "TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."

``` r
TADA_TableExport(MT.Criteria.auto)
```

Users who would like to ensure all Characteristic, Speciation and
Fractions are being considered, can use displayUniqueId = TRUE to show
all unique TADA.ComparableDataIdentifier(s) shown as explicit crosswalk.
Note: This may generate many additional rows if your WQP data results
are not harmonized or if there are many different combinations of
Characteristic, Speciation and Fractions to consider.

``` r
MT.Criteria.auto2 <- TADA_DefineCriteriaMethodology(
  tada.MT.clean,
  org_id = "MTDEQ",
  auto_assign = TRUE,
  displayUniqueId = TRUE,
  excel = FALSE
  # uncomment to run the excel file
  # excel = TRUE, overwrite = TRUE
)
```

    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment."
    ## [1] "TADA_ParametersForAnalysis: auto_assign == 'Org' was selected, finding an exact ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_UsesForAnalysis with default assignment."
    ## [1] "TADA_UsesForAnalysis: auto_assign == TRUE was selected, assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_MLSummary with default assignment."
    ## [1] "TADA_MLSummary: displayNA = TRUE: This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
    ## [1] "EQ_DomainValues: For param_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."

    ## TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected.
    ## Finding an alias match between ATTAINS parameter name and Criteria Search Tool (CST) standardized pollutant names.
    ## Finding an alias match between ATTAINS use name and Criteria Search Tool (CST) uses.
    ## If an ATTAINS.ParameterName and ATTAINS.UseName alias was found, populating these rows with the CST magnitude values.
    ## A many-to-many match is likely. User review is needed to ensure the proper parameter and uses from ATTAINS and CST alias crosswalk was accomplished (remove or add rows as needed).

    ## [1] "EQ_DomainValues: For use_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."
    ## [1] "TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'. TADA functions cannot currently handle analysis for these instances."

``` r
TADA_TableExport(MT.Criteria.auto2)
```

We can also choose to append EPA304(a) recommended criteria into the
criteria table for any WQP characteristics in your data frame that are
found by specifying “USEPA” as part of the org_id argument input.

``` r
MT.Criteria.auto3 <- TADA_DefineCriteriaMethodology(
  tada.MT.clean,
  org_id = c("MTDEQ", "USEPA"),
  auto_assign = TRUE,
  displayUniqueId = TRUE,
  excel = FALSE
  # uncomment to run the excel file
  # excel = TRUE, overwrite = TRUE
)
```

    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment."
    ## [1] "TADA_ParametersForAnalysis: More than one org_name was defined in your dataframe. Generating duplicate rows of TADA.ComparableDataIdentifier for each org."
    ## [1] "TADA_ParametersForAnalysis: auto_assign == 'Org' was selected, finding an exact ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_UsesForAnalysis with default assignment."
    ## [1] "TADA_UsesForAnalysis: auto_assign == TRUE was selected, assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_MLSummary with default assignment."
    ## [1] "TADA_MLSummary: displayNA = TRUE: This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
    ## [1] "EQ_DomainValues: For param_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."

    ## TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected.
    ## Finding an alias match between ATTAINS parameter name and Criteria Search Tool (CST) standardized pollutant names.
    ## Finding an alias match between ATTAINS use name and Criteria Search Tool (CST) uses.
    ## If an ATTAINS.ParameterName and ATTAINS.UseName alias was found, populating these rows with the CST magnitude values.
    ## A many-to-many match is likely. User review is needed to ensure the proper parameter and uses from ATTAINS and CST alias crosswalk was accomplished (remove or add rows as needed).

    ## [1] "EQ_DomainValues: For use_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."
    ## [1] "TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'. TADA functions cannot currently handle analysis for these instances."
    ## [1] "TADA_DefineCriteriaMethodology: USEPA was included in your 'org_id': Including EPA304a recommended criteria by each unique TADA.CharacteristicName if one is found."

``` r
TADA_TableExport(MT.Criteria.auto3)
```

### Option D: User Supplied Table

A user has a completed (or partially) filled criteria file, let’s use
MTDEQ as our example org. MTDEQ should thoroughly review this table and
determine if there are values that needs to be fixed or if there are
missing WQP Char to consider that isn’t defined in their criteria and
methods table that they have supplied. Users will be warned how many WQP
Char values are not defined from their user supplied table.

In this first example, a user supplies their own criteria table.

Note: If a user has an updated list of use names that have been applied
to an assessment unit, they should also provide a AU_UsesRef input.
Otherwise the uses will be pulled in from the prior ATTAINS assessment
cycle.

``` r
# Load the example R8 criteria table
criteria_table <- system.file("extdata", "criteria_table.rda", package = "EPATADA")
load(criteria_table)
# Load example uses to AU Ref table
utils::data(Data_MT_AU_UsesRef)

MT.Criteria.user <- TADA_DefineCriteriaMethodology(
  .data = tada.MT.clean,
  criteriaMethods = criteria_table, # user supplied table - all rows are kept from this table
  org_id = "MTDEQ",
  AU_UsesRef = Data_MT_AU_UsesRef,
  displayUniqueId = FALSE,
  excel = FALSE
  # uncomment to run the excel file
  # excel = TRUE, overwrite = TRUE
)
```

    ## [1] "TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."

``` r
TADA_TableExport(MT.Criteria.user)
```

Users can also choose to append EPA304(a) recommended criteria to their
criteria table by including “USEPA” in the org_id.

Users will need to determine their level of desired grouping of
TADA.CharacteristicName by aggregation. If a user has not gone through
the review process of assigning TADA.ComparableDataIdentifiers to
ATTAINS.ParameterName then they must specify what combinations of
fraction and speciations falls under an ATTAINS.ParameterName and
ATTAINS.UseName combination. By specifying displayUniqueId = TRUE, this
will display all unique TADA.ComparableDataIdentifier combinations in
the criteria table output.

``` r
# Will display all unique rows of TADA.Characteristic Name to ATTAINS ParameterName and ATTAINS UseName
MT.Criteria.user2 <- TADA_DefineCriteriaMethodology(
  .data = tada.MT.clean,
  criteriaMethods = criteria_table, # user supplied table - all rows are kept from this table
  org_id = c("MTDEQ", "USEPA"),
  displayUniqueId = TRUE, # will display all unique TADA.ComparableDataIdentifier in this table.
  excel = FALSE
  # uncomment to run the excel file
  # excel = TRUE, overwrite = TRUE
)
```

    ## [1] "TADA_DefineCriteriaMethodology: USEPA was included in your 'org_id': Including EPA304a recommended criteria by each unique TADA.CharacteristicName if one is found."

``` r
TADA_TableExport(MT.Criteria.user2)
```

### Choose a Final Criteria Template, Save and Re-use

``` r
# Save the criteria table of your liking to be used for your next analysis needs.
# TADA_CreateCSV(MT.Criteria_user_supplied_autofill2)

# We can now reuse this criteria table
MT.Criteria.reuse <- TADA_DefineCriteriaMethodology(
  .data = tada.MT.clean,
  criteriaMethods = MT.Criteria.user2, # user supplied table - all rows are kept from this table
  org_id = "MTDEQ",
  displayUniqueId = FALSE,
  excel = FALSE
  # uncomment to run the excel file
  # excel = TRUE, overwrite = TRUE
)
```

    ## [1] "TADA_DefineCriteriaMethodology: displayUniqueId == FALSE was selected, TADA.ComparableDataIdentifier is converted to NA and duplicated rows are removed. Users are recommended to fill out any applicable combinations of Characteristic, Fraction and Speciation for analysis."

``` r
TADA_TableExport(MT.Criteria.reuse)
```

Users are recommended to go through each of the 3 reference files one at
a time though in their review process. In this case, a user should
provide a MLSummaryRef file function input and turn the auto_assign
option to FALSE. Please see ExampleMod3Workflow.Rmd vignette for the
step by step process.
