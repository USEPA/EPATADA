# TADA Module 1: Training for Intermediate/Advanced R Users

## Welcome!

Thank you for your interest in Tools for Automated Data Analysis (TADA).
TADA is an open-source tool set built in the R programming language and
available for anyone to download and edit to their specific needs. This
**TADA Module 1: Training for Intermediate/Advanced R Users** RMarkdown
document ([learn more about
RMarkdown](https://bookdown.org/yihui/rmarkdown/)) walks through how to
download the TADA R package from GitHub, access and parameterize several
important functions with a sample dataframe, and create basic
visualizations. The workflow is similar to a funnel: at each decision
point, data that fail QC checks are removed from the core dataframe and
placed in a separate dataframe, while data that pass are carried into
the next step. At the end of the QC checks, the user should be confident
that their data are properly documented and applicable to the analysis
at hand.

**Note: TADA is still under development. New functionality is added
weekly, and sometimes we need to make bug fixes in response to tester
and user feedback. We appreciate your feedback, patience, and interest
in these helpful tools.**

## Customize or contribute

TADA is housed in a repository on
[GitHub](https://github.com/USEPA/EPATADA). Users desiring to review the
base code and customize the package for their own purposes may:

- Clone the repository using Git

- Open the repository using GitHub Desktop, or

- Download a zip file of the repository to their desktop.

Interested in contributing to the TADA package? The TADA team highly
encourages input and development from users. Check out the
[Contributing](https://usepa.github.io/EPATADA/articles/CONTRIBUTING.html)
page on the TADA GitHub site for guidance on collaboration conventions.

## Install and setup

Users can install the TADA package from GitHub into their R library
using the `remotes` package.

``` r
if (!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}
```

Load the remotes library:

``` r
library(remotes)
```

Next, install and load TADA using the remotes package. TADA R Package
dependencies will also be downloaded automatically from CRAN with the
TADA install. You may be prompted in the console to update dependency
packages that have more recent versions available. If you see this
prompt, it is recommended to update all of them (enter 1 into the
console).

``` r
remotes::install_github("USEPA/EPATADA",
  ref = "develop",
  dependencies = TRUE
)
# remotes::install_github("USGS-R/dataRetrieval", dependencies=TRUE)
```

Finally, use the **library()** function to load the TADA R Package into
your R session.

``` r
library(EPATADA)
```

It’s that easy!

The following code block ensures the additional packages needed to run
the code in this RMarkdown document are loaded. However, users may also
use the `package name:: package function` notation to avoid the list of
[`library()`](https://rdrr.io/r/base/library.html) calls.

``` r
if (!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")
}
```

Load tidyverse library:

``` r
library(tidyverse)
```

## Help pages

All TADA R package functions have their own individual help pages,
listed on the [Function
reference](https://usepa.github.io/EPATADA/reference/index.html) page on
the GitHub site. Users can also access the help page for a given
function in R or RStudio using the following format (example below):
`?[name of TADA function]`

``` r
?TADA_DataRetrieval
```

## Upload data

Now let’s start using the TADA R package functions. The first step is to
bring a dataframe into the R environment. TADA is designed to work with
[Water Quality Portal](https://www.waterqualitydata.us/) (WQP) data.
This means that all of its functions will look for WQP column names and
create new TADA-specific columns based on these elements. Users may
upload their own custom dataframe into R for use with TADA by ensuring
their column names and data formats (e.g. numeric, character) align with
WQP profiles.

If you are interested in reviewing the column headers and formats
required to run TADA, use the function below, which saves an example
spreadsheet to the user’s working directory.

``` r
getwd() # find your working directory
template <- TADA_GetTemplate() # download template to working directory
```

You can also take a look at an example dataframe, like
`Data_Nutrients_UT` to get an idea of the data structure and format.

``` r
utils::data(Data_Nutrients_UT)
# review example dataframe
exampledata_Nutrients_UT <- Data_Nutrients_UT
```

`TADA_DataRetrieval` is built upon USGS’s
[`dataRetrieval::readWQPdata`](https://rdrr.io/pkg/dataRetrieval/man/readWQPdata.html)
and
[`dataRetrieval::whatWQPsites`](https://rdrr.io/pkg/dataRetrieval/man/wqpSpecials.html)
functions within the dataRetrieval package, which uses web service calls
to bring WQP data into the R environment. Additionally,
`TADA_DataRetrieval` performs some basic quality control checks via
`TADA_AutoClean` on the data using new TADA-specific columns to preserve
the original dataframe:

- Converts key character columns to ALL CAPS for easier harmonization
  and validation.

- Identifies different classes of result values (numeric, text,
  percentage, comma-separated numeric, greater than/less than, numbers
  preceded by a tilde, etc.) and converts values to numeric where
  feasible.

- Unifies result and depth units to common units to improve ease of data
  harmonization. See
  [`?TADA_ConvertResultUnits`](https://usepa.github.io/EPATADA/reference/TADA_ConvertResultUnits.md)
  and
  [`?TADA_ConvertDepthUnits`](https://usepa.github.io/EPATADA/reference/TADA_ConvertDepthUnits.md)
  for more information on these processes. These functions can also be
  run separately if the user wishes to convert result or depth values to
  different units.

Let’s give it a try. Setting applyautoclean to TRUE in
`TADA:TADA_DataRetrieval` means that the basic quality control steps
described above are run. See
[`?TADA_AutoClean`](https://usepa.github.io/EPATADA/reference/TADA_AutoClean.md)
for more details. `TADA_DataRetrieval` follows similar parameterization
to the dataRetrieval package function
[`dataRetrieval::readWQPdata`](https://rdrr.io/pkg/dataRetrieval/man/readWQPdata.html),
but check out the [help
page](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.html)
or enter
[`?TADA_DataRetrieval`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
into the console for more information about input parameters and to see
several examples.

``` r
# download example data
# dataset_0  <- TADA_DataRetrieval(
#   organization = c("REDLAKE_WQX",
#                    "SFNOES_WQX",
#                    "PUEBLO_POJOAQUE",
#                    "FONDULAC_WQX",
#                    "PUEBLOOFTESUQUE",
#                    "CNENVSER"),
#   startDate = "2018-01-01",
#   endDate = "2023-01-01")

# For brevity, we'll skip pinging the WQP and instead load the example dataframe:
dataset_0 <- Data_6Tribes_5y

# Let's take a look at all of the TADA-created columns:
names(dataset_0)[grepl("TADA.", names(dataset_0))]
```

    ##  [1] "TADA.ActivityMediaName"                                           
    ##  [2] "TADA.MonitoringLocationName"                                      
    ##  [3] "TADA.MonitoringLocationTypeName"                                  
    ##  [4] "TADA.LatitudeMeasure"                                             
    ##  [5] "TADA.LongitudeMeasure"                                            
    ##  [6] "TADA.MonitoringLocationIdentifier"                                
    ##  [7] "TADA.ResultSampleFractionText"                                    
    ##  [8] "TADA.CharacteristicName"                                          
    ##  [9] "TADA.MethodSpeciationName"                                        
    ## [10] "TADA.ComparableDataIdentifier"                                    
    ## [11] "TADA.ResultMeasureValue"                                          
    ## [12] "TADA.ResultMeasure.MeasureUnitCode"                               
    ## [13] "TADA.WQXResultUnitConversion"                                     
    ## [14] "TADA.ResultMeasureValueDataTypes.Flag"                            
    ## [15] "TADA.DetectionQuantitationLimitMeasure.MeasureValue"              
    ## [16] "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode"           
    ## [17] "TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag"
    ## [18] "TADA.ResultDepthHeightMeasure.MeasureValue"                       
    ## [19] "TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag"         
    ## [20] "TADA.ResultDepthHeightMeasure.MeasureUnitCode"                    
    ## [21] "TADA.ActivityDepthHeightMeasure.MeasureValue"                     
    ## [22] "TADA.ActivityDepthHeightMeasure.MeasureValueDataTypes.Flag"       
    ## [23] "TADA.ActivityDepthHeightMeasure.MeasureUnitCode"                  
    ## [24] "TADA.ActivityTopDepthHeightMeasure.MeasureValue"                  
    ## [25] "TADA.ActivityTopDepthHeightMeasure.MeasureValueDataTypes.Flag"    
    ## [26] "TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode"               
    ## [27] "TADA.ActivityBottomDepthHeightMeasure.MeasureValue"               
    ## [28] "TADA.ActivityBottomDepthHeightMeasure.MeasureValueDataTypes.Flag" 
    ## [29] "TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode"

Currently, the `TADA_DataRetrieval` function combines three WQP data
profiles: Sample Results (Physical/Chemical), Site data, and Project
data. This ensures that all important quality control columns are
included in the dataframe.

**Note:** USGS and EPA are working together to create WQP 3.0 data
profiles. Once released (coming in 2025), one data profile will contain
the columns critical to TADA, removing the need to combine profiles in
this first step. This will simplify the steps needed to upload a custom
or WQP GUI-downloaded dataframe into the R package. However, column
names are changing in the new WQP 3.0 data profiles. This will impact
the `TADA_DataRetrieval` function. The WQP and TADA teams are available
to assist with cross walking the old to new column names when the time
comes.

## Initial data review

Now that we’ve pulled the data into the R session, let’s take a look at
it. Note that any column names with the “TADA.” prefix were generated
from the `TADA_DataRetrieval` function.

First, always good to take a look at the dataframe dimensions.

**Question 1: What are the dimensions of your dataframe?**

``` r
dim(dataset_0) # returns x and of x (as the numbers of rows and columns respectively)
```

    ## [1] 135932    152

Before we start filtering and flagging our data, let’s create a function
(`dimCheck`) that performs dimension checks between the results that
pass each filter or QC flag check (and are retained) and those that do
not (and are removed). These dimension checks ensure that the total
number of rows in the original input dataframe (`all_result_num`) equal
the the total number of rows added up between the passing (`pass_data`)
and removed (`fail_data`) dataframes.

``` r
# defining a dimension check function that compares removed and retained data dimensions against the initial data input
dimCheck <- function(all_result_num, pass_data, fail_data, checkName) {
  # check result numbers after split
  final_result_num <- dim(pass_data)[1] + dim(fail_data)[1]

  # always good to do a dimension check
  if (!all_result_num == final_result_num) {
    print(paste0("Help! Results do not add up between dataframe and removed after ", checkName, " check."))
  } else {
    print(paste0("Good to go. Zero results created or destroyed in ", checkName, " check."))
  }
}

# let's first get the total number of rows in the dataframe.
all_result_num <- dim(dataset_0)[1]
```

Next, we can use the
[`TADA_FieldCounts()`](https://usepa.github.io/EPATADA/reference/TADA_FieldCounts.md)
function to see how many unique values are contained within each column
of the dataframe. The function can either return all column counts,
most, or just the key columns. We’ll try the input with
`display = "key"` and `display = "all"`. Enter `?TADA_FieldCounts()`
into the console for more information on this function.

**Question 2: Which column should have a unique value in every row and
why?**

``` r
key_counts <- TADA_FieldCounts(dataset_0, display = "key")

key_counts
```

    ##                             Fields Count
    ## 1             SubjectTaxonomicName   278
    ## 2    TADA.ComparableDataIdentifier   226
    ## 3          TADA.CharacteristicName   147
    ## 4           OrganizationFormalName     6
    ## 5  TADA.MonitoringLocationTypeName     5
    ## 6           TADA.ActivityMediaName     3
    ## 7     ActivityMediaSubdivisionName     3
    ## 8        ActivityRelativeDepthName     3
    ## 9              ResultValueTypeName     3
    ## 10          ResultStatusIdentifier     2

``` r
all_counts <- TADA_FieldCounts(dataset_0, display = "all")

all_counts
```

    ##                                                                Fields  Count
    ## 1                                                    ResultIdentifier 135932
    ## 2                                             TADA.ResultMeasureValue  40846
    ## 3                                                  ResultMeasureValue  37487
    ## 4                                                  ActivityIdentifier  19180
    ## 5                                 ResultDetectionQuantitationLimitUrl  12311
    ## 6                                               ActivityStartDateTime  11989
    ## 7                                                         LastUpdated   6683
    ## 8                                              ActivityStartTime.Time   3737
    ## 9                          TADA.ResultDepthHeightMeasure.MeasureValue   3608
    ## 10                              ResultDepthHeightMeasure.MeasureValue   3519
    ## 11                                                ActivityEndDateTime   1026
    ## 12                                               ActivityEndTime.Time   1001
    ## 13                                                ActivityCommentText    805
    ## 14                                                  ActivityStartDate    756
    ## 15                                                  AnalysisStartDate    619
    ## 16                                                  ResultCommentText    385
    ## 17                TADA.DetectionQuantitationLimitMeasure.MeasureValue    377
    ## 18                     DetectionQuantitationLimitMeasure.MeasureValue    373
    ## 19                            ActivityDepthHeightMeasure.MeasureValue    321
    ## 20                       TADA.ActivityDepthHeightMeasure.MeasureValue    318
    ## 21                                               SubjectTaxonomicName    278
    ## 22                                  ActivityLocation.LongitudeMeasure    272
    ## 23                                   ActivityLocation.LatitudeMeasure    269
    ## 24                                       MonitoringLocationIdentifier    227
    ## 25                                  TADA.MonitoringLocationIdentifier    227
    ## 26                                      TADA.ComparableDataIdentifier    226
    ## 27                                             MonitoringLocationName    222
    ## 28                                        TADA.MonitoringLocationName    222
    ## 29                                                   LongitudeMeasure    218
    ## 30                                              TADA.LongitudeMeasure    218
    ## 31                                                    LatitudeMeasure    215
    ## 32                                               TADA.LatitudeMeasure    214
    ## 33                                                 CharacteristicName    148
    ## 34                                            TADA.CharacteristicName    147
    ## 35                              DataQuality.UpperConfidenceLimitValue     95
    ## 36                            ResultAnalyticalMethod.MethodIdentifier     74
    ## 37                                  ResultAnalyticalMethod.MethodName     73
    ## 38                                                    ActivityEndDate     68
    ## 39                              DataQuality.LowerConfidenceLimitValue     64
    ## 40                      ActivityBottomDepthHeightMeasure.MeasureValue     55
    ## 41                 TADA.ActivityBottomDepthHeightMeasure.MeasureValue     54
    ## 42                                      ResultMeasure.MeasureUnitCode     44
    ## 43                       ResultAnalyticalMethod.MethodDescriptionText     42
    ## 44                                                        ProjectName     30
    ## 45                                                  ProjectIdentifier     30
    ## 46                                 TADA.ResultMeasure.MeasureUnitCode     24
    ## 47                                             ProjectDescriptionText     18
    ## 48                                                         CountyCode     15
    ## 49                                                  HUCEightDigitCode     15
    ## 50                                  SampleCollectionMethod.MethodName     15
    ## 51                                  MonitoringLocationDescriptionText     14
    ## 52                            SampleCollectionMethod.MethodIdentifier     14
    ## 53                  DetectionQuantitationLimitMeasure.MeasureUnitCode     13
    ## 54                     ResultAnalyticalMethod.MethodIdentifierContext     13
    ## 55                       SampleCollectionMethod.MethodDescriptionText     13
    ## 56             TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode     12
    ## 57                                      SampleCollectionEquipmentName     11
    ## 58                                               MethodSpeciationName     10
    ## 59                                          TADA.MethodSpeciationName     10
    ## 60                              TADA.ResultMeasureValueDataTypes.Flag      9
    ## 61                                               MeasureQualifierCode      9
    ## 62                                                     LaboratoryName      9
    ## 63                                           ResultSampleFractionText      7
    ## 64                                      TADA.ResultSampleFractionText      7
    ## 65                                       ResultDetectionConditionText      7
    ## 66                                 DetectionQuantitationLimitTypeName      7
    ## 67                     SampleCollectionMethod.MethodIdentifierContext      7
    ## 68                                                   ActivityTypeCode      6
    ## 69                                             OrganizationIdentifier      6
    ## 70                                             OrganizationFormalName      6
    ## 71                                         MonitoringLocationTypeName      5
    ## 72                                    TADA.MonitoringLocationTypeName      5
    ## 73                                                          StateCode      4
    ## 74                                     ActivityStartTime.TimeZoneCode      4
    ## 75                              ActivityStartTime.TimeZoneCode_offset      4
    ## 76         TADA.ActivityDepthHeightMeasure.MeasureValueDataTypes.Flag      4
    ## 77                                             QAPPApprovalAgencyName      4
    ## 78                                                  ActivityMediaName      3
    ## 79                                             TADA.ActivityMediaName      3
    ## 80                                       ActivityMediaSubdivisionName      3
    ## 81                                                ResultValueTypeName      3
    ## 82  TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag      3
    ## 83                           ResultDepthHeightMeasure.MeasureUnitCode      3
    ## 84                                          ActivityRelativeDepthName      3
    ## 85                         ActivityDepthHeightMeasure.MeasureUnitCode      3
    ## 86   TADA.ActivityBottomDepthHeightMeasure.MeasureValueDataTypes.Flag      3
    ## 87                                                StatisticalBaseCode      3
    ## 88                                              QAPPApprovedIndicator      3
    ## 89                                       ActivityEndTime.TimeZoneCode      3
    ## 90                                ActivityEndTime.TimeZoneCode_offset      3
    ## 91                                                        CountryCode      2
    ## 92                       HorizontalCoordinateReferenceSystemDatumName      2
    ## 93                                       TADA.WQXResultUnitConversion      2
    ## 94           TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag      2
    ## 95                      TADA.ResultDepthHeightMeasure.MeasureUnitCode      2
    ## 96                    TADA.ActivityDepthHeightMeasure.MeasureUnitCode      2
    ## 97                         ActivityTopDepthHeightMeasure.MeasureValue      2
    ## 98                    TADA.ActivityTopDepthHeightMeasure.MeasureValue      2
    ## 99      TADA.ActivityTopDepthHeightMeasure.MeasureValueDataTypes.Flag      2
    ## 100                     ActivityTopDepthHeightMeasure.MeasureUnitCode      2
    ## 101                TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode      2
    ## 102                  ActivityBottomDepthHeightMeasure.MeasureUnitCode      2
    ## 103             TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode      2
    ## 104                                            ResultStatusIdentifier      2
    ## 105                                ActivityConductingOrganizationText      2
    ## 106                                             SourceMapScaleNumeric      2
    ## 107                                    HorizontalCollectionMethodName      2
    ## 108                                                      ProviderName      1

**Question 3: How many unique ‘TADA.ActivityMediaName’ values exist in
your dataframe? Are there any media types that are not water?**

TADA is currently designed to accommodate water data from the WQP. Let’s
ensure that we remove all non-water data first.

``` r
# remove data with media type that is not water
removed <- dataset_0 |>
  dplyr::filter(!TADA.ActivityMediaName %in% c("WATER")) |>
  dplyr::mutate(TADA.RemovalReason = "Activity media is not water.")

# what other media types exist in dataframe?
unique(removed$TADA.ActivityMediaName)
```

    ## [1] "BIOLOGICAL" "AIR"

``` r
# clean dataframe containing only water
dataset <- dataset_0 |> dplyr::filter(TADA.ActivityMediaName %in% c("WATER"))

dimCheck(all_result_num, dataset, removed, checkName = "Activity Media")
```

    ## [1] "Good to go. Zero results created or destroyed in Activity Media check."

Two additional helper functions one can use at any step in the process
are
[`TADA_FieldValuesTable()`](https://usepa.github.io/EPATADA/reference/TADA_FieldValuesTable.md)
and
[`TADA_FieldValuesPie()`](https://usepa.github.io/EPATADA/reference/TADA_FieldValuesPie.md).
These functions create a summary table and pie chart (respectively) of
all the unique values in a given column. Let’s give it a try on
OrganizationFormalName, which is a WQP column naming the organization
that supplied the result.

``` r
TADA_FieldValuesPie(dataset, field = "OrganizationFormalName")
```

![](TADAModule1_AdvancedTraining_files/figure-html/org-1.png)

``` r
org_counts <- TADA_FieldValuesTable(dataset, field = "OrganizationFormalName")

org_counts
```

    ##                                             Value Count
    ## 1                                    Red Lake DNR 85740
    ## 2               Fond du Lac Band of Chippewa (MN) 22945
    ## 3                     Sac and Fox Nation (Tribal)  9943
    ## 4                      Pueblo Of Tesuque (Tribal)  6798
    ## 5 Chickasaw Nation Environmental Service (Tribal)  4946
    ## 6                              Pueblo of Pojoaque  1101

**Question 4: When might a user choose to view a column’s unique values
as a table rather than in a pie chart?**

We can take a quick look at some of the TADA-created columns that review
result value types. Because TADA is intended to work with numeric data,
at this point, it would be good to remove those result values that are
NA without any detection limit info, or contain text or special
characters that cannot be converted to numeric. Note that TADA will fill
in missing values with detection limit values and units with the
`TADA_IDCensoredData` if the ResultDetectionConditionText and
DetectionQuantitationLimitType fields are populated. See
[`?TADA_ConvertSpecialChars`](https://usepa.github.io/EPATADA/reference/TADA_ConvertSpecialChars.md)
for more details on result value types and handling and
[`?TADA_IDCensoredData`](https://usepa.github.io/EPATADA/reference/TADA_IDCensoredData.md)
for details on censored data preparation.

First, we can run `TADA_IDCensoredData` to fill in as many NA/missing
values as possible. We can use `TADA_FieldValuesPie` to view the
censored data flags identified in the dataframe and their relative
frequency. `TADA_IDCensoredData` sorts result values into detection
limit categories (e.g. non-detect, over-detect) based on populated
values in the ResultDetectionConditionText and
DetectionQuantitationLimitTypeName columns.

You can find the reference tables used to make these decisions in
[`TADA_GetDetCondRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetDetCondRef.md)
and
[`TADA_GetDetLimitRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetDetLimitRef.md)
functions. In some cases, results are missing detection limit/condition
info, or there is a conflict in the detection limit and condition. The
user may want to remove problematic detection limit data before
proceeding. We can also filter for the “problem” data by
TADA.CensoredData.Flag and review the unique reasons for data removal.

``` r
dataset <- TADA_IDCensoredData(dataset)
```

    ## [1] "TADA_IDCensoredData: There are 115 results in your dataframe that are missing ResultDetectionConditionText. TADA requires BOTH ResultDetectionConditionText and DetectionQuantitationLimitTypeName fields to be populated in order to categorize censored data."
    ## [1] "TADA_IDCensoredData: DetectionQuantitationLimitTypeName column in dataframe contains value(s) NA which is/are not represented in the DetectionQuantitationLimitTypeName WQX domain table. These data records are placed under the TADA.CensoredData.Flag: Censored but not Categorized, and will not be used in censored data handling methods. Please contact TADA administrators to resolve."

``` r
TADA_FieldValuesPie(dataset, field = "TADA.CensoredData.Flag")
```

![](TADAModule1_AdvancedTraining_files/figure-html/id%20cens%20data-1.png)

``` r
problem_censored <- dataset |>
  dplyr::filter(!TADA.CensoredData.Flag %in% c("Non-Detect", "Over-Detect", "Other", "Uncensored")) |>
  dplyr::mutate(TADA.RemovalReason = "Detection limit information contains errors or missing information.")

# Let's take a look at the problematic data that we filtered out (if any)
check <- unique(problem_censored[, c("TADA.CharacteristicName", "ResultDetectionConditionText", "DetectionQuantitationLimitTypeName", "TADA.CensoredData.Flag")])

check
```

    ##     TADA.CharacteristicName ResultDetectionConditionText
    ## 1              PHEOPHYTIN A                         <NA>
    ## 10            CHLOROPHYLL A                         <NA>
    ## 12           ORTHOPHOSPHATE                         <NA>
    ## 14                  NITRATE                         <NA>
    ## 15                  NITRITE                         <NA>
    ## 23        ALKALINITY, TOTAL                         <NA>
    ## 27                 CHROMIUM                         <NA>
    ## 28                   COPPER                         <NA>
    ## 104                 SULFATE                         <NA>
    ##     DetectionQuantitationLimitTypeName
    ## 1                                 <NA>
    ## 10                                <NA>
    ## 12              Method Detection Level
    ## 14              Method Detection Level
    ## 15              Method Detection Level
    ## 23              Method Detection Level
    ## 27              Method Detection Level
    ## 28              Method Detection Level
    ## 104             Method Detection Level
    ##                                                TADA.CensoredData.Flag
    ## 1   Detection condition is missing and required for censored data ID.
    ## 10  Detection condition is missing and required for censored data ID.
    ## 12  Detection condition is missing and required for censored data ID.
    ## 14  Detection condition is missing and required for censored data ID.
    ## 15  Detection condition is missing and required for censored data ID.
    ## 23  Detection condition is missing and required for censored data ID.
    ## 27  Detection condition is missing and required for censored data ID.
    ## 28  Detection condition is missing and required for censored data ID.
    ## 104 Detection condition is missing and required for censored data ID.

``` r
dataset <- dataset |> dplyr::filter(TADA.CensoredData.Flag %in% c("Non-Detect", "Over-Detect", "Other", "Uncensored"))

# Let's take a look at the removed data
removed <- plyr::rbind.fill(removed, problem_censored)

# dimension check
dimCheck(all_result_num, dataset, removed, checkName = "Censored Data")
```

    ## [1] "Good to go. Zero results created or destroyed in Censored Data check."

Next, we can take a look at the data types present and filter out any
non-allowable types.

``` r
# take a look at datatypes
flag.datatypes <- TADA_FieldValuesTable(dataset, field = "TADA.ResultMeasureValueDataTypes.Flag")

# Numeric or numeric-coerced data types
rv_datatypes <- unique(subset(dataset, !is.na(dataset$TADA.ResultMeasureValue))$TADA.ResultMeasureValueDataTypes.Flag)

# Non-numeric data types coerced to NA
na_rv_datatypes <- unique(subset(dataset, is.na(dataset$TADA.ResultMeasureValue))$TADA.ResultMeasureValueDataTypes.Flag)
```

``` r
# these are all of the NOT allowable data types in the dataset.
incompatible_datatype <- dataset |>
  dplyr::filter(!dataset$TADA.ResultMeasureValueDataTypes.Flag %in% c("Numeric", "Less Than", "Greater Than", "Approximate Value", "Percentage", "Comma-Separated Numeric", "Numeric Range - Averaged", "Result Value/Unit Copied from Detection Limit")) |>
  dplyr::mutate(TADA.RemovalReason = "Result value type cannot be converted to numeric or no detection limit values provided.")

# take a look at the difficult data types - do they make sense?
check <- unique(incompatible_datatype[, c("TADA.CharacteristicName", "ResultMeasureValue", "TADA.ResultMeasureValue", "ResultMeasure.MeasureUnitCode", "TADA.ResultMeasure.MeasureUnitCode", "TADA.ResultMeasureValueDataTypes.Flag", "DetectionQuantitationLimitMeasure.MeasureValue", "TADA.DetectionQuantitationLimitMeasure.MeasureValue", "DetectionQuantitationLimitMeasure.MeasureUnitCode", "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode")])

check
```

    ##                          TADA.CharacteristicName
    ## 1                      LOONS, VISUAL OBSERVATION
    ## 3                                    CLOUD COVER
    ## 6                                  WIND VELOCITY
    ## 7                                    CLOUD COVER
    ## 17                                   CLOUD COVER
    ## 23                                  HEIGHT, GAGE
    ## 32                                    TRUE COLOR
    ## 33                                TOTAL HARDNESS
    ## 34                                APPARENT COLOR
    ## 35                                    ALKALINITY
    ## 36                       WATER APPEARANCE (TEXT)
    ## 37                       WATER APPEARANCE (TEXT)
    ## 39                       WATER APPEARANCE (TEXT)
    ## 41                       WATER APPEARANCE (TEXT)
    ## 42                       WATER APPEARANCE (TEXT)
    ## 44                       WATER APPEARANCE (TEXT)
    ## 50                       WATER APPEARANCE (TEXT)
    ## 66                       WATER APPEARANCE (TEXT)
    ## 70                       WATER APPEARANCE (TEXT)
    ## 87                       WATER APPEARANCE (TEXT)
    ## 92                       WATER APPEARANCE (TEXT)
    ## 95                       WATER APPEARANCE (TEXT)
    ## 132                      WATER APPEARANCE (TEXT)
    ## 164                      WATER APPEARANCE (TEXT)
    ## 166                      WATER APPEARANCE (TEXT)
    ## 216                      WATER APPEARANCE (TEXT)
    ## 234                      WATER APPEARANCE (TEXT)
    ## 280                      WATER APPEARANCE (TEXT)
    ## 294                      WATER APPEARANCE (TEXT)
    ## 369                      WATER APPEARANCE (TEXT)
    ## 426                      WATER APPEARANCE (TEXT)
    ## 492                      WATER APPEARANCE (TEXT)
    ## 502                      WATER APPEARANCE (TEXT)
    ## 508                      WATER APPEARANCE (TEXT)
    ## 578                      WATER APPEARANCE (TEXT)
    ## 598                      WATER APPEARANCE (TEXT)
    ## 600                      WATER APPEARANCE (TEXT)
    ## 672                      WATER APPEARANCE (TEXT)
    ## 804                      WATER APPEARANCE (TEXT)
    ## 817                      WATER APPEARANCE (TEXT)
    ## 964                      WATER APPEARANCE (TEXT)
    ## 1103                                        FLOW
    ## 1104                         BAROMETRIC PRESSURE
    ## 1105                         BAROMETRIC PRESSURE
    ## 1113                     WATER APPEARANCE (TEXT)
    ## 1313                     WATER APPEARANCE (TEXT)
    ## 1503                     WATER APPEARANCE (TEXT)
    ## 1615                          TEMPERATURE, WATER
    ## 1616                 DISSOLVED OXYGEN SATURATION
    ## 1617                                CONDUCTIVITY
    ## 1618                      TOTAL DISSOLVED SOLIDS
    ## 1643                     WATER APPEARANCE (TEXT)
    ## 1919                     WATER APPEARANCE (TEXT)
    ## 2027                     WATER APPEARANCE (TEXT)
    ## 2046                     WATER APPEARANCE (TEXT)
    ## 2248                     WATER APPEARANCE (TEXT)
    ## 2333 LAKE RECREATIONAL SUITABILITY (CHOICE LIST)
    ## 2373 LAKE RECREATIONAL SUITABILITY (CHOICE LIST)
    ## 2381 LAKE RECREATIONAL SUITABILITY (CHOICE LIST)
    ## 2427                     WATER APPEARANCE (TEXT)
    ##                                         ResultMeasureValue
    ## 1                                                     <NA>
    ## 3                                                    foggy
    ## 6                                                     Calm
    ## 7                                              clear_sunny
    ## 17                                                 raining
    ## 23                                                    1:78
    ## 32                                                    <NA>
    ## 33                                                    <NA>
    ## 34                                                    <NA>
    ## 35                                                    <NA>
    ## 36                            Sediment; some algae present
    ## 37                               Stain; some algae present
    ## 39                                                Sediment
    ## 41                               Stain; Some algae present
    ## 42                                    Clear; Crystal Clear
    ## 44                                                   Stain
    ## 50                               Green; some algae present
    ## 66                               Green; Some algae present
    ## 70                                  Stained; Crystal Clear
    ## 87                                    Green; crystal clear
    ## 92                            Sediment; Some algae present
    ## 95    Clear water color; crystal clear physical conditions
    ## 132                              Clear; some algae present
    ## 164                                   Stain; crystal clear
    ## 166                          Green; definite algae present
    ## 216                                                  Clear
    ## 234                                                  Green
    ## 280                                            Tea-colored
    ## 294                              Clear; Some algae present
    ## 369                                   Stain; Crystal clear
    ## 426                                                 Cloudy
    ## 492    Water color clear; physical condition crystal clear
    ## 502  Water color stained; physical condition crystal clear
    ## 508                          Stain; Definite algae present
    ## 578                                   Green; Crystal Clear
    ## 598                                Sediment; Crystal clear
    ## 600                                Sediment; Crystal Clear
    ## 672                          Green; Definite algae present
    ## 804                      Sediment; Definitie algae present
    ## 817                                   Green; Crystal clear
    ## 964                                                Stained
    ## 1103                                                  <NA>
    ## 1104                                         600.7061 mmHG
    ## 1105                                           600.71 mmHG
    ## 1113                                  Clear; Crystal clear
    ## 1313                      Sediment; Definite algae present
    ## 1503                         Stain; definite algae present
    ## 1615                                                  None
    ## 1616                                                  mg/l
    ## 1617                                                  mg/l
    ## 1618                                                   PSS
    ## 1643                                  Clear; crystal clear
    ## 1919                         Clear; Definite algae present
    ## 2027                 Sediment; Stage tape-down from bridge
    ## 2046                      Sediment; definite algae present
    ## 2248                                                 Muddy
    ## 2333                                           1.VERY GOOD
    ## 2373                                                2.GOOD
    ## 2381                                                3.FAIR
    ## 2427                               Green; High algal color
    ##      TADA.ResultMeasureValue ResultMeasure.MeasureUnitCode
    ## 1                         NA                          <NA>
    ## 3                         NA                             %
    ## 6                         NA                           mph
    ## 7                         NA                             %
    ## 17                        NA                             %
    ## 23                        NA                            ft
    ## 32                        NA                            CU
    ## 33                        NA                          mg/L
    ## 34                        NA                            CU
    ## 35                        NA                          mg/L
    ## 36                        NA                          <NA>
    ## 37                        NA                          <NA>
    ## 39                        NA                          <NA>
    ## 41                        NA                          <NA>
    ## 42                        NA                          <NA>
    ## 44                        NA                          <NA>
    ## 50                        NA                          <NA>
    ## 66                        NA                          <NA>
    ## 70                        NA                          <NA>
    ## 87                        NA                          <NA>
    ## 92                        NA                          <NA>
    ## 95                        NA                          <NA>
    ## 132                       NA                          <NA>
    ## 164                       NA                          <NA>
    ## 166                       NA                          <NA>
    ## 216                       NA                          <NA>
    ## 234                       NA                          <NA>
    ## 280                       NA                          <NA>
    ## 294                       NA                          <NA>
    ## 369                       NA                          <NA>
    ## 426                       NA                          <NA>
    ## 492                       NA                          <NA>
    ## 502                       NA                          <NA>
    ## 508                       NA                          <NA>
    ## 578                       NA                          <NA>
    ## 598                       NA                          <NA>
    ## 600                       NA                          <NA>
    ## 672                       NA                          <NA>
    ## 804                       NA                          <NA>
    ## 817                       NA                          <NA>
    ## 964                       NA                          <NA>
    ## 1103                      NA                           cfs
    ## 1104                      NA                           psi
    ## 1105                      NA                           psi
    ## 1113                      NA                          <NA>
    ## 1313                      NA                          <NA>
    ## 1503                      NA                          <NA>
    ## 1615                      NA                         deg C
    ## 1616                      NA                             %
    ## 1617                      NA                         uS/cm
    ## 1618                      NA                          mg/L
    ## 1643                      NA                          <NA>
    ## 1919                      NA                          <NA>
    ## 2027                      NA                          <NA>
    ## 2046                      NA                          <NA>
    ## 2248                      NA                          <NA>
    ## 2333                      NA                          <NA>
    ## 2373                      NA                          <NA>
    ## 2381                      NA                          <NA>
    ## 2427                      NA                          <NA>
    ##      TADA.ResultMeasure.MeasureUnitCode TADA.ResultMeasureValueDataTypes.Flag
    ## 1                                  <NA>                    NA - Not Available
    ## 3                                     %                                  Text
    ## 6                                 M/SEC                                  Text
    ## 7                                     %                                  Text
    ## 17                                    %                                  Text
    ## 23                                    M                         Coerced to NA
    ## 32                                  PCU                    NA - Not Available
    ## 33                                 MG/L                    NA - Not Available
    ## 34                                  PCU                    NA - Not Available
    ## 35                                 UG/L                    NA - Not Available
    ## 36                                 <NA>                                  Text
    ## 37                                 <NA>                                  Text
    ## 39                                 <NA>                                  Text
    ## 41                                 <NA>                                  Text
    ## 42                                 <NA>                                  Text
    ## 44                                 <NA>                                  Text
    ## 50                                 <NA>                                  Text
    ## 66                                 <NA>                                  Text
    ## 70                                 <NA>                                  Text
    ## 87                                 <NA>                                  Text
    ## 92                                 <NA>                                  Text
    ## 95                                 <NA>                                  Text
    ## 132                                <NA>                                  Text
    ## 164                                <NA>                                  Text
    ## 166                                <NA>                                  Text
    ## 216                                <NA>                                  Text
    ## 234                                <NA>                                  Text
    ## 280                                <NA>                                  Text
    ## 294                                <NA>                                  Text
    ## 369                                <NA>                                  Text
    ## 426                                <NA>                                  Text
    ## 492                                <NA>                                  Text
    ## 502                                <NA>                                  Text
    ## 508                                <NA>                                  Text
    ## 578                                <NA>                                  Text
    ## 598                                <NA>                                  Text
    ## 600                                <NA>                                  Text
    ## 672                                <NA>                                  Text
    ## 804                                <NA>                                  Text
    ## 817                                <NA>                                  Text
    ## 964                                <NA>                                  Text
    ## 1103                                CFS                    NA - Not Available
    ## 1104                               G/M2                                  Text
    ## 1105                               G/M2                                  Text
    ## 1113                               <NA>                                  Text
    ## 1313                               <NA>                                  Text
    ## 1503                               <NA>                                  Text
    ## 1615                              DEG C                                  Text
    ## 1616                                  %                                  Text
    ## 1617                              US/CM                                  Text
    ## 1618                               UG/L                                  Text
    ## 1643                               <NA>                                  Text
    ## 1919                               <NA>                                  Text
    ## 2027                               <NA>                                  Text
    ## 2046                               <NA>                                  Text
    ## 2248                               <NA>                                  Text
    ## 2333                               <NA>                                  Text
    ## 2373                               <NA>                                  Text
    ## 2381                               <NA>                                  Text
    ## 2427                               <NA>                                  Text
    ##      DetectionQuantitationLimitMeasure.MeasureValue
    ## 1                                              <NA>
    ## 3                                              <NA>
    ## 6                                              <NA>
    ## 7                                              <NA>
    ## 17                                             <NA>
    ## 23                                             <NA>
    ## 32                                             <NA>
    ## 33                                             <NA>
    ## 34                                             <NA>
    ## 35                                             <NA>
    ## 36                                             <NA>
    ## 37                                             <NA>
    ## 39                                             <NA>
    ## 41                                             <NA>
    ## 42                                             <NA>
    ## 44                                             <NA>
    ## 50                                             <NA>
    ## 66                                             <NA>
    ## 70                                             <NA>
    ## 87                                             <NA>
    ## 92                                             <NA>
    ## 95                                             <NA>
    ## 132                                            <NA>
    ## 164                                            <NA>
    ## 166                                            <NA>
    ## 216                                            <NA>
    ## 234                                            <NA>
    ## 280                                            <NA>
    ## 294                                            <NA>
    ## 369                                            <NA>
    ## 426                                            <NA>
    ## 492                                            <NA>
    ## 502                                            <NA>
    ## 508                                            <NA>
    ## 578                                            <NA>
    ## 598                                            <NA>
    ## 600                                            <NA>
    ## 672                                            <NA>
    ## 804                                            <NA>
    ## 817                                            <NA>
    ## 964                                            <NA>
    ## 1103                                           <NA>
    ## 1104                                           <NA>
    ## 1105                                           <NA>
    ## 1113                                           <NA>
    ## 1313                                           <NA>
    ## 1503                                           <NA>
    ## 1615                                           <NA>
    ## 1616                                           <NA>
    ## 1617                                           <NA>
    ## 1618                                           <NA>
    ## 1643                                           <NA>
    ## 1919                                           <NA>
    ## 2027                                           <NA>
    ## 2046                                           <NA>
    ## 2248                                           <NA>
    ## 2333                                           <NA>
    ## 2373                                           <NA>
    ## 2381                                           <NA>
    ## 2427                                           <NA>
    ##      TADA.DetectionQuantitationLimitMeasure.MeasureValue
    ## 1                                                     NA
    ## 3                                                     NA
    ## 6                                                     NA
    ## 7                                                     NA
    ## 17                                                    NA
    ## 23                                                    NA
    ## 32                                                    NA
    ## 33                                                    NA
    ## 34                                                    NA
    ## 35                                                    NA
    ## 36                                                    NA
    ## 37                                                    NA
    ## 39                                                    NA
    ## 41                                                    NA
    ## 42                                                    NA
    ## 44                                                    NA
    ## 50                                                    NA
    ## 66                                                    NA
    ## 70                                                    NA
    ## 87                                                    NA
    ## 92                                                    NA
    ## 95                                                    NA
    ## 132                                                   NA
    ## 164                                                   NA
    ## 166                                                   NA
    ## 216                                                   NA
    ## 234                                                   NA
    ## 280                                                   NA
    ## 294                                                   NA
    ## 369                                                   NA
    ## 426                                                   NA
    ## 492                                                   NA
    ## 502                                                   NA
    ## 508                                                   NA
    ## 578                                                   NA
    ## 598                                                   NA
    ## 600                                                   NA
    ## 672                                                   NA
    ## 804                                                   NA
    ## 817                                                   NA
    ## 964                                                   NA
    ## 1103                                                  NA
    ## 1104                                                  NA
    ## 1105                                                  NA
    ## 1113                                                  NA
    ## 1313                                                  NA
    ## 1503                                                  NA
    ## 1615                                                  NA
    ## 1616                                                  NA
    ## 1617                                                  NA
    ## 1618                                                  NA
    ## 1643                                                  NA
    ## 1919                                                  NA
    ## 2027                                                  NA
    ## 2046                                                  NA
    ## 2248                                                  NA
    ## 2333                                                  NA
    ## 2373                                                  NA
    ## 2381                                                  NA
    ## 2427                                                  NA
    ##      DetectionQuantitationLimitMeasure.MeasureUnitCode
    ## 1                                                 <NA>
    ## 3                                                 <NA>
    ## 6                                                 <NA>
    ## 7                                                 <NA>
    ## 17                                                <NA>
    ## 23                                                <NA>
    ## 32                                                <NA>
    ## 33                                                <NA>
    ## 34                                                <NA>
    ## 35                                                <NA>
    ## 36                                                <NA>
    ## 37                                                <NA>
    ## 39                                                <NA>
    ## 41                                                <NA>
    ## 42                                                <NA>
    ## 44                                                <NA>
    ## 50                                                <NA>
    ## 66                                                <NA>
    ## 70                                                <NA>
    ## 87                                                <NA>
    ## 92                                                <NA>
    ## 95                                                <NA>
    ## 132                                               <NA>
    ## 164                                               <NA>
    ## 166                                               <NA>
    ## 216                                               <NA>
    ## 234                                               <NA>
    ## 280                                               <NA>
    ## 294                                               <NA>
    ## 369                                               <NA>
    ## 426                                               <NA>
    ## 492                                               <NA>
    ## 502                                               <NA>
    ## 508                                               <NA>
    ## 578                                               <NA>
    ## 598                                               <NA>
    ## 600                                               <NA>
    ## 672                                               <NA>
    ## 804                                               <NA>
    ## 817                                               <NA>
    ## 964                                               <NA>
    ## 1103                                              <NA>
    ## 1104                                              <NA>
    ## 1105                                              <NA>
    ## 1113                                              <NA>
    ## 1313                                              <NA>
    ## 1503                                              <NA>
    ## 1615                                              <NA>
    ## 1616                                              <NA>
    ## 1617                                              <NA>
    ## 1618                                              <NA>
    ## 1643                                              <NA>
    ## 1919                                              <NA>
    ## 2027                                              <NA>
    ## 2046                                              <NA>
    ## 2248                                              <NA>
    ## 2333                                              <NA>
    ## 2373                                              <NA>
    ## 2381                                              <NA>
    ## 2427                                              <NA>
    ##      TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode
    ## 1                                                      <NA>
    ## 3                                                      <NA>
    ## 6                                                      <NA>
    ## 7                                                      <NA>
    ## 17                                                     <NA>
    ## 23                                                     <NA>
    ## 32                                                     <NA>
    ## 33                                                     <NA>
    ## 34                                                     <NA>
    ## 35                                                     <NA>
    ## 36                                                     <NA>
    ## 37                                                     <NA>
    ## 39                                                     <NA>
    ## 41                                                     <NA>
    ## 42                                                     <NA>
    ## 44                                                     <NA>
    ## 50                                                     <NA>
    ## 66                                                     <NA>
    ## 70                                                     <NA>
    ## 87                                                     <NA>
    ## 92                                                     <NA>
    ## 95                                                     <NA>
    ## 132                                                    <NA>
    ## 164                                                    <NA>
    ## 166                                                    <NA>
    ## 216                                                    <NA>
    ## 234                                                    <NA>
    ## 280                                                    <NA>
    ## 294                                                    <NA>
    ## 369                                                    <NA>
    ## 426                                                    <NA>
    ## 492                                                    <NA>
    ## 502                                                    <NA>
    ## 508                                                    <NA>
    ## 578                                                    <NA>
    ## 598                                                    <NA>
    ## 600                                                    <NA>
    ## 672                                                    <NA>
    ## 804                                                    <NA>
    ## 817                                                    <NA>
    ## 964                                                    <NA>
    ## 1103                                                   <NA>
    ## 1104                                                   <NA>
    ## 1105                                                   <NA>
    ## 1113                                                   <NA>
    ## 1313                                                   <NA>
    ## 1503                                                   <NA>
    ## 1615                                                   <NA>
    ## 1616                                                   <NA>
    ## 1617                                                   <NA>
    ## 1618                                                   <NA>
    ## 1643                                                   <NA>
    ## 1919                                                   <NA>
    ## 2027                                                   <NA>
    ## 2046                                                   <NA>
    ## 2248                                                   <NA>
    ## 2333                                                   <NA>
    ## 2373                                                   <NA>
    ## 2381                                                   <NA>
    ## 2427                                                   <NA>

Then we can take a closer look at the removed results and run another
dimension check on the data set.

``` r
# filter data set to include allowable data types
dataset <- dataset |> dplyr::filter(dataset$TADA.ResultMeasureValueDataTypes.Flag %in% c("Numeric", "Less Than", "Greater Than", "Approximate Value", "Percentage", "Comma-Separated Numeric", "Numeric Range - Averaged", "Result Value/Unit Copied from Detection Limit"))

# create dataframe to includ all removed results
removed <- plyr::rbind.fill(removed, incompatible_datatype)

# dimension check
dimCheck(all_result_num, dataset, removed, checkName = "Result Format")
```

    ## [1] "Good to go. Zero results created or destroyed in Result Format check."

## Data flagging

We’ve taken a quick look at the raw dataframe and split off some data
that are not compatible with TADA, now let’s run through some quality
control checks. The most important ones to run to ensure your dataframe
is ready for subsequent steps are
[`TADA_FlagFraction()`](https://usepa.github.io/EPATADA/reference/TADA_FlagFraction.md),
[`TADA_FlagSpeciation()`](https://usepa.github.io/EPATADA/reference/TADA_FlagSpeciation.md),
[`TADA_FlagResultUnit()`](https://usepa.github.io/EPATADA/reference/TADA_FlagResultUnit.md),
and
[`TADA_FindQCActivities()`](https://usepa.github.io/EPATADA/reference/TADA_FindQCActivities.md).
With the exception of
[`TADA_FindQCActivities()`](https://usepa.github.io/EPATADA/reference/TADA_FindQCActivities.md),
these flagging functions leverage WQX’s [QAQC Validation
Table](https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
See the [WQX QAQC Service User
Guide](https://usepa.github.io/EPATADA/articles/WQXValidationService.html)
for more details on how TADA leverages the validation table to flag
potentially suspect data.
[`TADA_FindQCActivities()`](https://usepa.github.io/EPATADA/reference/TADA_FindQCActivities.md)
uses a TADA-specific domain table users can review with
[`TADA_GetActivityTypeRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetActivityTypeRef.md).
All QAQC tables are frequently updated in the package to ensure they
match the latest version on the web.

Bring the QAQC Validation Table into your R session to view or save with
the following command:

``` r
qaqc_ref <- TADA_GetWQXCharValRef()

unique(qaqc_ref[["Type"]])
```

    ## [1] "CharacteristicFraction"   "CharacteristicMethod"    
    ## [3] "CharacteristicSpeciation" "CharacteristicUnit"

**Question 5: What do you think the Type column in the qaqc_ref data
frame indicates?**

TADA joins this validation table to the data and uses the “Pass” and
“Suspect” labels in the TADA.WQXVal.Flag column to create easily
understandable flagging columns for each function. Let’s run these four
flagging functions.

``` r
dataset_flags <- TADA_FlagFraction(dataset, clean = FALSE, flaggedonly = FALSE)
```

    ## [1] "TADA_FlagFraction: Rows with Suspect sample fractions have been flagged but retained. Review these rows using the TADA.SampleFraction.Flag column before proceeding and/or set clean = TRUE."

``` r
dataset_flags <- TADA_FlagSpeciation(dataset_flags, clean = "none", flaggedonly = FALSE)
```

    ## [1] "TADA_FlagSpeciation: Rows with Suspect speciations have been flagged but retained. Review these rows using the new TADA.MethodSpeciation.Flag column before proceeding and/or set clean = 'suspect_only' or 'both'."

``` r
dataset_flags <- TADA_FlagResultUnit(dataset_flags, clean = "none", flaggedonly = FALSE)
dataset_flags <- TADA_FindQCActivities(dataset_flags, clean = FALSE, flaggedonly = FALSE)

dimCheck(all_result_num, dataset_flags, removed, checkName = "Run Flag Functions")
```

    ## [1] "Good to go. Zero results created or destroyed in Run Flag Functions check."

**Question 6: Did any warnings or messages appear in the console after
running these flagging functions? What do they say?**

Now that we’ve run all the key flagging functions, let’s take a look at
the results and make some decisions.

``` r
TADA_FieldValuesPie(dataset_flags, field = "TADA.SampleFraction.Flag")
```

![](TADAModule1_AdvancedTraining_files/figure-html/flag%20pies-1.png)

``` r
TADA_FieldValuesPie(dataset_flags, field = "TADA.MethodSpeciation.Flag")
```

![](TADAModule1_AdvancedTraining_files/figure-html/flag%20pies-2.png)

``` r
TADA_FieldValuesPie(dataset_flags, field = "TADA.ResultUnit.Flag")
```

![](TADAModule1_AdvancedTraining_files/figure-html/flag%20pies-3.png)

``` r
TADA_FieldValuesPie(dataset_flags, field = "TADA.ActivityType.Flag")
```

![](TADAModule1_AdvancedTraining_files/figure-html/flag%20pies-4.png)

Any results flagged as “Suspect” are recognized in the QAQC Validation
Table as having some data quality issue. “NonStandardized” means that
the format has not been fully vetted or processed, while “Pass” confirms
that the characteristic combination is widely recognized as correctly
formatted. Let’s add any suspect results to the removed dataframe for
later review.

**Note: if you find any errors in the QAQC Validation Table, please
contact the WQX Help Desk at WQX@epa.gov to help correct it. Thanks in
advance!**

``` r
# grab all the flagged results from the four functions
problem_flagged <- dataset_flags |>
  dplyr::filter(TADA.SampleFraction.Flag == "Suspect" | TADA.MethodSpeciation.Flag == "Suspect" | TADA.ResultUnit.Flag == "Suspect" | !TADA.ActivityType.Flag %in% ("Non_QC")) |>
  dplyr::mutate(TADA.RemovalReason = "Invalid Unit, Method, Speciation, or Activity Type.")

dataset_flags <- dataset_flags |> dplyr::filter(!ResultIdentifier %in% problem_flagged$ResultIdentifier)

# create dataframe of removed results
removed <- plyr::rbind.fill(removed, problem_flagged)

# remove df no longer needed
rm(problem_flagged)

# dimension check
dimCheck(all_result_num, dataset_flags, removed, checkName = "Filter Flag Functions")
```

    ## [1] "Good to go. Zero results created or destroyed in Filter Flag Functions check."

**Question 7: Are there any other metadata columns that you review and
filter in your workflow?**

We’ve finished running the recommended flagging functions and removing
results that do not pass QC checks. Let’s look at the breakdown of these
data in the removed object.

``` r
removal <- TADA_FieldValuesTable(removed, field = "TADA.RemovalReason")

removal
```

    ##                                                                                     Value
    ## 1                                                            Activity media is not water.
    ## 2 Result value type cannot be converted to numeric or no detection limit values provided.
    ## 3                                     Invalid Unit, Method, Speciation, or Activity Type.
    ## 4                     Detection limit information contains errors or missing information.
    ##   Count
    ## 1  4459
    ## 2  3184
    ## 3  2518
    ## 4   115

You can review any other columns of interest and create custom domain
tables of your “Valid” and “Invalid” criteria using R or Excel. Also
check out some of the other flagging functions available in TADA:

- `?TADA_FindNearbySites()`

- `?TADA_FindPotentialDuplicatesMultipleOrgs()`

- `?TADA_FindPotentialDuplicatesSingleOrg()`

- `?TADA_FindQAPPApproval()`

- `?TADA_FindQAPPDoc()`

- `?TADA_FlagAboveThreshold()`

- `?TADA_FlagBelowThreshold()`

- `?TADA_FlagContinuousData()`

- `?TADA_FlagCoordinates()`

- `?TADA_FlagMeasureQualifierCode()`

- `?TADA_FlagMethod()`

Please let us know of other flagging functions you think would have
broad appeal in the TADA package or need assistance
brainstorming/developing.

## Censored data handling

We have already identified, flagged, and in some cases removed
problematic detection limit data from our dataframe, but that doesn’t
keep them from being difficult. Because we do not know the result value
with adequate precision, water quality data users often set non-detect
values to some number below the reported detection limit. TADA contains
some simple methods for handling detection limits: users may multiply
the detection limit by some number between 0 and 1, or convert the
detection limit value to a random number between 0 and the detection
limit. More complex detection limit estimation requiring regression
models (Maximum Likelihood, Kaplan-Meier, Robust Regression on Order
Statistics) or similar must be performed outside of the current version
of TADA (though future development is planned).

**Question 8: How would you parameterize
[`TADA_SimpleCensoredMethods()`](https://usepa.github.io/EPATADA/reference/TADA_SimpleCensoredMethods.md)
to make non-detect values equal to the provided detection limit? What
would you need to change in the example below?**

``` r
dataset_cens <- TADA_SimpleCensoredMethods(dataset_flags,
  nd_method = "multiplier",
  nd_multiplier = 0.5,
  od_method = "as-is"
)
```

Let’s take a look at how the censored data handling function affects the
`TADA.ResultMeasureValueDataTypes.Flag` column.

First, we can look use `TADA_FieldValuesTable` to look at the
TADA.ResultMeasureValueDataTypes.Flag column in data set before we ran
`TADA_SimpleCensoredMethods`.

``` r
# before
TADA_FieldValuesTable(dataset_flags, field = "TADA.ResultMeasureValueDataTypes.Flag")
```

    ##                                           Value  Count
    ## 1                                       Numeric 120823
    ## 2 Result Value/Unit Copied from Detection Limit   4032
    ## 3                                    Percentage    745
    ## 4                      Numeric Range - Averaged     33
    ## 5                                     Less Than     19
    ## 6                                  Greater Than      3
    ## 7                       Comma-Separated Numeric      1

Then we can use `TADA_FieldValuesTable` again to look at the same column
after `TADA_SimpleCensoredMethods`.

``` r
# after
TADA_FieldValuesTable(dataset_cens, field = "TADA.ResultMeasureValueDataTypes.Flag")
```

    ##                                              Value  Count
    ## 1                                          Numeric 120783
    ## 2 Result Value/Unit Estimated from Detection Limit   4072
    ## 3                                       Percentage    745
    ## 4                         Numeric Range - Averaged     33
    ## 5                                        Less Than     19
    ## 6                                     Greater Than      3
    ## 7                          Comma-Separated Numeric      1

**Question 9: Is there a difference between the first and second
tables?**

If you’d like to start thinking about using statistical methods to
estimate detection limit values, check out the
[`?TADA_Stats`](https://usepa.github.io/EPATADA/reference/TADA_Stats.md)
function, which accepts user-defined data groupings (or defaults to
TADA.ComparableDataIdentifier to determine measurement count, location
count, censored data stats, min, max, and percentile stats, and suggests
non-detect estimation methods based on the number of results, % of data
frame censored, and number of censoring levels (detection limits). The
decision tree used in the function was outlined in a [National Nonpoint
Source Tech
Memo](https://www.epa.gov/sites/default/files/2016-05/documents/tech_notes_10_jun2014_r.pdf).

## Data exploration

How are you feeling about your test dataframe? Does it seem ready for
the next step(s) in your analyses? There’s probably a lot more you’d
like to look at/filter out before you’re ready to say: QC complete.
Let’s first check out characteristics in the dataframe using `dplyr`
functions and pipes.

``` r
# get table of characteristics with number of results, sites, and organizations
dataset_cens_summary <- dataset_cens |>
  dplyr::group_by(TADA.CharacteristicName) |>
  dplyr::summarise(Result_Count = length(ResultIdentifier), Site_Count = length(unique(TADA.MonitoringLocationIdentifier)), Org_Count = length(unique(OrganizationIdentifier))) |>
  dplyr::arrange(desc(Result_Count))
```

You may see a characteristic that you’d like to investigate further in
isolation.
[`TADA_FieldValuesPie()`](https://usepa.github.io/EPATADA/reference/TADA_FieldValuesPie.md)
will also produce summary pie charts for a given column *within* a
specific characteristic. Let’s take a look.

``` r
# go ahead and pick a characteristic name from the table generated above. I picked dissolved oxygen (DO) amd selected OrganizationFormalName as the field to see the relative contribution of each org to DO results
TADA_FieldValuesPie(dataset_cens, field = "OrganizationFormalName", characteristicName = "DISSOLVED OXYGEN (DO)")
```

![](TADAModule1_AdvancedTraining_files/figure-html/pie%20by%20characteristic-1.png)

We can view the site locations using a TADA mapping function. In this
map, the circles indicate monitoring locations in the data set; their
size corresponds to the number of results collected at that site, while
the darker the circle, the more characteristics were sampled at that
site.

``` r
TADA_OverviewMap(dataset_cens)
```

Out of curiosity, let’s take a look at a breakdown of these monitoring
location types. Do they all indicate surface water samples? Depending
upon your program’s goals and methods, you might want to filter out some
of the types you see.

``` r
TADA_FieldValuesPie(dataset_cens, field = "TADA.MonitoringLocationTypeName")
```

![](TADAModule1_AdvancedTraining_files/figure-html/location%20pie-1.png)

One of the next big steps is data harmonization: translating and
aggregating synonyms, combining multiple forms/species of certain
characteristics, etc. We won’t get to that in this demo (more details
can be found here: [TADA Module 1: Water Quality Portal Data Discovery
and Cleaning](https://usepa.github.io/EPATADA/articles/TADAModule1.html)
or
[TADA_HarmonizeSynonyms()](https://usepa.github.io/EPATADA/reference/TADA_HarmonizeSynonyms.html)),
but for now we can start looking at data distributions within a single
characteristic-speciation-fraction-unit using the plotting functions
[`TADA_Histogram()`](https://usepa.github.io/EPATADA/reference/TADA_Histogram.md)
and
[`TADA_Boxplot()`](https://usepa.github.io/EPATADA/reference/TADA_Boxplot.md).
We can also view a stats table using `TADA_Stats`.

Let’s first take a look at the column TADA.ComparableDataIdentifier,
which breaks down characteristics into groups by name, fraction,
speciation, and unit. These four columns are important to evaluate
together to ensure the plotted data are sufficiently similar to appear
on a single plot together: it doesn’t make sense to plot characteristics
with different units or fractions in the same distribution.

``` r
# trusty field values table - lets just look at the first few entries with the most associated records
compid <- TADA_FieldValuesTable(dataset_cens, field = "TADA.ComparableDataIdentifier")
```

Now that we have an idea for what the TADA.ComparableDataIdentifier
looks like, we can check out how it is used to plot distinct
characteristic groups.

``` r
# Look at a histogram, boxplot, and stats for TADA.ComparableDataIdentifier(s) of your choice.
comp_data_id <- "PH_NA_NA_NONE"

plot_data <- subset(dataset_cens, dataset_cens$TADA.ComparableDataIdentifier %in% comp_data_id)
```

**Question 10: How does selecting the different options on the left side
of the histogram change the data displayed? When might you want to use a
histogram vs. a boxplot?**

Let’s take a look at the histogram and boxplot for the comparable data
identifier we selected.

``` r
TADA_Histogram(plot_data, id_cols = "TADA.ComparableDataIdentifier")
```

``` r
TADA_Boxplot(plot_data, id_cols = "TADA.ComparableDataIdentifier")
```

``` r
stats <- TADA_Stats(plot_data)
```

We can also explore depth profiles for selected characteristics at
specific site on a single date. There are a few functions that can help
with this. First we can use `TADA_FlagDepthCategory` to place results
into various depth categories (surface, middle, and bottom).

``` r
dataset_depth <- TADA_FlagDepthCategory(dataset_cens)
```

    ## [1] "TADA_FlagDepthCategory: checking data set for depth values. 59531 results have depth values available."
    ## [1] "TADA_FlagDepthCategory: assigning depth categories."
    ## [1] "TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for entire water column."
    ## [1] "TADA_FlagDepthCategory: No aggregation performed."

We can also use another function, `TADA_IDDepthProfiles` to identify
location/date/characteristic combinations in the data set that can be
used for depth profile plots or analysis. The default number of values
required to identify a location/date/characteristic as a depth profile
is 2, but this can be changed by the user. We will specify a larger
value, 5, so that any depth profiles identified will have results from
at least 5 different depths.

``` r
depth_profile_id <- TADA_IDDepthProfiles(dataset_depth, nvalue = 5)
```

**Question 11: How can TADA_IDDepthProfiles() help users use
TADA_DepthProfilePlot most efficiently?**

Now, we can use `TADA_DepthProfilePlot` to plot up to three
characteristics against depth. In this example, we will look at pH,
secchi depth, and pH.

``` r
TADA_DepthProfilePlot(dataset_cens,
  groups = c(
    "TEMPERATURE, WATER_NA_NA_DEG C",
    "DEPTH, SECCHI DISK DEPTH_NA_NA_M",
    "PH_NA_NA_NONE"
  ),
  location = "REDLAKE_WQX-ANKE",
  activity_date = "2018-10-04",
  depthcat = TRUE,
  surfacevalue = 2,
  bottomvalue = 2,
  unit = "m"
)
```

    ## [1] "TADA_DepthProfilePlot: Running TADA_FlagDepthCategory function to add required columns to data frame"
    ## [1] "TADA_FlagDepthCategory: checking data set for depth values. 59531 results have depth values available."
    ## [1] "TADA_FlagDepthCategory: assigning depth categories."
    ## [1] "TADA_FlagDepthCategory: Grouping results by TADA.MonitoringLocationIdentifier, OrganizationIdentifier, CharacteristicName, and ActivityStartDate for aggregation for entire water column."
    ## [1] "TADA_FlagDepthCategory: No aggregation performed."
    ## [1] "TADA_DepthProfilePlot: Depth unit in data set matches depth unit specified by user for plot. No conversion necessary."
    ## [1] "TADA_DepthProfilePlot: Identifying available depth profile data."
    ## [1] "TADA_DepthProfilePlot: Any results for DEPTH, SECCHI DISK DEPTH, DEPTH, SECCHI DISK DEPTH (CHOICE LIST), DEPTH, SECCHI DISK DEPTH REAPPEARS, DEPTH, DATA-LOGGER (NON-PORTED), DEPTH, DATA-LOGGER (PORTED), RBP STREAM DEPTH - RIFFLE, RBP STREAM DEPTH - RUN, THALWEG DEPTH match the depth unit selected for the figure."
    ## [1] "TADA_DepthProfilePlot: Adding surface delination to figure."
    ## [1] "TADA_DepthProfilePlot: Adding bottom delination to figure."

Finally, we can download our PASS and FAIL data sets together into an
Excel spreadsheet.

``` r
dataset_and_removed <- dplyr::bind_rows(dataset_cens, removed)

# Un-comment to download Excel spreadsheet to your working directory
# install.packages(writexl)
# library(writexl)
# writexl::write_xlsx(dataset_and_removed, "NCTCShepherdstownData.xlsx")
```

## TADA R Shiny Modules

Finally, take a look at an alternative workflow for QC’ing WQP data:
TADA Shiny Module 1: Data Discovery and Cleaning. This is a Shiny
application that runs many of the TADA functions covered in this
training document behind a graphical user interface. The shiny
application queries the WQP, contains maps and data visualizations,
flags suspect data results, handles censored data, and more. You can
launch it using the code below.

``` r
# download TADA Shiny repository
remotes::install_github("USEPA/TADAShiny", ref = "develop", dependencies = TRUE)

# launch the app locally.
TADAShiny::run_app()
```

DRAFT [Module 1](https://rconnect-public.epa.gov/TADAShiny/) is also
currently hosted on the web with minimal server memory/storage
allocated.
