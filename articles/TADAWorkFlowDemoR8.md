# TADA R8 Demo January 2026

## Overview and Setup

**Welcome!**

Thank you for your interest in Tools for Automated Data Analysis (TADA).
TADA is an open-source tool set built in the R programming language.
This [RMarkdown](https://bookdown.org/yihui/rmarkdown/) document walks
users through how to download the TADA R package from GitHub, access and
parameterize several important functions, and create basic
visualizations with a sample data set.

**Note:** TADA is still under development. New functionality is added
weekly, and sometimes we need to make bug fixes in response to tester
and user feedback. We appreciate your feedback, patience, and interest
in these helpful tools.

If you are interested in contributing to TADA development, more
information is available at:

[Contributing](https://usepa.github.io/EPATADA/articles/CONTRIBUTING.html)

We welcome collaboration with external partners.

Install and load packages

First, install and load the remotes package specifying the repo. This is
needed before installing TADA because it is only available on GitHub.

``` r
install.packages("remotes",
  repos = "http://cran.us.r-project.org"
)
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
```

Finally, use the library() function to load the TADA R Package into your
R session.

``` r
library(EPATADA)
```

Help pages

All TADA R package functions have their own individual help pages,
listed on the Function reference page on the TADA GitHub site. Users can
also access the help page for a given function in R or RStudio using the
following format (example below): ?\[name of TADA function\].

``` r
# Access help page for TADA_DataRetrieval
?TADA_DataRetrieval
```

### A) Retrieving, filtering and cleaning data from the WQP

Query the WQP using TADA_DataRetrieval. TADA_AutoClean is a powerful
function that runs as part of TADA_DataRetrieval when applyautoclean =
TRUE. It performs a variety of tasks, for example:

1.  creating new “TADA” prefixed columns and and capitalizing their
    contents to reduce case sensitivity issues,

2.  converts special characters in value columns,

3.  converts latitude and longitude values to numeric,

4.  replaces “meters” with “m”,

5.  replaces deprecated characteristic names with current WQX names,

6.  harmonizes result and detection limit units,

7.  harmonizes depth units to meters, and

8.  creates the column TADA.ComparableDataIdentifier by concatenating
    characteristic name, result sample fraction, method speciation, and
    result measure unit.

In this example, we will get dissolved oxygen concentration, Escherichia
coli, and pH data from January 2020 to December 2022 (two years) from
Missoula County, Montana.

``` r
tada.MT <- TADA_DataRetrieval(
  startDate = "2020-01-01",
  endDate = "2022-12-31",
  statecode = "MT",
  characteristicName = c(
    "Dissolved oxygen (DO)",
    "Escherichia coli",
    "pH"
  ),
  countycode = "Missoula County",
  applyautoclean = TRUE,
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
    ## [1] "Dissolved oxygen (DO)" "Escherichia coli"      "pH"                   
    ## 
    ## $endDate
    ## [1] "2022-12-31"
    ## 
    ## [1] "Data successfully downloaded. Running TADA_AutoClean function."
    ## [1] "TADA_Autoclean: creating TADA-specific columns."
    ## [1] "TADA_Autoclean: harmonizing dissolved oxygen characterisic name to DISSOLVED OXYGEN SATURATION if unit is % or % SATURATN."
    ## [1] "TADA_Autoclean: handling special characters and coverting TADA.ResultMeasureValue and TADA.DetectionQuantitationLimitMeasure.MeasureValue value fields to numeric."
    ## [1] "TADA_Autoclean: converting TADA.LatitudeMeasure and TADA.LongitudeMeasure fields to numeric."
    ## [1] "TADA_Autoclean: harmonizing synonymous unit names (m and meters) to m."
    ## [1] "TADA_Autoclean: updating deprecated (i.e. retired) characteristic names."
    ## [1] "No deprecated characteristic names found in dataset."
    ## [1] "TADA_Autoclean: harmonizing result and depth units."
    ## [1] "TADA_Autoclean: creating TADA.ComparableDataIdentifier field for use when generating visualizations and analyses."
    ## [1] "NOTE: This version of the TADA package is designed to work with numeric data with media name: 'WATER'. TADA_AutoClean does not currently remove (filter) data with non-water media types. If desired, the user must make this specification on their own outside of package functions. Example: dplyr::filter(.data, TADA.ActivityMediaName == 'WATER')"

#### Flag, clean, and visualize

Now, let’s use EPATADA functions to review, visualize, and whittle the
returned WQP data down to include only results that are applicable to
our water quality analysis and area of interest.

Create a pie chart to display the count of results for each
TADA.CharacteristicName.

``` r
TADA_FieldValuesPie(
  tada.MT,
  field = "TADA.CharacteristicName",
  characteristicName = "null"
)
```

![](TADAWorkFlowDemoR8_files/figure-html/review_chars-1.png)

TADA is primarily designed to accommodate water data from the WQP. Let’s
see what activity media types are represented in the data set.

Are there any media types that are not water?

``` r
# Create table with count for each ActivityMediaName
media <- TADA_FieldValuesTable(
  tada.MT,
  field = "ActivityMediaName"
)

DT::datatable(media, fillContainer = TRUE)
```

Create an overview map. For each site, we can view the measurement
count, visit count and characteristic counts along with the site ID,
site name, and organization name.

``` r
TADA_OverviewMap(tada.MT)
```

Let’s take a quick look at all unique values in the
MonitoringLocationIdentifier column and see how how many results are
associated with each.

``` r
# use TADA_FieldValuesTable to create a table of the number of results per MonitoringLocationIdentifier
sites <- TADA_FieldValuesTable(
  tada.MT,
  field = "MonitoringLocationIdentifier"
)

DT::datatable(sites, fillContainer = TRUE)
```

What about OrganizationFormalName?

``` r
# use TADA_FieldValuesTable to create a table of the number of results per MonitoringLocationIdentifier
orgs <- TADA_FieldValuesTable(
  tada.MT,
  field = "OrganizationFormalName",
)

DT::datatable(orgs, fillContainer = TRUE)
```

Next, let’s check if the dataset contains potential duplicate results
from within a single organization or from within multiple organizations
(such as when two or more organizations monitor the same location and
may submit duplicate results).

If potential duplicates from multiple organizations are found, you can
choose to prioritize results from one organization over another, this
can be done using the org_hierarchy argument in
`TADA_FindPotentialDuplicatesMultipleOrgs`.

``` r
# find duplicates from single org
tada.MT.singleorg.dups <- TADA_FindPotentialDuplicatesSingleOrg(tada.MT)
```

    ## [1] "TADA_FindPotentialDuplicatesSingleOrg: 18 groups of potentially duplicated results found in dataset. These have been placed into duplicate groups in the TADA.SingleOrgDupGroupID column and the function randomly selected one result from each group to represent a single, unduplicated value. Selected values are indicated in the TADA.SingleOrgDup.Flag as 'Unique', while duplicates are flagged as 'Duplicate' for easy filtering."

``` r
# Review organizations. You can select one to prioritize in TADA_FindPotentialDuplicatesMultipleOrgs
unique(tada.MT.singleorg.dups$OrganizationIdentifier)
```

    ## [1] "USGS-MT"      "USGS-CO"      "NARS_WQX"     "MDEQ_WQ_WQX"  "TSWQC_WQX"   
    ## [6] "MTWTRSHD_WQX" "MTVOLWQM_WQX"

``` r
unique(tada.MT.singleorg.dups$OrganizationFormalName)
```

    ## [1] "USGS Montana Water Science Center"           
    ## [2] "USGS Colorado Water Science Center"          
    ## [3] "EPA National Aquatic Resources Survey (NARS)"
    ## [4] "Montana DEQ WQPB"                            
    ## [5] "Tri-State Water Quality Council"             
    ## [6] "Montana Watershed"                           
    ## [7] "Montana Volunteer Water Quality Monitoring"

``` r
# find duplicates across multiple orgs
tada.MT.multipleorgs.dups <- TADA_FindPotentialDuplicatesMultipleOrgs(
  tada.MT.singleorg.dups
)
```

    ## [1] "Data after CRS assignment:"
    ##    TADA.MonitoringLocationIdentifier
    ## 1                      USGS-12334550
    ## 2                      USGS-12340500
    ## 3                      USGS-12340000
    ## 4               USGS-463828114364600
    ## 5               USGS-470202113592200
    ## 6              NARS_WQX-NWC_MT-10248
    ## 7              NARS_WQX-NWC_MT-10184
    ## 8             MDEQ_WQ_WQX-C04CKFKR05
    ## 9             MDEQ_WQ_WQX-C04KNDYC04
    ## 10            MDEQ_WQ_WQX-C04KNDYC02
    ## 11            MDEQ_WQ_WQX-C04KNDYC54
    ## 12            MDEQ_WQ_WQX-C04KNDYC57
    ## 13            MDEQ_WQ_WQX-C04KNDYC62
    ## 14            MDEQ_WQ_WQX-C04KNDYC01
    ## 15                TSWQC_WQX-CFRPO-12
    ## 16              TSWQC_WQX-CFRPO-15.5
    ## 17                TSWQC_WQX-CFRPO-18
    ## 18                TSWQC_WQX-CFRPO-22
    ## 19            MTWTRSHD_WQX-COMBITR02
    ## 20            MDEQ_WQ_WQX-C05BITRR02
    ## 21            MDEQ_WQ_WQX-C05BITRR34
    ## 22            MDEQ_WQ_WQX-C05BITRR13
    ## 23            MDEQ_WQ_WQX-C05LOWFC03
    ## 24            MDEQ_WQ_WQX-C10GLCRC06
    ## 25               TSWQC_WQX-CFRPO-22B
    ## 26              MTWTRSHD_WQX-HOLLAND
    ## 27            MTWTRSHD_WQX-LINDBERGH
    ## 28            MTVOLWQM_WQX-CLEARWR_1
    ## 29          MTVOLWQM_WQX-MORRELLC-04
    ## 30            MTVOLWQM_WQX-SEELEY_10
    ## 31            MTVOLWQM_WQX-SEELEY_11
    ## 32            MTVOLWQM_WQX-SEELEY_12
    ## 33            MTVOLWQM_WQX-SEELEY_13
    ## 34             MTVOLWQM_WQX-SEELEY_2
    ## 35             MTVOLWQM_WQX-SEELEY_3
    ## 36             MTVOLWQM_WQX-SEELEY_6
    ## 37             MTVOLWQM_WQX-SEELEY_9
    ## 38            MTVOLWQM_WQX-SEELEYLKM
    ## 39               MTVOLWQM_WQX-ALVA_3
    ## 40            MTVOLWQM_WQX-BIG SKY_3
    ## 41        MTVOLWQM_WQX-CLEARWATERR_1
    ## 42       MTVOLWQM_WQX-CLEARWATERR_10
    ## 43               MTVOLWQM_WQX-INEZ_4
    ## 44               MTVOLWQM_WQX-INEZ_5
    ## 45               MTVOLWQM_WQX-INEZ_6
    ## 46          MTVOLWQM_WQX-MORRELLC-01
    ## 47             MTVOLWQM_WQX-PLACID_3
    ## 48             MTVOLWQM_WQX-PLACID_5
    ## 49             MTVOLWQM_WQX-PLACID_6
    ## 50             MTVOLWQM_WQX-SALMON_5
    ## 51             MTVOLWQM_WQX-SALMON_6
    ## 52             MTVOLWQM_WQX-SALMON_7
    ## 53             MTVOLWQM_WQX-SALMON_8
    ## 54             MTVOLWQM_WQX-SEELEY_1
    ## 55             MTVOLWQM_WQX-SEELEY_4
    ## 56             MTVOLWQM_WQX-SEELEY_5
    ## 57             MTVOLWQM_WQX-SEELEY_7
    ## 58             MTVOLWQM_WQX-SEELEY_8
    ##                                   TADA.MonitoringLocationName
    ## 1                                                        <NA>
    ## 2                                                        <NA>
    ## 3                                                        <NA>
    ## 4                                                        <NA>
    ## 5                                                        <NA>
    ## 6                                                NWC_MT-10248
    ## 7                                                NWC_MT-10184
    ## 8              CLARK FORK RIVER AT KONA BRIDGE FISHING ACCESS
    ## 9  KENNEDY CREEK ABOVE MOUTH, UPSTREAM OF NINEMILE CREEK ROAD
    ## 10     KENNEDY CREEK 2 MILES ABOVE MOUTH BELOW MINING COMPLEX
    ## 11                    KENNEDY CREEK DOWNSTREAM OF NUGGET MINE
    ## 12           KENNEDY CREEK JUST DOWNSTREAM OF LOST CABIN MINE
    ## 13                  KENNEDY CREEK DOWNSTREAM OF HAUTILLA MINE
    ## 14                     KENNEDY CREEK UPSTREAM OF FR ROAD 5507
    ## 15                                 CLARK FORK RIVER AT BONITA
    ## 16                            CLARK FORK RIVER ABOVE MISSOULA
    ## 17                            CLARK FORK RIVER BELOW MISSOULA
    ## 18                                  CLARK FORK RIVER AT HUSON
    ## 19                       BITTERROOT RIVER AT BUCKHOUSE BRIDGE
    ## 20                       BITTERROOT RIVER AT BUCKHOUSE BRIDGE
    ## 21                          BITTERROOT RIVER ABOVE LOLO CREEK
    ## 22                 BITTERROOT RIVER BELOW TWO CHANNEL SECTION
    ## 23                  LOLO CREEK WF 900 FT BELOW W LEE CREEK RD
    ## 24                    GLACIER CREEK AT GLACIER LAKE TRAILHEAD
    ## 25                                  CLARK FORK RIVER AT HUSON
    ## 26                                  HOLLAND LAKE AT DEEP SITE
    ## 27                                LINDBERGH LAKE AT DEEP SITE
    ## 28               CLEARWATER RIVER AT RIVERVIEW/DOGTOWN BRIDGE
    ## 29                   MORRELL CREEK AT MT83 BRIDGE, NEAR MOUTH
    ## 30                            SEELEY LAKE AT DEER CREEK INLET
    ## 31                     SEELEY LAKE NEAR FOREST SERVICE CABINS
    ## 32                          SEELEY LAKE AT SEELEY CREEK INLET
    ## 33                             SEELEY LAKE AT S BAY NEAR DOCK
    ## 34                             SEELEY LAKE AT SW BAY OFF C ST
    ## 35                               SEELEY LAKE AT MONTANA PINES
    ## 36                 SEELEY LAKE AT RANGER STATION (RICE CREEK)
    ## 37                      SEELEY LAKE AT CLEARWATER RIVER INLET
    ## 38                                   SEELEY LAKE MIDDLE BASIN
    ## 39                        LAKE ALVA AT CAMPGROUND BOAT LAUNCH
    ## 40      BIG SKY LAKE (FISH LAKE) AT BOAT LAUNCH BY FISH CREEK
    ## 41                     CLEARWATER RIVER AT MOUTH AT HWY 200 E
    ## 42                CLEARWATER RIVER AT HWY 83 ABOVE RAINY LAKE
    ## 43                  LAKE INEZ OFF HWY 83 LOWER E SIDE OF LAKE
    ## 44                       LAKE INEZ OFF HELMS RD S END OF LAKE
    ## 45                  LAKE INEZ AT BOAT LAUNCH ON N END OF LAKE
    ## 46                       MORRELL CREEK AT AIRPORT ROAD BRIDGE
    ## 47                      PLACID LAKE AT PLACID LAKE CAMPGROUND
    ## 48                           PLACID LAKE OFF PLACID LAKE RD S
    ## 49           PLACID LAKE AT PLACID LK RD S BRIDGE (VAUGHN CK)
    ## 50                     SALMON LAKE NEAR HWY 83, N BOAT LAUNCH
    ## 51                 SALMON LAKE AT UPPER STATE PARK CAMPGROUND
    ## 52                 SALMON LAKE AT LOWER STATE PARK CAMPGROUND
    ## 53                  SALMON LAKE AT SOUTHERN PULLOFF OF HWY 83
    ## 54                                   SEELEY LAKE AT SOUTH BAY
    ## 55             SEELEY LAKE AT SL CAMPGROUND NEAR RIVER OUTLET
    ## 56                           SEELEY LAKE AT TAMARACK'S RESORT
    ## 57             SEELEY LAKE AT N END OF RIVER POINT CAMPGROUND
    ## 58               SEELEY LAKE AT S END OF BIG LARCH CAMPGROUND
    ##    TADA.LongitudeMeasure TADA.LatitudeMeasure
    ## 1              -113.8140             46.82591
    ## 2              -113.9321             46.87676
    ## 3              -113.7563             46.89941
    ## 4              -114.6128             46.64111
    ## 5              -113.9894             47.03389
    ## 6              -113.7437             47.54247
    ## 7              -113.6899             47.40631
    ## 8              -114.1513             46.89890
    ## 9              -114.4911             47.12308
    ## 10             -114.4784             47.13241
    ## 11             -114.4463             47.14579
    ## 12             -114.4451             47.14740
    ## 13             -114.4427             47.15102
    ## 14             -114.4224             47.16489
    ## 15             -113.5886             46.71770
    ## 16             -113.9757             46.86430
    ## 17             -114.0620             46.87450
    ## 18             -114.3429             47.03330
    ## 19             -114.0531             46.83194
    ## 20             -114.0541             46.83012
    ## 21             -114.0636             46.75559
    ## 22             -114.0456             46.69060
    ## 23             -114.5529             46.68958
    ## 24             -113.7928             47.38133
    ## 25             -114.2874             47.01060
    ## 26             -113.5996             47.44784
    ## 27             -113.7355             47.38825
    ## 28             -113.4906             47.16593
    ## 29             -113.4653             47.14588
    ## 30             -113.5297             47.20860
    ## 31             -113.5196             47.19705
    ## 32             -113.4895             47.17984
    ## 33             -113.4805             47.17403
    ## 34             -113.4924             47.17333
    ## 35             -113.5077             47.19337
    ## 36             -113.5219             47.21285
    ## 37             -113.5268             47.21183
    ## 38             -113.5045             47.18636
    ## 39             -113.5848             47.32309
    ## 40             -113.3824             47.11166
    ## 41             -113.3820             47.00000
    ## 42             -113.5880             47.34700
    ## 43             -113.5613             47.27849
    ## 44             -113.5687             47.27148
    ## 45             -113.5690             47.29438
    ## 46             -113.4693             47.17322
    ## 47             -113.5045             47.11858
    ## 48             -113.5188             47.10775
    ## 49             -113.5026             47.11501
    ## 50             -113.4239             47.10886
    ## 51             -113.4002             47.09444
    ## 52             -113.3944             47.08636
    ## 53             -113.3847             47.07165
    ## 54             -113.4816             47.17368
    ## 55             -113.5152             47.18986
    ## 56             -113.5124             47.20358
    ## 57             -113.5142             47.18813
    ## 58             -113.4909             47.18108
    ##    HorizontalCoordinateReferenceSystemDatumName epsg      lat       lon
    ## 1                                         NAD83 4269 46.82591 -113.8140
    ## 2                                         NAD83 4269 46.87676 -113.9321
    ## 3                                         NAD83 4269 46.89941 -113.7563
    ## 4                                         NAD83 4269 46.64111 -114.6128
    ## 5                                         NAD83 4269 47.03389 -113.9894
    ## 6                                         NAD83 4269 47.54247 -113.7437
    ## 7                                         NAD83 4269 47.40631 -113.6899
    ## 8                                         NAD83 4269 46.89890 -114.1513
    ## 9                                         NAD27 4267 47.12308 -114.4911
    ## 10                                        NAD27 4267 47.13241 -114.4784
    ## 11                                        NAD83 4269 47.14579 -114.4463
    ## 12                                        NAD83 4269 47.14740 -114.4451
    ## 13                                        NAD83 4269 47.15102 -114.4427
    ## 14                                        NAD27 4267 47.16489 -114.4224
    ## 15                                        UNKWN 4326 46.71770 -113.5886
    ## 16                                        UNKWN 4326 46.86430 -113.9757
    ## 17                                        UNKWN 4326 46.87450 -114.0620
    ## 18                                        UNKWN 4326 47.03330 -114.3429
    ## 19                                        NAD83 4269 46.83194 -114.0531
    ## 20                                        NAD83 4269 46.83012 -114.0541
    ## 21                                        NAD83 4269 46.75559 -114.0636
    ## 22                                        UNKWN 4326 46.69060 -114.0456
    ## 23                                        NAD83 4269 46.68958 -114.5529
    ## 24                                        NAD83 4269 47.38133 -113.7928
    ## 25                                        UNKWN 4326 47.01060 -114.2874
    ## 26                                        NAD83 4269 47.44784 -113.5996
    ## 27                                        NAD83 4269 47.38825 -113.7355
    ## 28                                        WGS84 4326 47.16593 -113.4906
    ## 29                                        WGS84 4326 47.14588 -113.4653
    ## 30                                        WGS84 4326 47.20860 -113.5297
    ## 31                                        WGS84 4326 47.19705 -113.5196
    ## 32                                        WGS84 4326 47.17984 -113.4895
    ## 33                                        WGS84 4326 47.17403 -113.4805
    ## 34                                        WGS84 4326 47.17333 -113.4924
    ## 35                                        WGS84 4326 47.19337 -113.5077
    ## 36                                        WGS84 4326 47.21285 -113.5219
    ## 37                                        WGS84 4326 47.21183 -113.5268
    ## 38                                        UNKWN 4326 47.18636 -113.5045
    ## 39                                        WGS84 4326 47.32309 -113.5848
    ## 40                                        WGS84 4326 47.11166 -113.3824
    ## 41                                        WGS84 4326 47.00000 -113.3820
    ## 42                                        WGS84 4326 47.34700 -113.5880
    ## 43                                        WGS84 4326 47.27849 -113.5613
    ## 44                                        WGS84 4326 47.27148 -113.5687
    ## 45                                        WGS84 4326 47.29438 -113.5690
    ## 46                                        WGS84 4326 47.17322 -113.4693
    ## 47                                        WGS84 4326 47.11858 -113.5045
    ## 48                                        WGS84 4326 47.10775 -113.5188
    ## 49                                        WGS84 4326 47.11501 -113.5026
    ## 50                                        WGS84 4326 47.10886 -113.4239
    ## 51                                        WGS84 4326 47.09444 -113.4002
    ## 52                                        WGS84 4326 47.08636 -113.3944
    ## 53                                        WGS84 4326 47.07165 -113.3847
    ## 54                                        WGS84 4326 47.17368 -113.4816
    ## 55                                        WGS84 4326 47.18986 -113.5152
    ## 56                                        WGS84 4326 47.20358 -113.5124
    ## 57                                        WGS84 4326 47.18813 -113.5142
    ## 58                                        WGS84 4326 47.18108 -113.4909
    ## [1] "Processing CRS: NAD27"
    ## [1] "Processing CRS: NAD83"
    ## [1] "Processing CRS: UNKWN"
    ## [1] "Processing CRS: WGS84"

    ## [1] "TADA_FindNearbySites: No org_hierarchy supplied by user. Organization will not be taken into account during metadata selection."
    ## [1] "No duplicate results detected. Returning input dataframe with duplicate flagging columns set to 'N'."

We will select to keep only unique samples from
`TADA_FindPotentialDuplicatesSingleOrg` where TADA.SingleOrgDup.Flag
equals “Unique”.

``` r
tada.MT.clean <- tada.MT.multipleorgs.dups |>
  dplyr::filter(TADA.SingleOrgDup.Flag == "Unique") |>
  dplyr::filter(TADA.ResultSelectedMultipleOrgs == "Y")
```

Remove intermediate variables in R by using ‘rm()’. In the remainder of
this workshop, we will work with the clean data set.

``` r
to_rm <- c("tada.MT.nearby.flag.dups","tada.MT.nearby","tada.MT.filter")
rm(list = to_rm[to_rm %in% ls()])
```

Censored data are measurements for which the true value is not known,
but we can estimate the value based on known lower or upper detection
conditions and limit types. TADA fills missing *TADA.ResultMeasureValue*
and *TADA.ResultMeasure.MeasureUnitCode* values with values and units
from *TADA.DetectionQuantitationLimitMeasure.MeasureValue* and
*TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode*, respectively,
using the `TADA_AutoClean` function.

The TADA package currently has functions that summarize censored data
incidence in the dataset and perform simple substitutions of censored
data values, including x times the detection limit and random selection
of a value between 0 and the detection limit. The user may specify the
methods used for non-detects and over-detects separately in the input to
the `TADA_SimpleCensoredMethods` function. The next step we take in this
example is to perform simple conversions to the censored data in the
dataset: we keep over-detects as is (no conversion made) and convert
non-detect values to 0.5 times the detection limit (half the detection
limit).

In this example, no censored data results were found. The input
tada.MT.clean data is returned.

``` r
tada.MT.clean <- TADA_SimpleCensoredMethods(
  tada.MT.clean,
  nd_method = "multiplier",
  nd_multiplier = 0.5,
  od_method = "as-is",
  od_multiplier = "null"
)
```

    ## [1] "TADA_FlagMeasureQualifierCode: Dataframe does not include any information (all NAs) in MeasureQualifierCode."
    ## [1] "TADA_IDCensoredData: No censored data detected in your dataframe. Returning input dataframe with new column TADA.CensoredData.Flag set to Uncensored"
    ## [1] "Cannot apply simple censored methods to dataframe with no censored data results. Returning input dataframe."

`TADA_FindQCActivities` identifies results with QA/QC identifies results
with QA/QC ActivityTypeCodes. When clean = TRUE, it removes QA/QC
results.

``` r
tada.MT.clean <- TADA_FindQCActivities(tada.MT.clean, clean = TRUE)
```

    ## [1] "TADA_FindQCActivities: Quality control samples have been removed or were not present in the input dataframe. Returning dataframe with TADA.ActivityType.Flag column for tracking."

TADA_RunKeyFlagFunctions is a shortcut function to run important TADA
flagging functions. See ?function documentation for TADA_FlagResultUnit,
TADA_FlagFraction, TADA_FlagMeasureQualifierCode, and
TADA_FlagSpeciation for more information.

``` r
tada.MT.clean <- TADA_RunKeyFlagFunctions(
  tada.MT.clean,
  clean = TRUE
)
```

    ## [1] "TADA_FindQCActivities: Quality control samples have been removed or were not present in the input dataframe. Returning dataframe with TADA.ActivityType.Flag column for tracking."
    ## [1] "TADA_FlagMeasureQualifierCode: Dataframe does not include any information (all NAs) in MeasureQualifierCode."

The functions TADA_FlagAboveThreshold and TADA_FlagBelowThreshold are
used to flag results falling above and below the WQX national
thresholds, respectively. When clean = TRUE, the flagged results are
removed from the TADA data frame.

``` r
tada.MT.clean <- TADA_FlagAboveThreshold(tada.MT.clean, clean = TRUE, flaggedonly = FALSE)

tada.MT.clean <- TADA_FlagBelowThreshold(tada.MT.clean, clean = TRUE, flaggedonly = FALSE)
```

### B) Create an Assessment Unit/WQP Monitoring Location crosswalk.

The **`TADA_CreateAUMLCrosswalk`** function efficiently creates a
crosswalk between ATTAINS assessment units and WQP monitoring locations.
It uses three prioritized data sources:

1.  **User-Supplied Crosswalk:** An optional crosswalk provided by the
    user (e.g., see example **`user.supplied.cw`** in next code chunk).

2.  **ATTAINS Crosswalk:** Utilizes **`TADA_GetATTAINSAUMLCrosswalk`**
    to incorporate crosswalk information stored by participating
    organizations in ATTAINS. There may not be an ATTAINS crosswalk
    available for all organizations as storing this information in
    ATTAINS is optional for states and some tribes are still in the
    process of developing their crosswalks.

3.  **ATTAINS Catchments/Geospatial Join:** Employs
    **`TADA_CreateATTAINSAUMLCrosswalk`** to connect monitoring
    locations to assessment units using ATTAINS catchments through a
    geospatial join. This function converts WQP monitoring locations
    into a geospatial sf object and associates them with their
    intersecting NHDPlus high resolution catchments containing
    [entity-defined assessment units in
    ATTAINS](https://www.epa.gov/waterdata/integrated-reporting-georeferencing-pilot-report).

**Process**

- The function prioritizes these sources in the order listed. It
  automatically attempts to assign unassigned monitoring locations using
  the next available data source. For example, if any monitoring
  locations remain unassigned after checking the user-supplied
  crosswalk, the function will check the ATTAINS crosswalk, then the
  geospatial join, as needed.

For demonstration purposes, let’s assume we know for sure this
MonitoringLocationIdentifier, “MTWTRSHD_WQX-COMBITR02” should be
associated with the AssessmentUnitIdentifier “MT76M001_020”. Let’s
create a crosswalk to assign “MTWTRSHD_WQX-COMBITR02” to the assessment
unit “MT76M001_020” and give it the WaterType “RIVER”.

#### Define a user supplied crosswalk of Assessment Units and Monitoring Location(s)

``` r
# user would like to associate this MonitoringLocationIdentifier to this AU.
user.supplied.cw <- data.frame(
  AssessmentUnitIdentifier = "MT76M001_020",
  MonitoringLocationIdentifier = "MTWTRSHD_WQX-COMBITR02",
  WaterType = "RIVER"
)

DT::datatable(user.supplied.cw, fillContainer = TRUE)
```

#### Run `TADA_GetATTAINSCrosswalk`

Now, let’s check to see if MTDEQ has submitted a prior crosswalk to
ATTAINS for the assessment unit identifier “MT76M001_020”.

Does our TADA data frame contain any of these monitoring locations?

``` r
ATTAINS.cw <- TADA_GetATTAINSAUMLCrosswalk(
  org_id = "MTDEQ"
) |>
  dplyr::filter(ATTAINS.AssessmentUnitIdentifier == "MT76M001_020")
```

    ## [1] "TADA_GetATTAINSAUMLCrosswalk: There are 5530 monitoring location identifiers associated with assessment units for MTDEQ in ATTAINS."

``` r
any(unique(tada.MT.clean$MonitoringLocationIdentifier) %in% ATTAINS.cw$ATTAINS.MonitoringLocationIdentifier)
```

    ## [1] FALSE

#### Run `TADA_CreateAUMLCrosswalk`

We are uncertain about any other assignments of assessment units to
monitoring locations. Let’s match the monitoring locations catchments
overlay from tada.MT.clean with assessment units catchments with a
geospatial join from ATTAINS by using TADA_CreateATTAINSAUMLCrosswalk to
identify which assessment units intersect with each monitoring location.

Recall, TADA_CreateATTAINSAUMLCrosswalk prioritizes the user supplied
crosswalk, user.supplied.cw. If any monitoring locations remain
unassigned after checking the user-supplied crosswalk, the function will
check the ATTAINS crosswalk (see ATTAINS.cw above), then lastly, the
geospatial join, as needed.

``` r
# make AU assignments for unassigned MLs
MT.AUMLRef <- TADA_CreateAUMLCrosswalk(
  tada.MT.clean,
  au_ref = user.supplied.cw,
  org_id = "MTDEQ",
  fill_ATTAINS_catch = TRUE,
  return_nearest = TRUE,
  batch_upload = FALSE,
)
```

    ## [1] "TADA_CreateAUMLCrosswalk: fetching ATTAINS geospatial data for assessment units in the user-supplied crosswalk."

    ## [1] "TADA_CreateAUMLCrosswalk: checking for crosswalk in ATTAINS."
    ## [1] "TADA_CreateAUMLCrosswalk: There are 5530 MonitoringLocation records in ATTAINS for MTDEQ."
    ## [1] "TADA_CreateAUMLCrosswalk: crosswalk from ATTAINS has been imported."
    ## [1] "TADA_CreateAUMLCrosswalk: checking to see if any unmatched monitoring locations remain in the original TADA data frame."
    ## [1] "TADA_CreateAUMLCrosswalk: using TADA_CreateATTAINSAUMLCrosswalk to match remaining monitoring locations to ATTAINS assessment units using a spatial join (EPA snapshot of NHDPlus HR catchments associated with entity submitted assessment unit features). Also returning USGS snapshot of NHDPlus V2 HR for monitoring locations not near any ATTAINS assessment unit."
    ## [1] "TADA_CreateAUMLCrosswalk: joining results to return list of dataframes compatible with TADA_ViewATTAINS."

The output of TADA_CreateAUMLCrosswalk is a list of data frames: 1)
“TADA_with_ATTAINS”  
2) “ATTAINS_catchments”  
3) “ATTAINS_points”  
4) “ATTAINS_lines” 5) “ATTAINS_polygons”  
6) “ATTAINS_crosswalk”

The data frame, TADA_with_ATTAINS contains all of the clean TADA data as
well as the added ATTAINS columns. The other ATTAINS_catchments,
ATTAINS_points, ATTAINS_lines, and ATTAINS_polygons data frames contain
geometry for mapping. The ATTAINS_crosswalk data frame contains a simple
crosswalk of monitoring locations and assessment units.

Now, let’s view the assessment units and monitoring locations on a map
to review the assessment unit/monitoring location assignments.

``` r
TADA_ViewATTAINS(MT.AUMLRef, ref_icons = TRUE)
```

### C) Assigning uses to assessment units.

Let’s filter our TADA data frame to a single assessment unit
“MT76M001_020”, and for a single TADA.CharacteristicName, “DISSOLVED
OXYGEN (DO)” for demonstration purposes for the remainder of this
example.

``` r
# get subset DO data for one AU
tada.MT.clean.DO <- MT.AUMLRef$TADA_with_ATTAINS |>
  sf::st_drop_geometry() |>
  dplyr::filter(TADA.CharacteristicName == "DISSOLVED OXYGEN (DO)",
    ATTAINS.AssessmentUnitIdentifier == "MT76M001_020")

tada.MT.AUMLRef <- MT.AUMLRef$ATTAINS_crosswalk |>
  dplyr::filter(ATTAINS.AssessmentUnitIdentifier == "MT76M001_020")
```

#### Retrieve Uses from Prior ATTAINS Assessment Cycles

Let’s retrieve the use names assigned to this example assessment unit
from a prior ATTAINS assessment cycle for MTDEQ. Users can append
additional rows or remove use names as needed. See the TADA Module 2:
Geospatial Functions vignette for more information.

``` r
AU.uses <- TADA_AssignUsesToAU(
  tada.MT.clean.DO,
  org_id = "MTDEQ",
  AUMLRef = tada.MT.AUMLRef
)
```

    ## [1] "TADA_AssignUsesToAU: Importing existing uses by AU from ATTAINS Expert Query."

``` r
TADA_TableExport(AU.uses)
```

#### From a User Supplied Crosswalk (Uses to AU)

Users may also choose to supply their own list of use names if desired
rather than sourcing it from the prior ATTAINS assessment cycle. To
ensure we use the correct column names, we can run TADA_AssignUsesToAU()
with no argument function inputs. Note: The TADA.AssessmentUnitStatus
column is auto populated and does not need to be supplied from a user
supplied crosswalk.

For ATTAINS.WaterType, please ensure this matches the ATTAINS.WaterType
from the output of MT.AUMLRef\$ATTAINS_crosswalk for your assessment
unit.

``` r
names(TADA_AssignUsesToAU())
```

    ## [1] "ATTAINS.OrganizationIdentifier"   "ATTAINS.AssessmentUnitIdentifier"
    ## [3] "ATTAINS.UseName"                  "ATTAINS.WaterType"               
    ## [5] "TADA.AssessmentUnitStatus"        "IncludeOrExclude"

``` r
AU.uses.user.supplied <- data.frame(
  ATTAINS.OrganizationIdentifier = c("MTDEQ", "MTDEQ"),
  ATTAINS.AssessmentUnitIdentifier = c("MT76M001_020", "MT76M001_020"),
  ATTAINS.UseName = c("Example: Aquatic Life, cold waters", "Example: Human Health"),
  ATTAINS.WaterType = c("RIVER", "RIVER"),
  IncludeOrExclude = c("Include", "Include")
)

AU.uses2 <- TADA_AssignUsesToAU(
  tada.MT.clean,
  org_id = "MTDEQ",
  AUMLRef = tada.MT.AUMLRef,
  AU_UsesRef = AU.uses.user.supplied
)
```

    ## [1] "TADA_AssignUsesToAU: Importing existing uses by AU from ATTAINS Expert Query."

``` r
TADA_TableExport(AU.uses2)
```

### D) Create a TADA-compatible Criteria and Methodologies Table

The EPATADA Module 3 functions are being designed to:

1.  Assist users with creating a Criteria and Methodologies table,
2.  Analyze Water Quality Portal results using the user supplied
    Criteria and Methodologies table.

Users can choose to:

1.  Fill in and populate their own criteria table **manually**,
2.  use an **auto_assign method** of creating a crosswalk of ATTAINS
    parameter names to WQX Characteristic and to CST pollutant names
    along with a crosswalk between ATTAINS use names and EPA Criteria
    Search Tool (CST) uses. This allows for populating the magnitude
    components from the CST for each ATTAINS parameter and use, if one
    is found for your organization (duration, frequency and other
    methodology components will still need user inputs and a thorough
    review of the crosswalk between the CST and ATTAINS parameter and
    uses will be required),
3.  or pull in their organization’s criteria table from a list of
    pre-filled templates from the **TADACommunityHub**\* if one exists
    (currently all R8 states and tribes have participated in submitting
    their organization’s criteria table to this repository!).

\* The TADACommunityHub is a collaborative hub where TADA users share
and maintain information for custom workflows (e.g., state, tribal,
territory, EPA, or other entities). For example, it includes
user-contributed criteria and methodologies templates to support
reproducible and efficient analyses.

The EPA 304A criteria and methodologies are also available for use.

#### TADA Community Hub Criteria and Methodologies Templates

For this demo, We will load in a pre-filled criteria template for MTDEQ
(draft). The reference tables may need additional review by the region
and states/tribes.

``` r
# Load the example R8 criteria table for MTDEQ
criteria_table <- system.file("extdata", "criteria_table.rda", package = "EPATADA")
load(criteria_table)

MT_criteria <- TADA_DefineCriteriaMethodology(
  tada.MT.clean.DO,
  org_id = "MTDEQ",
  criteriaMethods = criteria_table,
  AUMLRef = tada.MT.AUMLRef,
  AU_UsesRef = AU.uses,
  displayUniqueId = TRUE
)

TADA_TableExport(MT_criteria)
```

#### Assistance with Generating a Criteria and Methodologies Template from Scratch

Alternatively, if your organization has not submitted a criteria table
to the TADACommunityHub repository, TADA_DefineCriteriaMethodology has
an auto_assign function input which allows populating the magnitude
values from the Criteria Search Tool (CST) to past ATTAINS.ParameterName
and ATTAINS.UseName for your organization from prior submission to
ATTAINS assessment cycles. Filter it to just “DISSOLVED OXYGEN (DO)”.

Note that duration, frequency and other criteria components will still
need to be filled out with auto_assign. However, if users are only
interested in individual result measurement excursions, they can proceed
to use this table as is.

``` r
criteria_table_auto <- TADA_DefineCriteriaMethodology(
  tada.MT.clean,
  org_id = "MTDEQ",
  auto_assign = T,
  criteriaMethods = NULL,
  AUMLRef = tada.MT.AUMLRef,
  AU_UsesRef = AU.uses,
  displayUniqueId = TRUE
) |>
  dplyr::filter(TADA.CharacteristicName == "DISSOLVED OXYGEN (DO)")
```

    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_ParametersForAnalysis with default assignment."
    ## [1] "TADA_ParametersForAnalysis: auto_assign == 'Org' was selected, finding an exact ATTAINS.ParameterName match, by ATTAINS.OrganizationName, for each TADA.ComparableDataIdentifier - by WQP CharacteristicName if one is found."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_UsesForAnalysis with default assignment."
    ## [1] "TADA_UsesForAnalysis: auto_assign == TRUE was selected, assigning all unique ATTAINS.UseName, by ATTAINS.OrganizationIdentifier, to any ATTAINS.ParameterName that an organization have not done assessments for in prior ATTAINS cycle. Please review carefully and Exclude rows as needed."
    ## [1] "TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected. Running TADA_MLSummary with default assignment."
    ## [1] "TADA_MLSummary: displayNA = TRUE: This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
    ## [1] "TADA_MLSummary: displayNA = TRUE was selected:This MLSummaryRef table will display ALL parameters and uses for a ML/AU regardless if it contains data collected for that TADA.CharacteristicName in your TADA data frame."
    ## [1] "EQ_DomainValues: For param_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."

    ## TADA_DefineCriteriaMethodology: auto_assign = TRUE was selected.
    ## Finding an alias match between ATTAINS parameter name and Criteria Search Tool (CST) standardized pollutant names.
    ## Finding an alias match between ATTAINS use name and Criteria Search Tool (CST) uses.
    ## If an ATTAINS.ParameterName and ATTAINS.UseName alias was found, populating these rows with the CST magnitude values.
    ## A many-to-many match is likely. User review is needed to ensure the proper parameter and uses from ATTAINS and CST alias crosswalk was accomplished (remove or add rows as needed).

    ## [1] "EQ_DomainValues: For use_name the values in the 'name' column of the function output are the allowable values for rExpert Query functions."
    ## [1] "TADA_DefineCriteriaMethodology: removing any instances where CST Pollutant names are 'PH VARIATION', 'TEMPERATURE RISE ABOVE AMBIENT'. TADA functions cannot currently handle analysis for these instances."

``` r
TADA_TableExport(criteria_table_auto)
```

We will continue using the criteria table for MTDEQ that was submitted
to the TADACommunityHub.

#### Editing the criteria table in excel

If further edits are desired to the criteria table, users can export the
criteria table into an excel spreadsheet and read it back into the R
environment once any edits are made.

``` r
MT_criteria_excel <- TADA_DefineCriteriaMethodology(
  tada.MT.clean.DO,
  org_id = "MTDEQ",
  criteriaMethods = criteria_table,
  AUMLRef = tada.MT.AUMLRef,
  AU_UsesRef = AU.uses,
  displayUniqueId = TRUE,
  excel = T,
  overwrite = T
)
```

#### View and analyze scatter plot excursions for DO for assessment unit

Now, lets view some scatter plots for the criteria defined in your
criteria table which will be compared to the results from your TADA data
frame for DO for a single assessment unit, “MT76M001_020”. We will join
your TADA data frame to the criteria table using the MTDEQ table sourced
from the TADACommunityHub.

``` r
# join use and criteria to tada df (many to many relationships may occur)
tada.MT.clean.DO2 <- dplyr::left_join(
  tada.MT.clean.DO,
  MT_criteria,
  by = "TADA.ComparableDataIdentifier"
)

# break into separate data frames for each unique combination of DurationValue, DurationMethod, DurationUnit, and UniqueSpatialCriteria for analysis and figures
tada.MT.do.subsets <- tada.MT.clean.DO2 |>
  dplyr::group_by(DurationValue, DurationMethod, DurationUnit,
    UniqueSpatialCriteria) |>
  dplyr::group_split()
```

Note that the scatter points are only individual result measurements.
The TADA Team is working on developing functions for handling the
criteria (magnitude, duration and frequency) and methodologies
information (e.g. seasonality and data sufficiency) entered into the
Criteria and Methodologies Template.

``` r
TADA_scatters <- list()

n <- length(tada.MT.do.subsets)

for (i in 1:n){
  # need to set this up as function to plot multiple
  desc <- paste0(
    "Lower bound for ",
    tada.MT.do.subsets[[i]]$ATTAINS.UseName[1],
    " (",
    tada.MT.do.subsets[[i]]$DurationValue[1],
    " ",
    tada.MT.do.subsets[[i]]$DurationUnit[1],
    " ",
    tada.MT.do.subsets[[i]]$DurationMethod[1],
    " for ",
    tada.MT.do.subsets[[i]]$UniqueSpatialCriteria[1],
    ")"
  )

  TADA_scatters[[i]] <- TADA_Scatterplot(tada.MT.do.subsets[[i]]) |>
    plotly::add_lines(
      x = c(
        min(tada.MT.do.subsets[[i]]$ActivityStartDate, na.rm = TRUE),
        max(tada.MT.do.subsets[[i]]$ActivityStartDate, na.rm = TRUE)
      ),
      y = (tada.MT.do.subsets[[i]]$MagnitudeValueLower[1]),
      inherit = FALSE,
      name = paste(strwrap(desc, width = 20), collapse = "<br>"),
      line = list(color = "red", dash = "dash")
    )

  names(TADA_scatters)[i] <- desc
}
```

The criteria and methodologies table for DO contained multiple rows.
Let’s view the different options and explore which might be appropriate
for our assessment unit.

Here is what the scatter plots might look like if the “Lower bound for
Aquatic Life (1 n-day arithmetic min for Cold water, other life stages
present)” and “Lower bound for Aquatic Life (7 n-day arithmetic mean for
Cold water, other life stages present)” criteria and methodologies
information was determined to be the appropriate criteria for this
assessment unit and applied.

``` r
names(TADA_scatters)
```

    ##  [1] "Lower bound for Aquatic Life (1 n-day arithmetic min for Cold water, other life stages present)"           
    ##  [2] "Lower bound for Aquatic Life (1 n-day arithmetic min for Warm water, other life stages present)"           
    ##  [3] "Lower bound for Aquatic Life (1 n-day arithmetic min for early life stages present  - burbot not present)" 
    ##  [4] "Lower bound for Aquatic Life (1 n-day arithmetic min for early life stages present -  - burbot present)"   
    ##  [5] "Lower bound for Aquatic Life (7 n-day arithmetic mean for Cold water, other life stages present)"          
    ##  [6] "Lower bound for Aquatic Life (7 n-day arithmetic mean for early life stages present  - burbot not present)"
    ##  [7] "Lower bound for Aquatic Life (7 n-day arithmetic mean for early life stages present - burbot present)"     
    ##  [8] "Lower bound for Aquatic Life (7 n-day arithmetic mean min for Cold water, other life stages present)"      
    ##  [9] "Lower bound for Aquatic Life (7 n-day arithmetic mean min for Warm water, other life stages present)"      
    ## [10] "Lower bound for Aquatic Life (30 n-day arithmetic mean for Cold water, other life stages present)"         
    ## [11] "Lower bound for Aquatic Life (30 n-day arithmetic mean for Warm water, other life stages present)"

``` r
# TADA_scatters
TADA_scatters[[1]]
```

``` r
TADA_scatters[[5]]
```
