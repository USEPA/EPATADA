# WQX 3.0 Migration

This vignette walks through how to download WQX 3.0 data from the WQP
and rename the columns back to their WQX 2.0 equivalents. Check out the
[WQX3.0 Data Now Available on the Water Quality
Portal](https://waterdata.usgs.gov/blog/wqx3/) blog for more information
about the new WQX 3.0 profiles and WQP beta web services! In addition,
USGS provides a summary of the current status of WQP functions in
dataRetrieval
[here](https://doi-usgs.github.io/dataRetrieval/articles/Status.html).

First, install and load the remotes package specifying the repo. This is
needed before installing EPATADA because it is only available on GitHub
(not CRAN).

``` r
install.packages("remotes", repos = "http://cran.us.r-project.org")
```

    ## Installing package into '/home/runner/work/_temp/Library'
    ## (as 'lib' is unspecified)

``` r
library(remotes)
```

Next, install and load EPATADA using the remotes package. USGS’s
dataRetrieval and other TADA R Package dependencies will also be
downloaded automatically from CRAN with the TADA install. If desired,
the development version of dataRetrieval can be downloaded directly from
GitHub (un-comment).

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

Let’s use USGS’s dataRetrieval R package to query the [WQP
beta](https://www.waterqualitydata.us/beta/) web services and retrieve
the new WQX 3.0 full physical chemical profile.

``` r
wqx3_fullPhysChem <- dataRetrieval::readWQPdata(
  statecode = "Illinois",
  countycode = "DeWitt",
  characteristicName = "Nitrogen",
  service = "ResultWQX3",
  dataProfile = "fullPhysChem",
  ignore_attributes = TRUE
)
```

    ## GET: https://api.waterdata.usgs.gov/samples-data/codeservice/states?mimeType=application%2Fjson

    ## GET: https://api.waterdata.usgs.gov/samples-data/codeservice/counties?mimeType=application%2Fjson

    ## GET: https://api.waterdata.usgs.gov/samples-data/codeservice/states?mimeType=application%2Fjson
    ## GET: https://api.waterdata.usgs.gov/samples-data/codeservice/states?mimeType=application%2Fjson

    ## GET: https://www.waterqualitydata.us/wqx3/Result/search?countycode=US%3A17%3A039&characteristicName=Nitrogen&dataProfile=fullPhysChem&mimeType=csv

    ## WQX3 services are in-development, use with caution.

We use the `readWQPdata` and `whatWQPsites` functions from the USGS
`dataRetrieval` R package within `TADA_DataRetrieval`. Currently,
`TADA_DataRetrieval` supports only WQX 2.2 (most recent legacy
profiles). We are evaluating the new WQX 3.0 beta services and planning
to transition the EPATADA R package to utilize these services soon.

To facilitate this transition, we have developed a function that
converts all column headers in a WQX 3.0 profile back to their WQX 2.2
equivalents. This enables us to effectively test for backward
compatibility. This new EPATADA function, `TADA_RenametoLegacy`,
leverages a crosswalk file that is available on the [Water Quality
Portal Quick Reference
Guide](https://www.epa.gov/waterdata/water-quality-portal-quick-reference-guide).

``` r
wqx3schema <- readr::read_csv("https://www.epa.gov/system/files/other-files/2025-07/schema_outbound_wqx3.0.csv", show_col_types = FALSE)
```

After retrieving the WQX 3.0 full physical chemical profile, we can use
`TADA_RenametoLegacy` to change the column names back to WQX 2.2 where
applicable.

``` r
wqx3_legacynames <- EPATADA::TADA_RenametoLegacy(wqx3_fullPhysChem)
```

Now, we can test other EPATADA package functions as usual, for example,
we can run `TADA_AutoClean`:

``` r
wqx3_legacynames <- TADA_AutoClean(wqx3_legacynames)
```

    ## TADA_Autoclean: creating TADA-specific columns.

    ## TADA_Autoclean: handling special characters and coverting TADA.ResultMeasureValue and TADA.DetectionQuantitationLimitMeasure.MeasureValue value fields to numeric.

    ## TADA_Autoclean: converting TADA.LatitudeMeasure and TADA.LongitudeMeasure fields to numeric.

    ## TADA_Autoclean: harmonizing synonymous unit names (m and meters) to m.

    ## TADA_Autoclean: updating deprecated (i.e. retired) characteristic names.

    ## No deprecated characteristic names found in dataset.

    ## TADA_Autoclean: harmonizing result and depth units.

    ## TADA_Autoclean: creating TADA.ComparableDataIdentifier field for use when generating visualizations and analyses.

    ## NOTE: This version of the TADA package is designed to work with numeric data with media name: 'WATER'. TADA_AutoClean does not currently remove (filter) data with non-water media types. If desired, the user must make this specification on their own outside of package functions. Example: dplyr::filter(.data, TADA.ActivityMediaName == 'WATER')

The WQP and WQX teams (USGS and EPA) are interested in hearing feedback
from users on the new WQX 3.0 schema (data profiles) and WQP beta web
services (Contact WQX Helpdesk <WQX@epa.gov>). In addition, please reach
out if you run into any issues or questions related to the WQX 2.2 to
3.0 crosswalk file (available on the [Water Quality Portal Quick
Reference
Guide](https://www.epa.gov/waterdata/water-quality-portal-quick-reference-guide))
we developed to assist you with your transition to the new services or
data profiles. This EPATADA R package function, , `TADA_RenametoLegacy`,
is provided for WQX 3.0 schema and WQP beta service testing purposes
only. We plan to transition the EPATADA R Package to use the WQX 3.0
schema and new WQP services soon. Stay tuned.
