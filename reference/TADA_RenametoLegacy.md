# TADA_RenametoLegacy

This function renames columns in a dataframe from WQX3.0 (beta) names to
WQX2.0 (legacy) names. Water Quality Portal data are retrieved using
USGS dataRetrieval service = "ResultWQX3". The purpose of this function
is to aid in integrating and updating TADA dependencies developed under
WQX2.0 to function with data retrieved using WQX3.0 service.

## Usage

``` r
TADA_RenametoLegacy(.data)
```

## Arguments

- .data:

  A water quality monitoring dataframe retrieved using
  dataRetrieval::readWQPdata using WQX3.0 Beta services

## Value

A water quality monitoring dataframe with WQX2.0 Legacy column names

## Details

TADA_RenametoLegacy function calls on EPA web services to read in the
documented WQX3.0 schema file (schema_outbound_wqx3.0.csv).The file
crosswalks WQX3.0 column names with equivalent WQX2.0 Legacy column
names across profiles (e.g., PhysChem, ActivityMetric) where
appropriate. The function uses data.table::setnames() to rename columns
in the dataframe by reference - in this case where there are beta names,
rename to legacy names, and skip where there are no matches.

## Examples

``` r
DeWitt_wqx3 <- dataRetrieval::readWQPdata(
  statecode = "Illinois",
  countycode = "DeWitt", characteristicName = "Nitrogen",
  service = "ResultWQX3", dataProfile = "fullPhysChem",
  ignore_attributes = TRUE
)
#> GET: https://api.waterdata.usgs.gov/samples-data/codeservice/states?mimeType=application%2Fjson
#> GET: https://api.waterdata.usgs.gov/samples-data/codeservice/counties?mimeType=application%2Fjson
#> GET: https://api.waterdata.usgs.gov/samples-data/codeservice/states?mimeType=application%2Fjson
#> GET: https://api.waterdata.usgs.gov/samples-data/codeservice/states?mimeType=application%2Fjson
#> GET: https://www.waterqualitydata.us/wqx3/Result/search?countycode=US%3A17%3A039&characteristicName=Nitrogen&dataProfile=fullPhysChem&mimeType=csv
#> Error in importWQP(baseURL, tz = tz, convertType = convertType): ERROR: INCOMPLETE DATA - THE RESULTS FOR THIS REQUEST ARE NOT COMPLETE AND MORE DATA IS LIKELY AVAILABLE.  PLEASE RETRY THE REQUEST.

DeWitt_wqx3_withlegacynames <- EPATADA::TADA_RenametoLegacy(DeWitt_wqx3)
#> Error: object 'DeWitt_wqx3' not found
```
