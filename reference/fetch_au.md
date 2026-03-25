# fetch_au

This function gets ATTAINS data for a set of Assessment Unit IDs.

## Usage

``` r
fetch_au(baseurl, assessment_unit_ids, org_filter = "all")
```

## Arguments

- baseurl:

  A url for an ESRI REST service layer.

- assessment_unit_ids:

  An ATTAINS assessment unit ID or IDs

- org_filter:

  ATTAINS organization identifier(s) as a character string. If
  populated, Assessment Units will only be fetched from the specified
  organization(s). A list of organization identifiers can be found by
  downloading the ATTAINS Domains Excel file:
  https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
  Organization identifiers are listed in the "OrgName" tab. The "code"
  column contains the organization identifiers that should be used for
  this param. When org_id = "all", Assessment Units from all
  organizations will be considered. The default is "all".

## Examples

``` r
if (FALSE) { # \dontrun{
baseurls <- c(
  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3",
  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0",
  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1",
  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2"
)
line.features <- fetch_au(baseurl = baseurls[[3]],
  assessment_unit_ids = c("IL_N-99",
    "IL_N-12",
    "IL_N-16",
    "IL_N-17"))
} # }
```
