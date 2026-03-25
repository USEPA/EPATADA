# Helper Function to Apply Uses to Unassigned Assessment Units by Water Type

This is a helper function to TADA_AssignUsesToAU and is meant to help
users with reviewing all water type and use name combination from their
org. This function will help to assign ATTAINS use names to any new or
modified assessment unit provided from a user's AUMLRef if there are
any.

## Usage

``` r
TADA_AssignUsesToWaterType(
  .data,
  org_id = NULL,
  waterUseRef = NULL,
  AUMLRef = NULL,
  api_key = NULL
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

- waterUseRef:

  An optional data frame input. If provided, this data frame should
  contain a completed crosswalk of use names associated with a water
  type. Users will need to ensure this crosswalk contains the
  appropriate column names in order to run the function.

- AUMLRef:

  An optional data frame input. If provided, this data frame should
  contain a completed crosswalk of monitoring location sites associated
  with an assessment unit. This data frame must contain the following
  column names which can be generated from the output of
  TADA_CreateAUMLCrosswalk: ATTAINS.OrganizationIdentifier,
  TADA.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
  and ATTAINS.WaterType.

- api_key:

  Optional character string. An api key for Expert Query web services.
  If not supplied, the default TADA api key will be used. For best
  performance, it is recommended that users obtain and use their own api
  key. Request an api key here:
  https://owapps.epa.gov/expertquery/api-documentation

## Value

A data frame with all the MonitoringLocationIdentifier Sites for a
defined AU.

## Details

This function will assume all use names applies to a water type from the
prior assessment cycles are being done for an organization's assessment.
Users are expected to modify this ref file as needed.

## See also

[`TADA_AssignUsesToAU()`](https://usepa.github.io/EPATADA/reference/TADA_AssignUsesToAU.md)

## Examples

``` r
TADA_AssignUsesToWaterType(TADA_AK_EXAMPLE, org_id = "AKDECWQ")
#> [1] "EQ_DomainValues: For org_id the values in the 'code' column of the function output are the allowable values for rExpert Query functions."
#> TADA_CreateWaterusesRef: Importing unique water types and uses by organization from Expert Query.
#> # A tibble: 157 × 5
#>    ATTAINS.OrganizationName ATTAINS.OrganizationIdentifier ATTAINS.UseName      
#>    <chr>                    <chr>                          <chr>                
#>  1 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#>  2 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#>  3 Alaska                   AKDECWQ                        MARINE WATER / GROWT…
#>  4 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#>  5 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#>  6 Alaska                   AKDECWQ                        MARINE WATER / HARVE…
#>  7 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#>  8 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#>  9 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#> 10 Alaska                   AKDECWQ                        MARINE WATER / WATER…
#> # ℹ 147 more rows
#> # ℹ 2 more variables: ATTAINS.WaterType <chr>, IncludeOrExclude <chr>
```
