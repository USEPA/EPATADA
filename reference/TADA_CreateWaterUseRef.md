# Helper Function to Apply Uses to Unassigned Assessment Units by Water Type

This is a helper function to TADA_CreateUseAURef and is meant to help
users with reviewing all water type and use name combination from their
org. This function will help to assign ATTAINS use names to any new or
modified assessment unit provided from a user's AUMLRef if there are
any.

## Usage

``` r
TADA_CreateWaterUseRef(.data, org_id = NULL, waterUseRef = NULL)
```

## Arguments

- .data:

  A TADA dataframe. The user should run all desired data cleaning,
  processing, harmonization, filtering, and handling of censored data
  functions prior to running this function.

- org_id:

  The ATTAINS organization identifier must be supplied by the user. A
  list of organization identifiers can be found by downloading the
  ATTAINS Domains Excel file:
  https://www.epa.gov/system/files/other-files/2025-02/domains_2025-02-25.xlsx.
  organization identifiers are listed in the "OrgName" tab. The "code"
  column contains the organization identifiers that should be used for
  this param. If a user does not provide an org_id argument, the
  function attempts to identify which organization identifier(s) to
  include based on the unique ATTAINS organization identifiers found in
  the dataframe.

- waterUseRef:

  An optional data frame input. If provided, this data frame should
  contain a completed crosswalk of use names associated with a water
  type. Users will need to ensure this crosswalk contains the
  appropriate column names in order to run the function.

## Value

A data frame with all the MonitoringLocationIdentifier Sites for a
defined AU.

## Details

This function will assume all use names applies to a water type from the
prior assessment cycles are being done for an organization's assessment.
Users are expected to modify this ref file as needed.

## See also

[`TADA_CreateUseAURef()`](usepa.github.io/EPATADA/reference/TADA_CreateUseAURef.md)

## Examples

``` r
TADA_CreateWaterUseRef(TADA_AK_EXAMPLE, org_id = "AKDECWQ")
#> [1] "TADA_CreateWaterUseParamRef: Importing unique water types and uses by organization from Expert Query."
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
