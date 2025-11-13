# Get Organization and Provider Reference Table

This function creates a crosswalk of all OrganizationIdentifiers,
OrganizationFormalNames, and ProviderNames in the Water Quality Portal
(WQP).

## Usage

``` r
TADA_GetWQPOrgProviderRef()
```

## Value

A crosswalk dataframe including the following columns:
OrganizationIdentifier, OrganizationFormalName, ProviderName.

## Examples

``` r
if (FALSE) { # \dontrun{
provider.ref <- TADA_GetProviderRef()
} # }
```
