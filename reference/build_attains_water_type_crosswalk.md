# Build ATTAINS water type crosswalk

Internal helper to construct the crosswalk used to assign
ATTAINS.WaterType from TADA.MonitoringLocationTypeName.

## Usage

``` r
build_attains_water_type_crosswalk(org_id = NULL, org_only = FALSE)
```

## Arguments

- org_id:

  Character string. Optional organization ID used to prioritize
  organization-specific ATTAINS values.

- org_only:

  Logical. If TRUE, only organization-specific ATTAINS water types are
  used. If FALSE, unmatched types fall back to the TADA default
  crosswalk.

## Value

A data frame with columns:

- TADA.MonitoringLocationTypeName:

  Upper-case monitoring location type name

- TADA.ATTAINS.WaterType:

  Recommended ATTAINS water type
