# Crosswalk WQP Monitoring Location Type to ATTAINS Water Type

Adds or updates ATTAINS.WaterType using TADA.MonitoringLocationTypeName.
By default, only missing ATTAINS.WaterType values are populated.

## Usage

``` r
TADA_CrosswalkATTAINSWaterTypes(
  .data,
  org_id = NULL,
  org_only = FALSE,
  replace_all = FALSE
)
```

## Arguments

- .data:

  A TADA data frame.

- org_id:

  Character string. Optional organization ID used to prioritize
  organization-specific ATTAINS values.

- org_only:

  Logical. If TRUE, only org-specific ATTAINS values are used. If FALSE,
  unmatched types fall back to the TADA default crosswalk.

- replace_all:

  Logical. If TRUE, replace all ATTAINS.WaterType values. If FALSE, only
  fill missing values. Default is FALSE.

## Value

A TADA data frame with ATTAINS.WaterType populated.

## Examples

``` r

if (FALSE) { # \dontrun{

# example for MT data
testdat <- Data_MT_MissoulaCounty

crosswalk <- TADA_CrosswalkATTAINSWaterTypes(testat, org_Id = "MTDEQ")
} # }
```
