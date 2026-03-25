# Get Monitoring Location Identifier and Assessment Unit Identifier Crosswalk from ATTAINS

Tribes and States who participate in electronic reporting of water
quality conditions through EPA ATTAINS may optionally submit a crosswalk
of WQP monitoring location identifiers associated with their assessment
units to ATTAINS. If an organization has recorded this information in
ATTAINS, this function can be used to get the ATTAINS user submitted
crosswalk of known monitoring location identifiers and assessment units.
As of 2025, all tribal nations record this information in ATTAINS but
only a few states.

## Usage

``` r
TADA_GetATTAINSAUMLCrosswalk(
  org_id = "all",
  batch_upload = FALSE,
  api_key = NULL
)
```

## Arguments

- org_id:

  Character vector. ATTAINS organization identifier(s). Use "all"
  (default) to retrieve the national extract for all organizations.

- batch_upload:

  Logical. If TRUE, return columns formatted for ATTAINS batch upload
  (ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID, MS_DATA_LINK). If
  FALSE (default), return TADA workflow column names.

- api_key:

  Optional API key for Expert Query. If NULL, the default TADA key will
  be used via .setEQKey().

## Value

A data.frame. When batch_upload = FALSE, columns:
OrganizationIdentifier, ATTAINS.OrganizationIdentifier,
ATTAINS.MonitoringLocationIdentifier, ATTAINS.AssessmentUnitIdentifier,
ATTAINS.MonitoringDataLinkText, ATTAINS.WaterType. When batch_upload =
TRUE, columns: MS_ORG_ID, MS_LOCATION_ID, ASSESSMENT_UNIT_ID,
MS_DATA_LINK.
