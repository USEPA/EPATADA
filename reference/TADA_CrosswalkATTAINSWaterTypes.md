# Crosswalk WQP Monitoring Location Types to ATTAINS Water Types

The WQP Monitoring Location Types and ATTAINS Water Types are not direct
one-to-one matches. This function crosswalks WQP Monitoring Location
Type names to the corresponding ATTAINS Water Type using a crosswalk
maintained by the TADA team.

## Usage

``` r
TADA_CrosswalkATTAINSWaterTypes(
  .data,
  overwrite_existing = FALSE,
  validation = c("none", "flag", "correct")
)
```

## Arguments

- .data:

  A TADA data frame. Must include TADA.MonitoringLocationIdentifier and
  TADA.MonitoringLocationTypeName.

- overwrite_existing:

  Logical. If TRUE, overwrite ATTAINS.WaterType with the crosswalked
  value where a match exists. If FALSE (default), only fill
  ATTAINS.WaterType where it is missing or blank.

- validation:

  Character. Validation mode for ATTAINS.WaterType values:

  - "none": do not validate (default).

  - "flag": validate against the ATTAINS water_type domain and add
    TADA.ATTAINSWaterType.Flag; values are flagged as "Pass" or
    "Suspect"; no values are changed.

  - "correct": validate and attempt to replace invalid or missing values
    using the crosswalk; the TADA.ATTAINSWaterType.Flag indicates
    whether the final value is "Pass", "Corrected", or "Suspect".

## Value

A TADA data frame with ATTAINS.WaterType created (if needed) and
populated or overwritten as requested. If validation is requested and
allowable ATTAINS water types can be retrieved, the output also includes
TADA.ATTAINSWaterType.Flag.

## Details

- The crosswalk is read via
  `system.file("extdata", "ATTAINSWaterTypeToWQPMonLocType.csv", package = "EPATADA")`.

- Matching is case-insensitive (`TADA.MonitoringLocationTypeName` is
  uppercased prior to joining).

- Blank ATTAINS.WaterType values are normalized to `NA` for processing.

- When validation = "flag" or "correct", allowable values are retrieved
  via `rExpertQuery::EQ_DomainValues("water_type")`. If retrieval fails,
  the function warns and skips validation.

- In validation = "correct", rows with missing or invalid final
  ATTAINS.WaterType values are marked "Corrected" if a crosswalk value
  is available and used to replace the value; otherwise they remain
  "Suspect".

- If the crosswalk contains multiple rows for the same monitoring
  location type, the join may duplicate rows; the crosswalk should be
  curated to maintain one-to-one mappings where possible.

## See also

[`rExpertQuery::EQ_DomainValues`](https://rdrr.io/pkg/rExpertQuery/man/EQ_DomainValues.html)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- tibble::tibble(
  TADA.MonitoringLocationIdentifier = c("LOC1", "LOC2", "LOC3"),
  TADA.MonitoringLocationTypeName = c("Stream", "Lake", "Estuary"),
  ATTAINS.WaterType = c(NA_character_, "", "InvalidValue")
)

y  <- TADA_CrosswalkATTAINSWaterTypes(x)
y2 <- TADA_CrosswalkATTAINSWaterTypes(x, overwrite_existing = TRUE)
y3 <- TADA_CrosswalkATTAINSWaterTypes(x, validation = "flag")
y4 <- TADA_CrosswalkATTAINSWaterTypes(x, validation = "correct")
} # }
```
