# Assign Salt or Freshwater Indicator

Assigns A Salt or Freshwater Indicator at the monitoring location or
assessment unit level by either ATTAINS.WaterType or
TADA.MonitoringLocationTypeName.

## Usage

``` r
TADA_AssignSaltFreshIndicator(
  .data,
  location_col = "AU",
  type_col = "ATTAINS",
  ref = NULL
)
```

## Arguments

- .data:

  A data frame containing at least one location column
  (TADA.MonitoringLocationIdentifier or
  ATTAINS.AssessmentUnitIdentifier) and at least one water type column
  (TADA.MonitoringLocationTypeName or ATTAINS.WaterType). Columns
  provided must match the columns selected for indicator assignment in
  the other function params.

- location_col:

  Character string. Options are "AU" or "ML". When location_col equals
  "AU", ATTAINS.AssessmentUnitIdentifier is used as the location column.
  When location_col equals "ML", TADA.MonitoringLocationIdentifier is
  used as the location_col. Default is location_col equals "AU".

- type_col:

  Character string. Options are "TADA" or "ATTAINS". When type_col
  equals "TADA", TADA.MonitoringLocationTypeName is used to crosswalk
  each location with a salt/freshwater indicator. When type_col equals
  "ATTAINS", ATTAINS.WaterType is used to crosswalk each location with a
  salt/freshwater indicator. Default is type_col equals "ATTAINS".

- ref:

  An optional data frame containing the crosswalk of salt/fresh to
  either ATTAINS.WaterType or TADA.MonitoringLocationTypeName. Users can
  choose to modify this table or use the default NULL table generated
  from
  [`TADA_GetSaltFreshRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetSaltFreshRef.md)

## Value

The input data frame with an added TADA.SaltFreshIndicator column.

## Examples

``` r

if (FALSE) { # \dontrun{
# Get test data with both freshwater and saltwater results
testdat <- TADA_DataRetrieval(statecode = "OR",
                              startDate = "2023-06-01",
                              endDate = "2023-06-15",
                              characteristicType = "Physical",
                              ask = FALSE)

# Get saltfresh indicator based on TADA.MonitoringLocationTypeName,
# using TADA.MonitoringLocationIdentifier as location grouping
 TADA.SaltFreshRef <- TADA_GetSaltFreshRef(testdat,
                                           location_col = "ML",
                                           type_col = "TADA")

# Assign saltfresh indicator to WQP data based on TADA.MonitoringLocationTypeName
 testdat.SaltFresh <- TADA_AssignSaltFreshIndicator(testdat,
                                                    location_col = "ML",
                                                    type_col = "TADA",
                                                    ref = TADA.SaltFreshRef)

# Get ATTAINS water types to test data
testdat2 <- TADA_UpdateATTAINSWaterTypes(testdat, org_id = "OREGONDEQ")

# Assign saltfresh indicator based on ATTAINS.WaterType with DEFAULT ref values,
# using TADA.MonitoringLocationIdentifier as location grouping
ATTAINS.example <- TADA_AssignSaltFreshIndicator(testdat2,
                                                 location_col = "ML",
                                                 type_col = "ATTAINS")
} # }
```
