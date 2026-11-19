# Get Salt or Freshwater Reference Table

Retrieves the Salt or Freshwater reference at the monitoring location or
assessment unit level by either ATTAINS.WaterType or
TADA.MonitoringLocationTypeName, filtered by the type found in your WQP
dataframe.

## Usage

``` r
TADA_GetSaltFreshRef(.data, location_col = "AU", type_col = "ATTAINS")
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

# Retrieve saltfresh ref table based on testdat by TADA.MonitoringLocationTypeName,
# using TADA.MonitoringLocationIdentifier as location grouping
 TADA.SaltFreshRef <- TADA_GetSaltFreshRef(testdat,
                                      location_col = "ML",
                                      type_col = "TADA")

# Get ATTAINS water types to test data
testdat2 <- TADA_UpdateATTAINSWaterTypes(testdat, org_id = "OREGONDEQ")

# Retrieve saltfresh ref table based on testdat by ATTAINS.WaterType,
# using TADA.MonitoringLocationIdentifier as location grouping
ATTAINS.SaltFreshRef <- TADA_GetSaltFreshRef(testdat2,
                                             location_col = "ML",
                                             type_col = "ATTAINS")
} # }
```
