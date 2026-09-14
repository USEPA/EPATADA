# Assign Salt or Freshwater Indicator

Assigns A Salt or Freshwater Indicator at the monitoring location or
assessment unit level by either ATTAINS.WaterType or
TADA.MonitoringLocationTypeName.

## Usage

``` r
TADA_SaltFreshIndicator(.data, location_col = "AU", type_col = "ATTAINS")
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

# Assign saltfresh indicator based on TADA.MonitoringLocationTypeName,
# using TADA.MonitoringLocationIdentifier as location grouping
 TADA.example <- TADA_SaltFreshIndicator(testdat,
                                         location_col = "ML",
                                         type_col = "TADA")

# Assign ATTAINS water types to test data
testdat.ATTAINSwattypes <- testdat |>
  TADA_CrosswalkATTAINSWaterTypes()

# Assign saltfresh indicator based on ATTAINS.WaterType,
# using ATTAINS.AssessmentUnitIdentifier as location grouping
ATTAINS.example <- TADA_SaltFreshIndicator(testdat.ATTAINSwattypes,
                                           location_col = "AU",
                                           type_col = "ATTAINS")
} # }
```
