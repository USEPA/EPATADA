# Pair Results for Numeric Criteria Calculation (UNDER ACTIVE DEVELOPMENT)

This function pairs TADA results with results from specified
characteristics from the same TADA.MonitoringLocation within a
user-specified time window to facilitate the calculation of numeric
criteria. The columns created by TADA_AutoClean are required to run this
function. If they are not present in the data frame, the function will
stop and print an error message.

## Usage

``` r
TADA_PairForCriteriaCalc(.data, ref = "null", hours_range = 4)
```

## Arguments

- .data:

  TADA dataframe

- ref:

  Write description of what columns need to be in this ref or null
  option

- hours_range:

  Numeric argument. The time difference allowed between the paired
  characteristic and the result.

## Value

A TADA data frame with six additional columns added for each pairing
group specified in the pairing ref.

## Details

Users can provide a pairing reference file (can be created using
TADA_CreatePairRef) to specify which combinations of
TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnit,
TADA.MethodSpeciationName, and TADA.ResultSampleFractionText should be
used for hardness, pH, temperature, salinity, chloride or other
user-defined groups. If no ref is specified, all possible combinations
for hardness, pH, temperature, salinity and chloride will be used. It is
highly recommended that users perform all unit conversion and synonym
harmonization before using TADA_PairForCriteriaCalc.

## Examples

``` r
if (FALSE) { # \dontrun{
AL_df <- TADA_DataRetrieval(
  startDate = "2010-11-30",
  endDate = "2010-12-01",
  statecode = "AL",
  ask = FALSE
)

AL_PairRef <- TADA_PairForCriteriaCalc(AL_df)
} # }
```
