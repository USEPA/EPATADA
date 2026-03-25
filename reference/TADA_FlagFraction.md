# Check Sample Fraction Validity

Function checks the validity of each characteristic-fraction combination
in the dataframe. When clean = TRUE, rows with Suspect
characteristic-fraction combinations are removed. Default is clean =
TRUE. When flaggedonly = TRUE, only Suspect characteristic-fraction
combinations are returned. Default is flaggedonly = FALSE.

## Usage

``` r
TADA_FlagFraction(.data, clean = TRUE, flaggedonly = FALSE)
```

## Arguments

- .data:

  TADA dataframe

- clean:

  Boolean argument; removes "Suspect" characteristic-fraction
  combinations from the dataframe when clean = TRUE. Default is clean =
  TRUE.

- flaggedonly:

  Boolean argument; filters to show only the "Suspect"
  characteristic-fraction combinations in the dataframe when flaggedonly
  = TRUE. Default is flaggedonly = FALSE.

## Value

This function adds the following column to your dataframe:
TADA.SampleFraction.Flag, which flags each CharacteristicName and
ResultSampleFractionText combination in your dataframe as either
"NonStandardized", "Suspect", "Pass", or "Not Reviewed". When clean =
FALSE and flaggedonly = TRUE, the data are filtered to show the
"Suspect" rows only. When clean = TRUE and flaggedonly = FALSE,
"Suspect" rows are removed from the dataframe and no column will be
appended. When clean = TRUE and flaggedonly = TRUE, the function does
not execute and an error message is returned.

## Details

The “Not Reviewed” value within "TADA.SampleFraction.Flag" means that
the EPA WQX team has not yet reviewed the combinations (see
https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
The WQX team plans to review and update these new combinations
quarterly.

## Examples

``` r
# Load example dataset:
utils::data(Data_Nutrients_UT)

# Remove data with Suspect characteristic-fraction combinations:
SuspectFraction_clean <- TADA_FlagFraction(Data_Nutrients_UT)
#> All characteristic/fraction combinations are valid in your dataframe. Returning input dataframe with TADA.SampleFraction.Flag column for tracking.

# Flag, but do not remove, data with Suspect characteristic-fraction
# combinations
# in new column titled "TADA.SampleFraction.Flag":
SuspectFraction_flags <- TADA_FlagFraction(Data_Nutrients_UT, clean = FALSE)
#> All characteristic/fraction combinations are valid in your dataframe. Returning input dataframe with TADA.SampleFraction.Flag column for tracking.

# Show only Suspect characteristic-fraction combinations:
SuspectFraction_flaggedonly <- TADA_FlagFraction(Data_Nutrients_UT,
  clean = FALSE, flaggedonly = TRUE
)
#> This dataframe is empty because we did not find any Suspect fraction/characteristic combinations in your dataframe
```
