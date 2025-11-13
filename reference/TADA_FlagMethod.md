# Check for Suspect Analytical Methods

Function checks the validity of each characteristic-analytical method
combination in the dataframe. When clean = TRUE, rows with Suspect
characteristic-analytical method combinations are removed. Default is
clean = FALSE. When flaggedonly = TRUE, dataframe is filtered to show
only Suspect characteristic-analytical method combinations. Default is
flaggedonly = FALSE.

## Usage

``` r
TADA_FlagMethod(.data, clean = FALSE, flaggedonly = FALSE)
```

## Arguments

- .data:

  TADA dataframe

- clean:

  Boolean argument; removes "Suspect" characteristic-analytical method
  combinations from the dataframe when clean = TRUE. Default is clean =
  FALSE.

- flaggedonly:

  Boolean argument; filters dataframe to show only "Suspect"
  characteristic-analytical method combinations when flaggedonly = TRUE.
  Default is flaggedonly = FALSE.

## Value

This function adds the TADA.AnalyticalMethod.Flag to a TADA dataframe.
This column flags Suspect CharacteristicName,
ResultAnalyticalMethod/MethodIdentifier, and
ResultAnalyticalMethod/MethodIdentifierContext combinations in your
dataframe as either "Not Reviewed", "Suspect", or "Pass". When clean =
FALSE and flaggedonly = TRUE, the dataframe is filtered to show only
"Suspect" characteristic-analytical method combinations; the column
TADA.AnalyticalMethod.Flag is still appended. When clean = TRUE and
flaggedonly = FALSE, "Suspect" rows are removed from the dataframe and
no column will be appended.

## Details

The “Not Reviewed” value within "TADA.AnalyticalMethod.Flag" means that
the EPA WQX team has not yet reviewed the combinations (see
https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
The WQX team plans to review and update these new combinations
quarterly.

## Examples

``` r
# Load example dataset
utils::data(Data_R5_TADAPackageDemo)

# Remove Suspect characteristic-analytical method combinations from
# data frame:
SuspectMethod_clean <- TADA_FlagMethod(Data_R5_TADAPackageDemo,
  clean = TRUE
)

# Flag, but do not remove, Suspect characteristic-analytical method
# combinations
# in new column titled "TADA.AnalyticalMethod.Flag":
SuspectMethod_flags <- TADA_FlagMethod(Data_R5_TADAPackageDemo,
  clean = FALSE
)

# Show only Suspect characteristic-analytical method combinations:
SuspectMethod_flaggedonly <- TADA_FlagMethod(Data_R5_TADAPackageDemo,
  clean = FALSE, flaggedonly = TRUE
)
```
