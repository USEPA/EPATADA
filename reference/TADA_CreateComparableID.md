# Create TADA.ComparableDataIdentifier Column

Create a comparable identifier by concatenating:

- TADA.CharacteristicName

- TADA.ResultSampleFractionText

- TADA.MethodSpeciationName

- TADA.ResultMeasure.MeasureUnitCode

## Usage

``` r
TADA_CreateComparableID(.data)
```

## Arguments

- .data:

  A TADA dataframe (data.frame or tibble) with the required columns:
  TADA.CharacteristicName, TADA.ResultSampleFractionText,
  TADA.MethodSpeciationName, and TADA.ResultMeasure.MeasureUnitCode.

## Value

The input dataframe with:

- harmonized fields (fraction/speciation/unit) where missing/"none" -\>
  "NONE"

- a character column TADA.ComparableDataIdentifier

## Details

Harmonization:

- TADA.ResultSampleFractionText, TADA.MethodSpeciationName, and
  TADA.ResultMeasure.MeasureUnitCode are first normalized so any blank,
  NULL/NA, or any case variant of "none" are set to the literal "NONE".

Identifier construction:

- Each component is trimmed. For the characteristic name only, blanks/NA
  are converted to the literal "NA". For fraction/speciation/unit, the
  normalized values are used (i.e., "NONE" where missing).

- Example: "DISSOLVED OXYGEN (DO)\_NONE_NONE_MG/L"

## Examples

``` r
df <- data.frame(
  TADA.CharacteristicName = c("DISSOLVED OXYGEN (DO)", "pH", "Nitrate"),
  TADA.ResultSampleFractionText = c("", NA, "Dissolved"),
  TADA.MethodSpeciationName = c(" ", NA, ""),
  TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "none", NA),
  stringsAsFactors = FALSE
)

out <- TADA_CreateComparableID(df)
out$TADA.ComparableDataIdentifier
#> [1] "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
#> [2] "pH_NONE_NONE_NONE"                   
#> [3] "Nitrate_Dissolved_NONE_NONE"         
# Expected:
# [1] "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
# [2] "pH_NONE_NONE_NONE"
# [3] "Nitrate_Dissolved_NONE_NONE"
```
