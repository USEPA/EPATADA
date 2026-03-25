# Get Unique Synonym Reference Table

Function generates a synonym reference table containing all unique
combinations of TADA.CharacteristicName, TADA.ResultSampleFractionText,
and TADA.MethodSpeciationName. The function also joins in some
TADA-specific suggested synonyms for nutrients and priority parameters.
These target synonyms (denoted in the reference table with the prefix
"Target.") are intended to help the user aggregate synonymous data that
may be uploaded with slightly different metadata conventions and prepare
nutrient data for total N and P summations. Users can review how their
input data relates to target synonyms for TADA.CharacteristicName,
TADA.ResultSampleFractionText, and TADA.MethodSpeciationName. Once the
synonym table is created, users may optionally edit the target columns
in the reference table to meet their needs. Additionally, the function
assumes the user has already removed any data containing suspect
characteristic-unit-fraction-speciation combinations (i.e. user has
already run TADA_FlagFraction, TADA_FlagSpeciation, TADA_FlagResultUnit,
etc.).

## Usage

``` r
TADA_GetSynonymRef(.data = NULL)
```

## Arguments

- .data:

  TADA dataframe. If a dataframe is not provided, the function will
  return the default internal reference table.

## Value

Synonym Reference Table unique to the input dataframe
