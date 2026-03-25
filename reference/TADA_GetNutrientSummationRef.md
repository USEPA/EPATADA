# Get Nutrient Summation Reference Key

Return the installed nutrient summation reference table used by
TADA_CalculateTotalNP() to identify which nitrogen and phosphorus
subspecies are combined into total N and total P.

## Usage

``` r
TADA_GetNutrientSummationRef()
```

## Value

A data.frame of nutrient summation combinations with normalized keys.

## Details

The internal file can be customized by users and supplied back to
TADA_CalculateTotalNP() via the `sum_ref` argument.

- Key columns (TADA.CharacteristicName, TADA.ResultSampleFractionText,
  TADA.MethodSpeciationName) are normalized: leading/trailing whitespace
  is trimmed and both `""` and `"NONE"` are treated as `NA`.

- Rows are de-duplicated after normalization.

- The CSV is read with `na.strings = c("", "NA")` so blanks are
  converted to `NA`.

Expected columns include, at minimum:

- TADA.CharacteristicName

- TADA.ResultSampleFractionText

- TADA.MethodSpeciationName

- NutrientGroup

- SummationName

- SummationRank

- SummationNote

- SummationFractionNotes

- SummationSpeciationNotes

- SummationSpeciationConversionFactor

## See also

TADA_CalculateTotalNP()

## Examples

``` r
ref <- TADA_GetNutrientSummationRef()
head(ref)
#>   TADA.ActivityMediaName TADA.CharacteristicName TADA.ResultSampleFractionText
#> 1                  WATER                 AMMONIA                     DISSOLVED
#> 2                  WATER                 AMMONIA                     DISSOLVED
#> 3                  WATER                 AMMONIA                     DISSOLVED
#> 4                  WATER                 AMMONIA                     DISSOLVED
#> 5                  WATER                 AMMONIA                    FILTERABLE
#> 6                  WATER                 AMMONIA                    FILTERABLE
#>                                 SummationFractionNotes
#> 1                                                 <NA>
#> 2                                                 <NA>
#> 3                                                 <NA>
#> 4                                                 <NA>
#> 5 Assume filtered/filterable equals dissolved fraction
#> 6 Assume filtered/filterable equals dissolved fraction
#>   TADA.MethodSpeciationName SummationSpeciationNotes
#> 1                      AS N                     <NA>
#> 2                    AS NH3         Convert NH3 to N
#> 3                    AS NH4         Convert NH4 to N
#> 4                      <NA>                     <NA>
#> 5                      AS N                     <NA>
#> 6                    AS NH3         Convert NH3 to N
#>   SummationSpeciationConversionFactor SummationName SummationRank
#> 1                                  NA         AMMON             9
#> 2                               0.822         AMMON             9
#> 3                               0.776         AMMON            12
#> 4                                  NA       OTHER N           999
#> 5                                  NA         AMMON             9
#> 6                               0.822         AMMON             9
#>                                                                                                                       SummationNote
#> 1                                                                                                                              <NA>
#> 2                                                                                                                              <NA>
#> 3 Not explicity included in Nutrient Aggregation Sheet, but similar enough to EDH to be given rank anyway. Expert review suggested.
#> 4                                                                       Not used, speciation not reported and could not be inferred
#> 5                                                                                                                              <NA>
#> 6                                                                                                                              <NA>
#>   NutrientGroup
#> 1      Nitrogen
#> 2      Nitrogen
#> 3      Nitrogen
#> 4      Nitrogen
#> 5      Nitrogen
#> 6      Nitrogen

# Use a customized version:
# my_ref <- ref
# ...edit rows/targets as needed...
# df_out <- TADA_CalculateTotalNP(df_in, sum_ref = my_ref)
```
