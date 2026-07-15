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

## Examples

``` r
# Return the default internal synonym reference table
syn_ref_default <- TADA_GetSynonymRef()
head(syn_ref_default)
#>   TADA.CharacteristicName Target.TADA.CharacteristicName
#> 1                 AMMONIA                           <NA>
#> 2                 AMMONIA                           <NA>
#> 3                 AMMONIA                           <NA>
#> 4                 AMMONIA                           <NA>
#> 5                 AMMONIA                           <NA>
#> 6                 AMMONIA                           <NA>
#>   TADA.CharacteristicNameAssumptions TADA.ResultSampleFractionText
#> 1                               <NA>                     DISSOLVED
#> 2                               <NA>                     DISSOLVED
#> 3                               <NA>                     DISSOLVED
#> 4                               <NA>                     DISSOLVED
#> 5                               <NA>                    FILTERABLE
#> 6                               <NA>                    FILTERABLE
#>   Target.TADA.ResultSampleFractionText
#> 1                             FILTERED
#> 2                             FILTERED
#> 3                             FILTERED
#> 4                             FILTERED
#> 5                             FILTERED
#> 6                             FILTERED
#>                                                                                                  TADA.FractionAssumptions
#> 1                      Assumes DISSOLVED and TOTAL SOLUBLE are equivalent to FILTERED. FILTERED is precise and preferred.
#> 2                      Assumes DISSOLVED and TOTAL SOLUBLE are equivalent to FILTERED. FILTERED is precise and preferred.
#> 3                      Assumes DISSOLVED and TOTAL SOLUBLE are equivalent to FILTERED. FILTERED is precise and preferred.
#> 4                      Assumes DISSOLVED and TOTAL SOLUBLE are equivalent to FILTERED. FILTERED is precise and preferred.
#> 5 Assumes FILTERED, LAB and FILTERED, FIELD are equivalent to FILTERABLE and FILTERED. FILTERED is precise and preferred.
#> 6 Assumes FILTERED, LAB and FILTERED, FIELD are equivalent to FILTERABLE and FILTERED. FILTERED is precise and preferred.
#>   TADA.MethodSpeciationName Target.TADA.MethodSpeciationName
#> 1                      AS N                             <NA>
#> 2                    AS NH3                             AS N
#> 3                    AS NH4                             AS N
#> 4                      <NA>                             AS N
#> 5                      AS N                             <NA>
#> 6                    AS NH3                             AS N
#>                                               TADA.SpeciationAssumptions
#> 1                                                                   <NA>
#> 2              Converts speciation and result values from AS NH3 to AS N
#> 3              Converts speciation and result values from AS NH4 to AS N
#> 4 Assumes original speciation for AMMONIA is AS NH3 and converts to AS N
#> 5                                                                   <NA>
#> 6              Converts speciation and result values from AS NH3 to AS N
#>   Target.TADA.SpeciationConversionFactor HarmonizationGroup
#> 1                                     NA           Nitrogen
#> 2                                  0.822           Nitrogen
#> 3                                  0.776           Nitrogen
#> 4                                  0.822           Nitrogen
#> 5                                     NA           Nitrogen
#> 6                                  0.822           Nitrogen
```
