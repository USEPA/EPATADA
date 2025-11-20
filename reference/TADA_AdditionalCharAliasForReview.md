# TADA Alias Methodology for ATTAINS, WQP and CST Alias Table for Review

This function compares the current WQX Characteristic Alias table of
ATTAINS.ParameterNames and WQX CharacteristicNames to the logic used in
this function, which looks at the percentage of words that are a match
between each ATTAINS parameters and WQX Characteristics (and within
ATTAINS and CST, and CST and WQX by CAS) as an alternative way of
finding additional aliases. It is recommended for the TADA team to
review this table and decide whether these aliases are accurate, and if
so, reach out to the WQX team to add these to the WQX Characteristic
Alias table.

## Usage

``` r
TADA_AdditionalCharAliasForReview(
  includeCST = FALSE,
  displayPercent = FALSE,
  ATTAINS.CST.tolerance = 1,
  CST.ATTAINS.tolerance = 1,
  ATTAINS.WQX.tolerance = 1,
  WQX.ATTAINS.tolerance = 1
)
```

## Arguments

- includeCST:

  a Boolean value. If TRUE, this will include columns for CST pollutant
  Name if it contains an alias between any 3 sources.

- displayPercent:

  a Boolean value. If True, this will display the percent match in
  number of words between the WQX characteristic, ATTAINS parameter and
  CST pollutant names.

- ATTAINS.CST.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with CST.ATTAINS.tolerance which defines
  the minimum percentage of the number of words that must be found in an
  ATTAINS parameter to a CST pollutant name for it to be considered an
  alias match.

- CST.ATTAINS.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with ATTAINS.CST.tolerance which defines
  the minimum percentage of the number of words that must be found in a
  CST pollutant name to an ATTAINS parameter to for it to be considered
  an alias match.

- ATTAINS.WQX.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with ATTAINS.WQX.tolerance which defines
  the minimum percentage of the number of words that must be found in an
  ATTAINS parameter to a WQX characteristic name for it to be considered
  an alias match.

- WQX.ATTAINS.tolerance:

  a numeric value ranging from 0 to 1 (0% to 100%). Default is 100%.
  This value is an OR condition with ATTAINS.WQX.tolerance which defines
  the minimum percentage of the number of words that must be found in a
  WQX characteristic name to an ATTAINS parameter to for it to be
  considered an alias match.

## Value

a data frame consisting of potential additional ATTAINS.ParameterName to
WQX.CharacteristicName alias for review. TADA team will review and
decide if these are appropriate aliases.

## Details

Note for Development: We should keep a reference file to indicate which
rows have already been reviewed during this process.In addition, we can
modify the 'strictness' of percent matches. Being more strict can result
in less potential match (false negatives) findings while less strict may
result in greater number of matches that shouldn't be matched (false
positives). Default for now is to be more strict.

## Examples

``` r
review <- TADA_AdditionalCharAliasForReview()
#> [1] "EQ_DomainValues: For param_name the values in the name column of the function output are the allowable values for rExpert Query functions."
review2 <- TADA_AdditionalCharAliasForReview(includeCST = TRUE)
#> [1] "EQ_DomainValues: For param_name the values in the name column of the function output are the allowable values for rExpert Query functions."

review_more_strict <- TADA_AdditionalCharAliasForReview(
  displayPercent = TRUE,
  ATTAINS.WQX.tolerance = 1.0,
  WQX.ATTAINS.tolerance = 1.0
  )
#> [1] "EQ_DomainValues: For param_name the values in the name column of the function output are the allowable values for rExpert Query functions."

review_less_strict <- TADA_AdditionalCharAliasForReview(
  displayPercent = TRUE,
  ATTAINS.WQX.tolerance = 0.5,
  WQX.ATTAINS.tolerance = 0.5
  )
#> [1] "EQ_DomainValues: For param_name the values in the name column of the function output are the allowable values for rExpert Query functions."
```
