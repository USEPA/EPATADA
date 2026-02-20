# checkTADAColsForMap

Check to see if data frame selected for mapping contains the related
TADA and WQP columns. If param attains = TRUE, additional columns
required to include ATTAINS assessment unit identifier and source ref
for assessment unit will also be included in the check.

## Usage

``` r
checkTADAColsForMap(.data, attains = FALSE)
```

## Arguments

- .data:

  A TADA data frame or the "TADA_with_ATTAINS" data frame that is part
  of the output of TADA_CreateATTAINSAUMLCrosswalk or
  TADA_CreateAUMLCrosswalk.

- attains:

  Boolean argument. When attains = TRUE, the columns
  "ATTAINS.AssessmentUnitIdentifier" and "TADA.AURefSource" will be
  added to the check. Default is ATTAINS = FALSE.

## Value

The function will stop and provide an error message if any required cols
are missing.
