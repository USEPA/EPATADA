# TADA_ConvertSpecialChars

This function will screen a column of the user's choice for special
characters. It creates a NEW column that describes the content of the
column prior to conversion to numeric (named "TADA.COLUMN NAME
DataTypes.Flag"). It also creates a NEW column to hold the new, numeric
format (named "TADA.COLUMN NAME"). This function will successfully
convert some special character formats to numeric: whitespace, \>, \<,
~, %, and commas are removed before converting a result value to
numeric. Result values in the format \# - \# are converted to an average
of the two numbers. Result values containing any other text or
non-numeric characters become NA in the newly created "TADA.COLUMN NAME"
and labeled accordingly in "TADA.COLUMN NAME DataTypes.Flag". When clean
= TRUE, rows that cannot be converted to numeric are removed. When clean
= FALSE, no rows are removed. Default is clean = FALSE. When flaggedonly
= TRUE, data frame is filtered to show only rows with non-numeric result
values. Default is flaggedonly = FALSE.

## Usage

``` r
TADA_ConvertSpecialChars(
  .data,
  col,
  percent.ave = TRUE,
  clean = FALSE,
  flaggedonly = FALSE
)
```

## Arguments

- .data:

  A TADA profile object

- col:

  A character column to be converted to numeric

- percent.ave:

  Boolean argument; default is percent.ave = TRUE. When clean = TRUE,
  any percent range values will be averaged. When percent.ave = FALSE,
  percent range values are not averaged, but are flagged.

- clean:

  Boolean argument; removes non-numeric result values from the data
  frame when clean = TRUE. Default is clean = FALSE.

- flaggedonly:

  Boolean argument; filters dataframe to show only non-numeric result
  values when flaggedonly = TRUE. Default is flaggedonly = FALSE.

## Value

Returns the original dataframe with two new columns: the input column
with the prefix "TADA.", which holds the numeric form of the original
column, and "TADA.COLUMN NAME DataTypes.Flag", which has text describing
the type of data contained within the column of interest, including
"Numeric", "Less Than" (\<), "Greater Than" (\>), "Approximate Value"
(~), "Text" (A-z), "Percentage" (%), "Comma-Separated Numeric" (#,###),
and "Numeric Range - Averaged" (# - \#).

## Examples

``` r
HandleSpecialChars_ResultMeasureValue <-
  TADA_ConvertSpecialChars(Data_Nutrients_UT, "ResultMeasureValue")
unique(HandleSpecialChars_ResultMeasureValue$
  TADA.ResultMeasureValueDataTypes.Flag)
#> [1] "Coerced to NA"      "Numeric"            "NA - Not Available"
#> [4] "Text"              

HandleSpecialChars_DetLimMeasureValue <-
  TADA_ConvertSpecialChars(
    Data_Nutrients_UT,
    "TADA.DetectionQuantitationLimitMeasure.MeasureValue"
  )
unique(HandleSpecialChars_DetLimMeasureValue$
  TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag)
#> [1] "Numeric"            "NA - Not Available"
```
