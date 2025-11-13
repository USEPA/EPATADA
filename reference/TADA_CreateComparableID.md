# Create TADA.ComparableDataIdentifier Column

This utility function creates the TADA.ComparableDataIdentifier column
by pasting together TADA.CharacteristicName,
TADA.ResultSampleFractionText, TADA.MethodSpeciationName, and
TADA.ResultMeasure.MeasureUnitCode.

## Usage

``` r
TADA_CreateComparableID(.data)
```

## Arguments

- .data:

  TADA dataframe

## Value

Input TADA dataframe with added TADA.ComparableDataIdentifier column.
