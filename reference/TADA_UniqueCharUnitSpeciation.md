# Create Characteristic/MeasureUnitCode/MethodSpeciation Ref

Creates data frame of unique combinations of TADA.CharacteristicName,
TADA.ResultMeasure.MeasureUnitCode (normalized), optional
ResultMeasure.MeasureUnitCode, and TADA.MethodSpeciationName in a TADA
data frame.

## Usage

``` r
TADA_UniqueCharUnitSpeciation(.data)
```

## Arguments

- .data:

  A TADA data frame.

## Value

A data frame with unique combinations of TADA.CharacteristicName,
TADA.ResultMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode (if
present), and TADA.MethodSpeciationName

## Examples

``` r
UniqueCharUnitSpecExample <-
  TADA_UniqueCharUnitSpeciation(Data_Nutrients_UT)
```
