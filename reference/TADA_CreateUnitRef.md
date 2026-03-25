# Generate A DataFrame of Units and Target by Characteristic Name

This function generates a dataframe listing all unique characteristic
(by CharacteristicName) and unit (by TADA.ResultMeasure.MeasureUnitCode)
pairs present in the dataset. Where possible, the function fills in the
columns for target unit, and conversion factor. Users can edit it and
use it as an input for TADA_ConvertResultUnits to customize
standardization of units by characteristic. The
TADA.ResultMeasure.MeasureUnitCode column automatically incorporates any
additional unique unit codes from
TADA.DetectionQuantitationLimiMeasure.MeasureUnitCode that were not
observed in TADA.ResultMeasure.MeasureUnitCode. This is done to
facilitate estimating censored data later in the workflow. All variants
of TADA.ResultMeasure.MeasureUnitCode and ResultMeasure.MeasureUnitCode,
including USGS results where speciation is listed in the units are
included. This facilitates moving speciation from units to
TADA.MethodSpeciationName in TADA_ConvertResultUnits.

## Usage

``` r
TADA_CreateUnitRef(.data, print.message = TRUE)
```

## Arguments

- .data:

  TADA dataframe

- print.message:

  Boolean argument with two possible arguments, TRUE and FALSE. When
  print.message = TRUE, a message is printed that lists any
  characteristics (TADA.CharacteristicName) that have been assigned more
  than one target unit. When print.message = FALSE, no message is
  printed. The default is print.message = TRUE.

## Value

A dataframe with seven columns: TADA.CharacteristicName,
TADA.ResultMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode,
TADA.Target.ResultMeasureUnit, TADA.MethodSpeciationName,
ConversionFactor, and ConversionCoefficient. The number of rows will
vary based on the number of unique
TADA.CharacteristicName/ResultMeasure.MeasureUnitCode combinations in
the initial TADA dataframe.

## Details

The columns created by TADA_AutoClean are required to run this function.
If they are not present in the dataframe, TADA_AutoClean is
automatically run before the unit reference dataframe is created.

## Examples

``` r
# Load example dataset:
utils::data(Data_Nutrients_UT)

# Create a unit reference dataframe
UT_UnitRef <- TADA_CreateUnitRef(Data_Nutrients_UT)
#> TADA.CreateUnitRef: The following characteristics have more than one listed target unit: NITROGEN (MG/L and NONE) and NITRATE (MG/L and NONE). This may be due to units of different types that cannot be converted to match each other. You may wish to review the output of TADA.CreateUnitRef and edit it.
```
