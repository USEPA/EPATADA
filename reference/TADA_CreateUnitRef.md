# Generate A DataFrame of Units and Target by Characteristic Name

Creates a dataframe of unique characteristic/unit pairs and fills target
unit and conversion factors where possible. This reference can be edited
and used by TADA_ConvertResultUnits to standardize units by
characteristic.

## Usage

``` r
TADA_CreateUnitRef(.data, print.message = TRUE)
```

## Arguments

- .data:

  TADA dataframe

- print.message:

  Logical. If TRUE (default), prints a message listing any
  characteristics (TADA.CharacteristicName) that have more than one
  target unit.

## Value

A dataframe with the following columns:

- TADA.CharacteristicName

- ResultMeasure.MeasureUnitCode (if available in the input; optional)

- TADA.ResultMeasure.MeasureUnitCode (normalized canonical name)

- TADA.Target.ResultMeasure.MeasureUnitCode

- TADA.WQXUnitConversionFactor

- TADA.WQXUnitConversionCoefficient

## Details

Notes:

- Handles both prefixed and unprefixed unit columns
  (TADA.ResultMeasure.MeasureUnitCode and
  ResultMeasure.MeasureUnitCode). Internally normalizes to the
  TADA-prefixed name.

- Incorporates units observed only in detection-limit fields to support
  censored-data workflows.

- USGS unit synonyms (including units with speciation embedded) are
  normalized via TADA_GetUSGSSynonymRef. This facilitates moving
  speciation from units to TADA.MethodSpeciationName in
  TADA_ConvertResultUnits.

- WQX unit reference (TADA_GetMeasureUnitRef) provides default
  conversions, while TADA priority references
  (TADAPriorityCharUnitRef.csv and TADAPriorityCharConvertRef.csv) allow
  characteristic-specific targets and conversions that can override WQX
  defaults when necessary.

- For characteristics with multiple target units (e.g., fundamentally
  different unit families), an informational message is printed to
  prompt user review.

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
