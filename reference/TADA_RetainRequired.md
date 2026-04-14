# TADA_RetainRequired

This function removes all duplicate columns where TADA has created a new
column with a TADA prefix. It retains all TADA prefixed columns as well
as other original fields that are either required by other TADA
functions or are commonly used filters. Using this function allows the
user to accept all TADA created changes and reduce the size of the data
set before using TADA mapping or data visualization features in the TADA
package or Shiny app.

## Usage

``` r
TADA_RetainRequired(.data)
```

## Arguments

- .data:

  A dataframe

## Value

A dataframe containing all required fields for use with TADA as well as
fields commonly used for filtering. Removes non-required columns
containing only NA values.

## Details

This function also removes any columns not required for the TADA
workflow where all values are equal to NA. It provides a warning message
identifying any TADA required columns containing only NA values.

## Examples

``` r
utils::data(Data_Nutrients_UT)
reducedcols_Data_Nutrients_UT <- TADA_RetainRequired(Data_Nutrients_UT)
#> TADA_RetainRequired: removing columns not required for TADA workflow if they contain only NAs.
#> The following column(s) were removed as they contained only NAs and are not required for the TADA workflow: ActivityEndDate and ActivityEndDateTime.
#> TADA_RetainRequired: checking required columns for non-NA values.
#> TADA_RetainRequired: TADA Required column(s) ResultDepthHeightMeasure.MeasureValue, TADA.ResultDepthHeightMeasure.MeasureValue, DataQuality.PrecisionValue, DataQuality.BiasValue, DataQuality.ConfidenceIntervalValue, DataQuality.UpperConfidenceLimitValue and DataQuality.LowerConfidenceLimitValue contain only NA values. This may impact other TADA functions.
#> TADA_RetainRequired: removing columns not required for TADA workflow including original columns that have been replaced with TADA prefix duplicates.
#> TADA_RetainRequired: The following non-required columns were removed: ActivityDepthAltitudeReferencePointText, ActivityEndTime.Time, ActivityEndTime.TimeZoneCode, ActivityConductingOrganizationText, SampleAquifer, ActivityLocation.LatitudeMeasure, ActivityLocation.LongitudeMeasure, ResultWeightBasisText, ResultTemperatureBasisText, ResultParticleSizeBasisText, USGSPCode, BinaryObjectFileName, BinaryObjectFileTypeCode, AnalysisStartDate, ResultDetectionQuantitationLimitUrl, LabSamplePreparationUrl, ActivityStartTime.TimeZoneCode_offset, ActivityEndTime.TimeZoneCode_offset, SourceMapScaleNumeric, HorizontalAccuracyMeasure.MeasureValue, HorizontalAccuracyMeasure.MeasureUnitCode, HorizontalCollectionMethodName, VerticalMeasure.MeasureValue, VerticalMeasure.MeasureUnitCode, VerticalAccuracyMeasure.MeasureValue, VerticalAccuracyMeasure.MeasureUnitCode, VerticalCollectionMethodName, VerticalCoordinateReferenceSystemDatumName, FormationTypeText, ProjectMonitoringLocationWeightingUrl, DrainageAreaMeasure.MeasureValue, DrainageAreaMeasure.MeasureUnitCode, ContributingDrainageAreaMeasure.MeasureValue, ContributingDrainageAreaMeasure.MeasureUnitCode, ProviderName and LastUpdated.
```
