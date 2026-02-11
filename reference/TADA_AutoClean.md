# TADA_AutoClean

This function performs several cleaning tasks on a TADA dataframe:

## Usage

``` r
TADA_AutoClean(.data)
```

## Arguments

- .data:

  TADA dataframe

## Value

Input dataframe with several added TADA-specific columns, including:

- TADA.ActivityMediaName (character)

- TADA.ResultSampleFractionText (character)

- TADA.CharacteristicName (character)

- TADA.MethodSpeciationName (character)

- TADA.ComparableDataIdentifier (character)

- TADA.ResultMeasureValue (numeric)

- TADA.ResultMeasureValueDataTypes.Flag (character)

- TADA.ResultMeasure.MeasureUnitCode (character)

- TADA.WQXResultUnitConversion (character)

- TADA.DetectionQuantitationLimitMeasure.MeasureValue (numeric)

- TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag
  (character)

- TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode (character)

- TADA.ResultDepthHeightMeasure.MeasureValue (numeric)

- TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag (character)

- TADA.ResultDepthHeightMeasure.MeasureUnitCode (character)

- TADA.ActivityDepthHeightMeasure.MeasureValue (numeric)

- TADA.ActivityDepthHeightMeasure.MeasureValueDataTypes.Flag (character)

- TADA.ActivityDepthHeightMeasure.MeasureUnitCode (character)

- TADA.ActivityTopDepthHeightMeasure.MeasureValue (numeric)

- TADA.ActivityTopDepthHeightMeasure.MeasureValueDataTypes.Flag
  (character)

- TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode (character)

- TADA.ActivityBottomDepthHeightMeasure.MeasureValue (numeric)

- TADA.ActivityBottomDepthHeightMeasure.MeasureValueDataTypes.Flag
  (character)

- TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode (character)

- TADA.LatitudeMeasure (numeric)

- TADA.LongitudeMeasure (numeric)

- TADA.MonitoringLocationIdentifier (character)

- TADA.MonitoringLocationName (character)

- TADA.MonitoringLocationTypeName (character)

Note: The number of TADA-specific depth columns in the returned
dataframe depends on the number of depth columns with one or more
results populated with a numeric value. If all depth columns contain
only NAs, no conversion is necessary and no TADA depth columns are
created.

## Details

1.  **Column Creation and Capitalization**: Creates new columns with the
    TADA prefix "TADA." and capitalizes all letters within them for
    interoperability with the WQX validation reference tables, reducing
    case-sensitivity issues when joining data. Affected columns include:

    - CharacteristicName

    - ResultSampleFractionText

    - MethodSpeciationName

    - ResultMeasure.MeasureUnitCode

    - ActivityMediaName

    - DetectionQuantitationLimitMeasure.MeasureUnitCode

2.  **Special Character Conversion**: Runs `TADA_ConvertSpecialChars` on
    the following columns and creates new versions with the TADA prefix:

    - ResultMeasureValue

    - DetectionQuantitationLimitMeasure.MeasureValue

3.  **Latitude and Longitude Conversion**: Converts the column type of
    LatitudeMeasure and LongitudeMeasure to numeric (double) and creates
    new columns with the TADA prefix.

4.  **Unit Code Replacement**: Replaces `meters` with `m` in the
    following columns:

    - TADA.ResultMeasure.MeasureUnitCode

    - ActivityDepthHeightMeasure.MeasureUnitCode

    - ActivityTopDepthHeightMeasure.MeasureUnitCode

    - ActivityBottomDepthHeightMeasure.MeasureUnitCode

    - ResultDepthHeightMeasure.MeasureUnitCode

5.  **Deprecated Characteristic Replacement**: Runs
    `TADA_SubstituteDeprecatedChars` to replace deprecated
    characteristic names based on the Water Quality Exchange (WQX)
    Characteristic domain table.

6.  **Result Unit Harmonization**: Runs `TADA_ConvertResultUnits` to
    harmonize result and detection limit units to WQX and TADA or
    user-supplied target units. For more details, see
    [`?TADA_ConvertResultUnits`](https://usepa.github.io/EPATADA/reference/TADA_ConvertResultUnits.md)
    and `?TADA_CreateUnitRef()`.

7.  **Depth Unit Conversion**: Runs `TADA_ConvertDepthUnits` to convert
    depth units to meters on the following columns, adding new columns
    with the TADA prefix:

    - ResultDepthHeightMeasure.MeasureValue

    - ActivityDepthHeightMeasure.MeasureValue

    - ActivityTopDepthHeightMeasure.MeasureValue

    - ActivityBottomDepthHeightMeasure.MeasureValue

8.  **Comparable ID Creation**: Runs `TADA_CreateComparableID` to create
    a comparable data group by concatenating:

    - TADA.CharacteristicName

    - TADA.ResultSampleFractionText

    - TADA.MethodSpeciationName

    - TADA.ResultMeasure.MeasureUnitCode

Original columns are not changed. New columns are appended to the
dataframe with the prefix `TADA.`. `TADA_AutoClean` can be run as a
standalone function but is primarily used by the `TADA_dataRetrieval`
function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Find web service URLs for each Profile using WQP User Interface:
# https://www.waterqualitydata.us/

# Example WQP URL:
# https://www.waterqualitydata.us/#statecode=US%3A09&characteristicType=Nutrient&
# startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&providers=NWIS&
# providers=STEWARDS&providers=STORET

# Define base URL and common components
baseurl <- "https://www.waterqualitydata.us"
filters <- "/search?statecode=US%3A09&characteristicType=Nutrient"
dates <- "&startDateLo=04-01-2023&startDateHi=11-01-2023"
type <- "&mimeType=csv&zip=yes"
providers <- "&providers=NWIS&providers=STEWARDS&providers=STORET"

# Construct URLs for different profiles
station_url <- paste0(baseurl, "/data/Station", filters, dates, type, providers)
result_url <- paste0(
  baseurl, "/data/Result", filters, dates, type,
  "&dataProfile=resultPhysChem", providers
)
project_url <- paste0(baseurl, "/data/Project", filters, dates, type, providers)

# Use TADA_ReadWQPWebServices to load Station, Project, and Phys-Chem Result profiles
stationProfile <- TADA_ReadWQPWebServices(station_url)
physchemProfile <- TADA_ReadWQPWebServices(result_url)
projectProfile <- TADA_ReadWQPWebServices(project_url)

# Join all three profiles using TADA_JoinWQPProfiles
TADAProfile <- TADA_JoinWQPProfiles(
  FullPhysChem = physchemProfile,
  Sites = stationProfile,
  Projects = projectProfile
)

# Run TADA_AutoClean
Autocleaned_TADAProfile <- TADA_AutoClean(TADAProfile)
} # }
```
