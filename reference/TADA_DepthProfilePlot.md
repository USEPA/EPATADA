# Create A Three-Characteristic Depth Profile

Create A Three-Characteristic Depth Profile

## Usage

``` r
TADA_DepthProfilePlot(
  .data,
  groups = NULL,
  location = NULL,
  activity_date = NULL,
  depthcat = TRUE,
  surfacevalue = 2,
  bottomvalue = 2,
  unit = "m"
)
```

## Arguments

- .data:

  TADA data frame containing the data downloaded from the WQP, where
  each row represents a unique data record. TADA_FlagDepthCategory has
  been run as data frame must include the columns
  TADA.DepthCategory.Flag,
  TADA.ResultDepthHeightMeasure.MeasureUnitCode,
  TADA.ActivityDepthHeightMeasure.MeasureUnitCode, and
  TADA.ActivityDepthHeightMeasure.MeasureValue. Units for all depth
  fields must be the same. This can be accomplished using
  TADA_AutoClean() or TADA_ConvertDepthUnits.

- groups:

  A vector of two identifiers from the TADA.ComparableDataIdentifier
  column. For example, the groups could be 'DISSOLVED OXYGEN
  (DO)\_NA_NA_UG/L' and 'PH_NA_NA_NA'. These groups will be specific to
  your data frame. The TADA_IDDepthProfiles can be used to identify
  available groups.

- location:

  A single TADA.MonitoringLocationIdentifier to plot the depth profile.
  A TADA.MonitoringLocationIdentifier must be entered or an error will
  be returned and no depth profile will be created.

- activity_date:

  The date the depth profile results were collected.

- depthcat:

  Boolean argument indicating whether delineation between depth
  categories should be shown on the depth profile figure. depthcat =
  TRUE is the default and displays solid black lines to delineate
  between surface, middle, and bottom samples and labels each section of
  the plot.

- surfacevalue:

  numeric argument. The user enters how many meters from the surface
  should be included in the "Surface" category. Default is surfacevalue
  = 2.

- bottomvalue:

  numeric argument. The user enters how many meters from the bottom
  should be included in the "Bottom" category. Default is bottomvalue =
  2.

- unit:

  Character argument. The enters either "m" or "ft" to specify which
  depth units should be used for the plot. Default is "m".

## Value

A depth profile plot displaying up to three parameters for a single
TADA.MonitoringLocationIdentifier. Displaying depth categories is
optional with the depthcat argument.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataframe:
utils::data(Data_TribalNations_Harmonized)
# Create a depth profile figure with three parameters for a single
# monitoring location and date
TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
groups = c("TEMPERATURE_NONE_NONE_DEG C", "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
"DEPTH, SECCHI DISK DEPTH_NONE_NONE_M"),
location = "REDLAKE_WQX-BASS-SE",
activity_date = "2025-07-16"
)

# Load example data frame:
utils::data(Data_TribalNations_Harmonized)
# Create a depth profile figure with two parameters for a single monitoring
# location and date without displaying depth categories
TADA_DepthProfilePlot(Data_TribalNations_Harmonized,
groups = c("TEMPERATURE_NONE_NONE_DEG C", "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"),
location = "REDLAKE_WQX-NONA",
activity_date = "2024-02-05",
depthcat = FALSE
)
} # }
```
