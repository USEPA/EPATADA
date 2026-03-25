# Simple Tools for Censored Data Handling

Apply simple, non-frequency-based methods to populate values for
censored results categorized as "Non-Detect" (lower limit) or
"Over-Detect" (upper limit). This function first ensures censored data
are identified and categorized by calling TADA_IDCensoredData (if
needed), then fills result values using one of the supported methods:

- For Non-Detects (lower limits):

  - "multiplier": value = nd_multiplier × lower detection limit
    (default)

  - "randombelowlimit": value = U(0, 1) × lower detection limit (a new
    random uniform value per row)

  - "as-is": retain lower detection limit value unchanged

- For Over-Detects (upper limits):

  - "multiplier": value = od_multiplier × upper detection limit

  - "as-is": retain upper detection limit value unchanged (default)

## Usage

``` r
TADA_SimpleCensoredMethods(
  .data,
  nd_method = "multiplier",
  nd_multiplier = 0.5,
  od_method = "as-is",
  od_multiplier = "null"
)
```

## Arguments

- .data:

  A TADA dataframe. If TADA.CensoredData.Flag is not present, the
  function will call TADA_IDCensoredData to generate it (and may call
  TADA_FlagMeasureQualifierCode inside that step).

- nd_method:

  Character. Method for Non-Detect handling: "multiplier" (default),
  "randombelowlimit", or "as-is".

- nd_multiplier:

  Numeric scalar. Multiplier applied to the LOWER detection limit when
  nd_method = "multiplier". Default is 0.5 (half the detection limit).

- od_method:

  Character. Method for Over-Detect handling: "multiplier" or "as-is"
  (default).

- od_multiplier:

  Numeric scalar or "null". Multiplier applied to the UPPER detection
  limit when od_method = "multiplier". When od_method = "as-is", this
  parameter is ignored. If od_method = "multiplier", a numeric
  multiplier must be supplied.

## Value

A TADA dataframe with:

- Updated TADA.ResultMeasureValue for censored records according to the
  chosen method(s),

- Updated TADA.ResultMeasureValueDataTypes.Flag documenting that values
  were estimated from detection limits,

- Added TADA.CensoredMethod indicating the method used (e.g., "Detection
  Limit Value Multiplied by 0.5", "Random Value Between 0 and Detection
  Limit Using this Multiplier: 0.317"),

- For problematic records (missing detection condition or undocumented
  types), values/units are set to NA and the data type flag indicates
  they cannot be estimated from detection limits.

## Details

Records flagged as:

- "Detection condition is missing and required for censored data ID."

- "Detection condition or detection limit is not documented in TADA
  reference tables." are set to NA for TADA.ResultMeasureValue and
  TADA.ResultMeasure.MeasureUnitCode and given
  TADA.ResultMeasureValueDataTypes.Flag = "Result Value/Unit Cannot Be
  Estimated From Detection Limit".

If the dataframe contains no censored results, the function returns the
input unchanged and prints an informational message.

This function assumes detection limits have already been copied to
TADA.ResultMeasureValue and TADA.ResultMeasure.MeasureUnitCode for
censored records by TADA_IDCensoredData when feasible. It coerces
TADA.ResultMeasureValue to numeric before applying multipliers or
randomness.

Required columns in .data (either pre-existing or created by
TADA_IDCensoredData):

- ResultIdentifier

- ResultMeasureValue

- ResultDetectionConditionText

- DetectionQuantitationLimitTypeName

- TADA.ResultMeasureValueDataTypes.Flag

- TADA.ResultMeasureValue

- TADA.ResultMeasure.MeasureUnitCode

- TADA.CensoredData.Flag

## See also

- TADA_IDCensoredData for censored data identification and flagging

- TADA_FlagMeasureQualifierCode for measure qualifier flagging that can
  aid in censor identification

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataframe:
utils::data(Data_Nutrients_UT)

# Example 1: Fill Non-Detects with half the lower detection limit,
# keep Over-Detects as-is
dat_simple <- TADA_SimpleCensoredMethods(
  Data_Nutrients_UT,
  nd_method = "multiplier",
  nd_multiplier = 0.5,
  od_method = "as-is",
  od_multiplier = "null"
)
table(dat_simple$TADA.CensoredData.Flag)
head(subset(dat_simple, TADA.CensoredData.Flag == "Non-Detect")[
  , c("ResultIdentifier", "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode", "TADA.CensoredMethod")
])

# Example 2: Randomize Non-Detects below the detection limit, keep Over-Detects as-is
set.seed(42)
dat_rand <- TADA_SimpleCensoredMethods(
  Data_Nutrients_UT,
  nd_method = "randombelowlimit",
  nd_multiplier = "null",
  od_method = "as-is",
  od_multiplier = "null"
)
head(subset(dat_rand, TADA.CensoredData.Flag == "Non-Detect")[
  , c("ResultIdentifier", "TADA.ResultMeasureValue", "TADA.CensoredMethod")
])

# Example 3: Apply multipliers to both Non-Detects and Over-Detects
dat_both <- TADA_SimpleCensoredMethods(
  Data_Nutrients_UT,
  nd_method = "multiplier",
  nd_multiplier = 0.2,
  od_method = "multiplier",
  od_multiplier = 0.8
)
head(subset(dat_both, TADA.CensoredData.Flag %in% c("Non-Detect", "Over-Detect"))[
  , c("ResultIdentifier", "TADA.ResultMeasureValue", "TADA.CensoredMethod")
])

# Example 4: If no censored data are present, the function returns the input
dat_unc <- subset(Data_Nutrients_UT, FALSE)  # empty subset for illustration
res <- TADA_SimpleCensoredMethods(dat_unc)
# message: "Cannot apply simple censored methods to dataframe with no censored
# data results. Returning input dataframe."
} # }
```
