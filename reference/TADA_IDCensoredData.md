# Prepare and Flag Censored Data

Identify and categorize censored data records using detection condition
text, detection limit type, and (if available) user-supplied measure
qualifier codes. Records are flagged as:

- "Non-Detect"

- "Over-Detect"

- "Other Condition/Limit Populated"

- "Conflict between Condition and Limit"

- "Detection condition is missing and required for censored data ID."

- "Detection condition or detection limit is not documented in TADA
  reference tables." Uncensored records are flagged as "Uncensored".

## Usage

``` r
TADA_IDCensoredData(.data)
```

## Arguments

- .data:

  A TADA dataframe. It must include the following columns:

  - ResultIdentifier

  - ResultMeasureValue

  - ResultDetectionConditionText

  - DetectionQuantitationLimitTypeName

  - TADA.ResultMeasureValueDataTypes.Flag

  - TADA.ResultMeasureValue

  - TADA.ResultMeasure.MeasureUnitCode

  - TADA.DetectionQuantitationLimitMeasure.MeasureValue

  - TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode

## Value

A TADA dataframe with:

- Added column TADA.CensoredData.Flag describing censored data category,
  and

- Possible updates to TADA.ResultMeasureValue,
  TADA.ResultMeasure.MeasureUnitCode, and
  TADA.ResultMeasureValueDataTypes.Flag, when detection limits are
  copied or conflicts are detected.

## Details

The function also copies detection limit values and units to
TADA.ResultMeasureValue and TADA.ResultMeasure.MeasureUnitCode when:

- ResultMeasureValue is the text "BPQL", "BDL", or "ND" and detection
  limit value/unit are present, or

- TADA.ResultMeasureValueDataTypes.Flag indicates "NA - Not Available"
  and detection limit value/unit are present (copy occurs only if the
  TADA result value/unit are actually NA).

When a conflict between detection condition and detection limit type is
found, the function sets TADA.ResultMeasureValue and
TADA.ResultMeasure.MeasureUnitCode to NA and updates
TADA.ResultMeasureValueDataTypes.Flag to "Result Value/Unit Cannot Be
Estimated From Detection Limit".

If TADA.MeasureQualifierCode.Flag is not already present in the input,
the function calls TADA_FlagMeasureQualifierCode(clean = FALSE)
internally to help identify censored results via measure qualifier
codes. Note that the internal flag column may be removed before
returning the final result; if you need to retain qualifier flags for
auditing, run TADA_FlagMeasureQualifierCode separately prior to calling
this function.

This function uses TADA reference tables from TADA_GetDetCondRef()
(detection condition domain) and TADA_GetDetLimitRef() (detection limit
type domain). If values found in the input dataframe are not present in
the reference tables, the function flags those records as "Detection
condition or detection limit is not documented in TADA reference
tables." and prints an informational message. Such records are excluded
from downstream censored data handling methods.

When ResultMeasureValue is a special text indicating censored data
("BPQL", "BDL", "ND"), and detection limit value/unit are present, the
detection limit value and unit are copied to TADA result value/unit and
TADA.ResultMeasureValueDataTypes.Flag is set to "Result Value/Unit
Copied from Detection Limit".

## See also

- TADA_SimpleCensoredMethods for simple censor handling

- TADA_FlagMeasureQualifierCode for measure qualifier flagging

- TADA_GetDetCondRef and TADA_GetDetLimitRef for domain reference tables

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Typical workflow with package dataset
utils::data(Data_Nutrients_UT)

# Flag censored data and inspect categories
dat_flagged <- TADA_IDCensoredData(Data_Nutrients_UT)

# Count records per censored category
table(dat_flagged$TADA.CensoredData.Flag)

# Review Non-Detects and confirm the detection limit value was copied
nd <- subset(dat_flagged, TADA.CensoredData.Flag == "Non-Detect")
head(nd[, c("ResultIdentifier",
  "ResultDetectionConditionText",
  "DetectionQuantitationLimitTypeName",
  "TADA.ResultMeasureValue",
  "TADA.ResultMeasure.MeasureUnitCode",
  "TADA.ResultMeasureValueDataTypes.Flag")])

# Review conflicts (values/units set to NA)
conf <- subset(dat_flagged, TADA.CensoredData.Flag == "Conflict between Condition and Limit")
head(conf[, c("ResultIdentifier",
  "ResultDetectionConditionText",
  "DetectionQuantitationLimitTypeName",
  "TADA.ResultMeasureValue",
  "TADA.ResultMeasure.MeasureUnitCode",
  "TADA.ResultMeasureValueDataTypes.Flag")])

# Example 2: Ensure measure qualifier flags are present (optional)
# This can improve censored result identification via user-supplied codes.
dat_mq <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT, clean = FALSE)
dat_flagged2 <- TADA_IDCensoredData(dat_mq)
table(dat_flagged2$TADA.CensoredData.Flag)
} # }
# Prepare and Flag Censored Data (NA/blank safe)
```
