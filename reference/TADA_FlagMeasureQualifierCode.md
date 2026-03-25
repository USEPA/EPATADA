# Check for results with suspect Measure Qualifier Codes

This function inspects the 'MeasureQualifierCode' column and flags
results based on a reference table of qualifier codes. Results are
flagged as:

- "Suspect" for suspect qualifiers

- "Non-Detect" or "Over-Detect" for censored-data qualifiers (for later
  use in TADA_SimpleCensoredMethods)

- "Pass" when qualifiers indicate a passing result (or when no qualifier
  is provided)

- "Not Reviewed" for qualifiers not present in the WQX domain table

## Usage

``` r
TADA_FlagMeasureQualifierCode(
  .data,
  clean = FALSE,
  flaggedonly = FALSE,
  define = TRUE
)
```

## Arguments

- .data:

  A TADA dataframe which must include the column 'MeasureQualifierCode'.

- clean:

  Logical; default `FALSE`. When `TRUE`, rows flagged as "Suspect" are
  removed. Ignored when `flaggedonly = TRUE`.

- flaggedonly:

  Logical; default `FALSE`. When `TRUE`, returns only rows flagged as
  "Suspect" and overrides the `clean` argument.

- define:

  Logical; default `TRUE`. When `TRUE`, adds a column
  'TADA.MeasureQualifierCode.Def' with concatenated definitions for any
  qualifier codes present in each result. When `FALSE`, no definition
  column is added.

## Value

The input dataframe with:

- A new column 'TADA.MeasureQualifierCode.Flag' indicating the flag for
  each result: "Suspect", "Non-Detect", "Over-Detect", "Pass", or "Not
  Reviewed".

- Optionally, a new column 'TADA.MeasureQualifierCode.Def' (when
  `define = TRUE`), containing concatenated "Code - Description" strings
  for any qualifiers present.

Output shape depends on arguments:

- `clean = FALSE`, `flaggedonly = FALSE` (default): return all rows with
  flags (no removal).

- `clean = TRUE`, `flaggedonly = FALSE`: return all rows except those
  flagged "Suspect".

- `flaggedonly = TRUE` (regardless of `clean`): return only rows flagged
  "Suspect".

Messages:

- A message is printed when unknown qualifier codes are encountered;
  those rows are labeled "Not Reviewed". Blank tokens are ignored and
  will not produce messages.

- A message is printed if the entire 'MeasureQualifierCode' column is
  NA.

## Details

Multiple qualifiers in a single record are supported when delimited by
semicolons (;). Unknown qualifiers are assigned "Not Reviewed" and a
message is emitted.

- Requirements:

  - The input must contain a 'MeasureQualifierCode' column.

  - If `define = TRUE`, the input must also contain 'ResultIdentifier'
    to support building a definition column.

- Multiple codes:

  - When multiple qualifier codes are present in a single record
    (semicolon-separated), the following precedence is used when
    assigning the flag: Suspect \> Non-Detect \> Over-Detect \> Pass \>
    Not Reviewed.

- Unknown/missing/blank codes:

  - Any qualifier code not found in the WQX domain reference is labeled
    "Not Reviewed" and a message is issued.

- Definitions:

  - When `define = TRUE`, an additional column
    'TADA.MeasureQualifierCode.Def' is added, containing concatenated
    "Code - Description" definitions (for all codes present in a
    record).

- Cleaning vs filtering:

  - `clean = TRUE` removes only rows flagged as "Suspect". Other flags
    (e.g., "Non-Detect", "Over-Detect", "Not Reviewed") are retained.

  - `flaggedonly = TRUE` overrides `clean` and returns only rows flagged
    as "Suspect".

## See also

[`TADA_GetMeasureQualifierCodeRef()`](https://usepa.github.io/EPATADA/reference/TADA_GetMeasureQualifierCodeRef.md)

[`TADA_SimpleCensoredMethods()`](https://usepa.github.io/EPATADA/reference/TADA_SimpleCensoredMethods.md)

## Examples

``` r
# Load example dataset:
utils::data(Data_Nutrients_UT)

# 1) Flag all records and keep everything:
mq_flagged <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT)

# 2) Return only suspect records (flaggedonly overrides clean):
mq_suspect_only <- TADA_FlagMeasureQualifierCode(
  Data_Nutrients_UT,
  flaggedonly = TRUE
)

# 3) Remove suspect records but retain Non-Detect, Over-Detect, Not Reviewed, and Pass:
mq_clean <- TADA_FlagMeasureQualifierCode(Data_Nutrients_UT, clean = TRUE)

# 4) Remove suspect records and skip adding the definition column:
mq_clean_nodefs <- TADA_FlagMeasureQualifierCode(
  Data_Nutrients_UT,
  clean = TRUE, define = FALSE
)
```
