# Data Dictionary for Criteria and Methodology Workbook

Create or refresh documentation tabs for the Criteria and Methodology
workbook used by TADA. This helper builds two worksheets:

- DataDictionary: human-readable definitions for each column in the
  Criteria/Methodology template (name, requirement, source, type,
  description).

- AllowableValues: curated domain references and example values for each
  column, including labeled hyperlinks to EPA ATTAINS domain values and
  WQX Characteristics.

## Usage

``` r
.TADA_CriteriaDataDictionary(downloads_path = NULL)
```

## Arguments

- downloads_path:

  Character string path to the Excel workbook to update (e.g.,
  "CriteriaMethodology.xlsx"). If NULL (default), the function attempts
  to locate the user's Downloads folder.

## Value

No return value; called for its side effects of creating or updating an
Excel workbook in the downloads_path. The function writes or refreshes:

- "DataDictionary" worksheet with columns: ColumnName, Requirement,
  Source, ColumnType, Description.

- "AllowableValues" worksheet with columns: ColumnName, ColumnType,
  AllowableValues, ExampleValues.

## Details

The function is primarily called by TADA_DefineCriteriaMethodology() to
ensure the workbook includes up-to-date guidance for users who fill out
criteria, methodology, and (optionally) equation parameterization.

If the target Excel file does not exist, a new workbook is created at
that path with base sheets "DefineCriteriaMethodology" and hidden
"Index-Criteria", then the two documentation tabs are added (or replaced
if already present).

## See also

[`TADA_DefineCriteriaMethodology()`](https://usepa.github.io/EPATADA/reference/TADA_DefineCriteriaMethodology.md)
[`TADA_ParametersForAnalysis()`](https://usepa.github.io/EPATADA/reference/TADA_ParametersForAnalysis.md)

## Examples

``` r
# Example 1: Write to a temporary path (recommended for reproducible scripts/tests)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  tmp_xlsx <- file.path(tempdir(), "CriteriaMethodology.xlsx")
  # Calling the internal function is possible within EPATADA package via :::,
  # but generally discouraged for users. Kept here only for demonstration.
  EPATADA:::.TADA_CriteriaDataDictionary(tmp_xlsx)
  openxlsx::getSheetNames(tmp_xlsx)
}
#> [1] "DefineCriteriaMethodology" "Index-Criteria"           
#> [3] "DataDictionary"            "AllowableValues"          

# Example 2: Use the default Downloads location (may vary by OS/user)
# \dontrun{
# .TADA_CriteriaDataDictionary()
# }
```
