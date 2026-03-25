# Update All TADA Reference Files (Internal)

This internal function updates all TADA reference files by calling a
series of update functions. It is used to ensure that all reference data
is current and accurate across various datasets and geospatial layers.

## Usage

``` r
.TADA_UpdateRefFiles()
```

## Value

This function does not return any value. It performs updates as a side
effect.

## Details

The function sequentially calls several internal functions that update
different sets of reference data. Some updates may take longer than
others, particularly the
[`.TADA_UpdateATTAINSParamUseOrgRef()`](https://usepa.github.io/EPATADA/reference/dot-TADA_UpdateATTAINSParamUseOrgRef.md)
function.

The specific reference files updated by this function include:

- ATTAINS organization IDs

- ATTAINS parameter use organization

- ATTAINS parameter WQP characteristics

- WQX characteristic values

- Measurement units

- Detection conditions

- Detection limits

- Activity types

- Characteristics

- Measurement qualifier codes

- Monitoring location types

- WQP organization providers

- EPA CST references

- Tribal geospatial layers

## Examples

``` r
if (FALSE) { # \dontrun{
# Internal function to update all reference files
.TADA_UpdateRefFiles()
} # }
```
