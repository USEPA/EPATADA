# USGS Unit and Speciation Conversion Table

This internal reference file includes USGS only units/speciations. It
was created in July 2023 using the pcodes domain table from NWIS. All
USGS units and speciations are given a target unit and speciation that
is synonymous, but adheres to the WQX schema (WQX measure unit domain
table).

## Usage

``` r
TADA_GetUSGSSynonymRef()
```

## Value

Dataframe of USGS only units and speciations and their WQX compatible
targets/synonyms.

## Details

This reference file is used in the TADA_ConvertResultUnits() function
where synonymous units and speciations are harmonized before units are
then also harmonized/converted to WQX targets.
