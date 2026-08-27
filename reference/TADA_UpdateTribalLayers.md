# Function to update tribal layer shapefiles. Tribal geopackage is stored in inst/extdata. Existing layers with the same name will be deleted first if they exist.

Function to update tribal layer shapefiles. Tribal geopackage is stored
in inst/extdata. Existing layers with the same name will be deleted
first if they exist.

## Usage

``` r
TADA_UpdateTribalLayers(tribal_gpkg = NULL)
```

## Arguments

- tribal_gpkg:

  full path to the tribal geopackage file. Default is
  "inst/extdata/Tribal.gpkg".
