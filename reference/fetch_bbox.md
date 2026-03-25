# fetch_bbox

This function gets ATTAINS data for the bounding box of a feature.

## Usage

``` r
fetch_bbox(baseurl, df)
```

## Arguments

- baseurl:

  A url for an ESRI REST service layer.

- df:

  An sf dataframe developed with
  [`TADA_MakeSpatial()`](https://usepa.github.io/EPATADA/reference/TADA_MakeSpatial.md).

## Value

An sf data frame of features from the REST service within the bounding
box of the spatial feature of interest.

## Examples

``` r
if (FALSE) { # \dontrun{
baseurl <- "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3"

df <- Data_MT_MissoulaCounty |>
      TADA_MakeSpatial()

features <- fetch_bbox(baseurl, df)
} # }
```
