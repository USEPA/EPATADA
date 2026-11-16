# nhd_get_med_flowlines

Sends a set of sites to the NHD Medium resolution flowline service and
returns matching flowline features.

## Usage

``` r
.nhd_get_med_flowlines(unique_sites, geospatial_data)
```

## Arguments

- unique_sites:

  A spatial data frame or sf object containing unique site geometries
  used as query inputs.

- geospatial_data:

  Optional geospatial data used in the query pipeline.
