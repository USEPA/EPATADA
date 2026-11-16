# nhd_get_hr_flowlines

Sends a set of AOIs to the NHD HR flowline service and returns matching
high-resolution flowline features.

## Usage

``` r
.nhd_get_hr_flowlines(nhd_hr_flowlines, geospatial_aoi)
```

## Arguments

- nhd_hr_flowlines:

  An ArcGIS layer or object representing the NHD HR flowlines dataset.

- geospatial_aoi:

  A list or vector of spatial features representing AOIs to query.
