# nhd_get_hr_catchments

Sends a set of bounding boxes or AOIs to the NHD HR catchment service
and returns matching high-resolution catchment features.

## Usage

``` r
.nhd_get_hr_catchments(nhd_hr_catchments, wqp_bboxes)
```

## Arguments

- nhd_hr_catchments:

  An ArcGIS layer or object representing the NHD HR catchments dataset.

- wqp_bboxes:

  A list or vector of spatial features representing bounding boxes or
  AOIs to query.
