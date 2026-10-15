# Create an ATTAINS AU–ML Crosswalk from WQP Monitoring Location IDs for New Point AUs

Build a distinct crosswalk between WQP Monitoring Locations and ATTAINS
Assessment Units. Missing or blank `ATTAINS.AssessmentUnitIdentifier`
values are filled from `TADA.MonitoringLocationIdentifier`, optionally
prefixed with `auid_prefix`. Existing non-missing, non-blank AUIDs are
left unchanged. Optionally, this function can also return either a df or
a shp file containing the point or multipoint geometry and its
corresponding assessment unit identifier.

## Usage

``` r
TADA_CreatePointAUs(
  .data,
  auid_prefix = NULL,
  create_geo = FALSE,
  download_geo = FALSE
)
```

## Arguments

- .data:

  A data frame containing, at minimum:

  - `TADA.MonitoringLocationIdentifier`

  If `create_geo = TRUE`, the input must also contain:

  - `TADA.LatitudeMeasure`

  - `TADA.LongitudeMeasure`

  - `HorizontalCoordinateReferenceSystemDatumName`

  If missing water-type values need to be crosswalked, the input must
  also contain:

  - `TADA.MonitoringLocationTypeName`

  Optional columns:

  - `ATTAINS.AssessmentUnitIdentifier`

  - `ATTAINS.WaterType`

- auid_prefix:

  Character or `NULL`. If provided and non-empty, this prefix is added
  only to newly created `ATTAINS.AssessmentUnitIdentifier` values.

- create_geo:

  Logical. If `TRUE`, also create point geometry using
  [`TADA_CreatePointAUGeometry()`](https://usepa.github.io/EPATADA/reference/TADA_CreatePointAUGeometry.md).

- download_geo:

  Logical. If `TRUE`, write the geometry shapefile to the user's
  downloads folder.

## Value

If `create_geo = FALSE`, a distinct crosswalk data frame containing:

- `ATTAINS.MonitoringLocationIdentifier`

- `ATTAINS.AssessmentUnitIdentifier`

- `ATTAINS.WaterType`

If `create_geo = TRUE`, a named list with:

- `crosswalk`

- `geometry`

## Details

If `ATTAINS.WaterType` is missing or contains any blank values, the
function attempts to populate it by calling
[`TADA_UpdateATTAINSWaterTypes()`](https://usepa.github.io/EPATADA/reference/TADA_UpdateATTAINSWaterTypes.md)
internally with `overwrite_existing = FALSE` and `validation = "none"`.

- Missing or blank `ATTAINS.AssessmentUnitIdentifier` values are
  replaced with `TADA.MonitoringLocationIdentifier`.

- If `auid_prefix` is supplied and non-empty, it is used only for newly
  created AUIDs.

- `ATTAINS.MonitoringLocationIdentifier` is created from
  `TADA.MonitoringLocationIdentifier`.

- `ATTAINS.WaterType` is not overwritten unless it is missing or blank.

## See also

[`TADA_UpdateATTAINSWaterTypes()`](https://usepa.github.io/EPATADA/reference/TADA_UpdateATTAINSWaterTypes.md),
[`TADA_CreatePointAUGeometry()`](https://usepa.github.io/EPATADA/reference/TADA_CreatePointAUGeometry.md)
