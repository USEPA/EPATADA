# Create point geometry for ATTAINS Assessment Units

Creates point or multipoint geometry for ATTAINS Assessment Units using
`ATTAINS.AssessmentUnitIdentifier` when available, otherwise
`TADA.MonitoringLocationIdentifier`.

## Usage

``` r
TADA_CreatePointAUGeometry(
  .data,
  target_crs = 4269,
  download_geo = FALSE,
  auid_prefix = NULL
)
```

## Arguments

- .data:

  A data frame containing:

  - `TADA.LongitudeMeasure`

  - `TADA.LatitudeMeasure`

  - `HorizontalCoordinateReferenceSystemDatumName`

  And at least one of:

  - `ATTAINS.AssessmentUnitIdentifier`

  - `TADA.MonitoringLocationIdentifier`

- target_crs:

  Numeric. Target EPSG code for the output CRS. If the input geometry is
  not the same as the target_crs, it is transformed to the target_crs.
  Default is 4269. If the input CRS and target_crs match, no CRS
  transformation occurs.

- download_geo:

  Logical. If `TRUE`, write a shapefile to the user's Downloads folder.

- auid_prefix:

  Character or `NULL`. If provided and non-empty, this prefix is applied
  only to newly created `ATTAINS.AssessmentUnitIdentifier` values when
  missing or blank.

## Value

An `sf` containing:

- `ATTAINS.AssessmentUnitIdentifier`

- `HorizontalCoordinateReferenceSystemDatumName`

- `epsg`

- `geometry`

If `download_geo = TRUE`, a shapefile is written to the Downloads
folder.

## Details

- Missing or blank `ATTAINS.AssessmentUnitIdentifier` values are
  replaced with `TADA.MonitoringLocationIdentifier`.

- Duplicate coordinate rows are removed before geometry creation.

- Input coordinates are assumed to be in EPSG:4269 before
  transformation.

- Single-location groups retain `POINT` geometry.

- Multi-location groups are written as `MULTIPOINT`.

## See also

[`TADA_CreatePointAUs()`](https://usepa.github.io/EPATADA/reference/TADA_CreatePointAUs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Build point geometry from package data
# The input must include longitude, latitude, datum, and at least one ID field.
df <- Data_TribalNations_Harmonized |>
  dplyr::filter(OrganizationFormalName == "Blackfeet Nation (Montana)")

au_points <- TADA_CreatePointAUGeometry(df)
unique(sf::st_geometry_type(au_points))

# Example 2: Demonstrate POINT vs MULTIPOINT output
# AU1 has two locations, so it becomes MULTIPOINT.
# AU2 has one location, so it remains POINT.
df2 <- data.frame(
  ATTAINS.AssessmentUnitIdentifier = c("AU1", "AU1", "AU2"),
  TADA.MonitoringLocationIdentifier = c("ML1", "ML1", "ML2"),
  TADA.LongitudeMeasure = c(-90, -90, -91),
  TADA.LatitudeMeasure = c(40, 41, 41),
  HorizontalCoordinateReferenceSystemDatumName = c("NAD83", "NAD83", "NAD83"),
  stringsAsFactors = FALSE
)

au_geoms <- TADA_CreatePointAUGeometry(df2)
unique(sf::st_geometry_type(au_geoms))
} # }
```
