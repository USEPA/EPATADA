# Access options available for querying tribal spatial data with `TADA_DataRetrieval()`.

This function provides access to [six layer
datasets](https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer)
containing spatial data related to tribal lands: "Alaska Native
Allotments", "Alaska Native Villages", "American Indian Reservations",
"Off-reservation Trust Lands", "Oklahoma Tribal Statistical Areas", and
"Virginia Federally Recognized Tribes". These datasets are used by
[`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
when retrieving spatial data for tribal lands specified by the user.

The purpose of `TADA_TribalOptions()` is to allow the user to review the
available data in those datasets and identify the records they would
like to query with
[`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md).

An interactive map of the six layer datasets is available on ArcGIS
Online Map Viewer here:
https://www.arcgis.com/apps/mapviewer/index.html?url=https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer&source=sd

## Usage

``` r
TADA_TribalOptions(tribal_area_type, return_sf = FALSE)
```

## Arguments

- tribal_area_type:

  A character string. Must be one of the six tribal spatial layers:
  "Alaska Native Allotments", "Alaska Native Villages", "American Indian
  Reservations", "Off-reservation Trust Lands", "Oklahoma Tribal
  Statistical Areas", or "Virginia Federally Recognized Tribes".

- return_sf:

  Logical. Should the function return the dataset as an `sf` object
  (TRUE) or a data frame (FALSE)? Defaults to FALSE.

## Value

A data frame or `sf` object containing the specified layer from the EPA
Map Service.

## Note

Alaska Native Villages and Virginia Federally Recognized Tribes are
point geometries in the Map Service, not polygons. At the time of this
writing they do not return any data when used for WQP bbox queries.

## See also

[`TADA_DataRetrieval()`](https://usepa.github.io/EPATADA/reference/TADA_DataRetrieval.md)
