# Retrieve feature layer from ArcGIS REST service getFeatureLayer is used by writeLayerIfChanged to write feature layers to local files

Retrieve feature layer from ArcGIS REST service getFeatureLayer is used
by writeLayerIfChanged to write feature layers to local files

## Usage

``` r
getFeatureLayer(url, bbox = NULL)
```

## Arguments

- url:

  URL of the layer REST service, ending with "/query". Example:
  https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query
  (American Indian Reservations)

- bbox:

  A bounding box from the sf function st_bbox; used to filter the query
  results. Optional; defaults to NULL.

## Value

ArcGIS feature layer

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example dataset
utils::data(Data_Nutrients_UT)
# Get the bounding box of the data
bbox <- sf::st_bbox(
  c(
    xmin = min(Data_Nutrients_UT$TADA.LongitudeMeasure),
    ymin = min(Data_Nutrients_UT$TADA.LatitudeMeasure),
    xmax = max(Data_Nutrients_UT$TADA.LongitudeMeasure),
    ymax = max(Data_Nutrients_UT$TADA.LatitudeMeasure)
  ),
  crs = sf::st_crs(Data_Nutrients_UT)
)
# Get the American Indian Reservations feature layer,
# filtered by the bounding box for the Data_Nutrients_UT example dataset
getFeatureLayer("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query", bbox)
} # }
```
