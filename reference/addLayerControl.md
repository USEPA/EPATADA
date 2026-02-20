# addLayerControl Internal function to add layer control to a TADA leaflet map to toggle the legend on/off.

addLayerControl Internal function to add layer control to a TADA leaflet
map to toggle the legend on/off.

## Usage

``` r
addLayerControl(map = NULL, overlay_groups = NULL)
```

## Arguments

- map:

  A TADA leaflet map to add layer control to.

- overlay_groups:

  Initialized vector to add names of groups to map. This is to allow
  users to toggle specific layers on/off. If it is NULL, the function
  will fail with an error message. Default is overlay_list = NULL.

## Value

A TADA map with the layer control added.
