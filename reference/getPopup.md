# Get text for tribal marker popup getPopup is used within TADA_addPolys and TADA_addPoints

Get text for tribal marker popup getPopup is used within TADA_addPolys
and TADA_addPoints

## Usage

``` r
getPopup(layer, layername)
```

## Arguments

- layer:

  A map feature layer

- layername:

  Name of the layer

## Value

Vector of strings to be used as the text for the popups when clicking on
a tribal marker

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the Oklahoma Tribal Statistical Areas feature layer
layer <- getLayer("extdata/OKTribe.shp")
# Get popup text for individual markers
getPopup(layer, "Oklahoma Tribal Statistical Areas")
} # }
```
