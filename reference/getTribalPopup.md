# Get text for tribal marker popup getTribalPopup is used within TADA_addPolys and TADA_addPoints

Get text for tribal marker popup getTribalPopup is used within
TADA_addPolys and TADA_addPoints

## Usage

``` r
getTribalPopup(layer, layername)
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
# Read in the Oklahoma Tribal Statistical Areas layer
layer <- readLayer("extdata", "Tribal.gpkg", "OKTribe")
# Get popup text for individual markers
getTribalPopup(layer, "Oklahoma Tribal Statistical Areas")
} # }
```
