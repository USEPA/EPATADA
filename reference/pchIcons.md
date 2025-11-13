# Create icon(s) to be used to represent points on a map feature layer pchIcons is used within TADA_addPoints

Uses the different plotting symbols available in R to create PNG files
that can be used as markers on a map feature layer.

## Usage

``` r
pchIcons(
  pch = 1,
  width = 30,
  height = 30,
  bg = "transparent",
  col = "black",
  lwd = NULL
)
```

## Arguments

- pch:

  Plot character code; either a single number or a vector of multiple
  numbers. Possible values available at
  https://www.geeksforgeeks.org/r-plot-pch-symbols-different-point-shapes-available-in-r/.
  Defaults to 1 (an open circle).

- width:

  Width of the plot character. Defaults to 30 pixels.

- height:

  Height of the plot character. Defaults to 30 pixels.

- bg:

  Background color of the plot character Defaults to transparent.

- col:

  Color(s) of the plot character(s). Defaults to black.

- lwd:

  Line width. Optional, defaults to NULL.

## Value

Path(s) to PNG file(s) in a temp folder on user's computer.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create three PNG files, a red circle, blue triangle, and yellow "X", each on a green background.
pchIcons(c(1, 2, 4), 40, 40, "green", c("red", "blue", "yellow"))
} # }
```
