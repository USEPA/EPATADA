# Create Color Palette For Use in Graphs and Maps

Creates a consistent color palette for use in TADA visualizations.
Consistent color pairings can be utilized by setting col_pair = TRUE, in
which each row consists of two values for color outlines and fills.
Currently, the palette is utilizing the "Okabe-Ito" palette from base R
via the palette.colors function. The palette includes 9 colors by
default. However, additional colors can be added to the palette as
needed as more complex visualization functions are added to the TADA
package.

## Usage

``` r
TADA_ColorPalette(col_pair = FALSE)
```

## Arguments

- col_pair:

  Boolean argument. Optional argument to define consistent color
  pairings for outlines/fills of TADA figures defined by the row values
  in a dataframe.

## Value

A color palette based on the "Okabe-Ito" palette, extended to 15 colors,
with modifications for use in mapping and graphing functions

## Examples

``` r
TestColorPalette <- TADA_ColorPalette()
TestColorPalettePairings <- TADA_ColorPalette(col_pair = TRUE)
TestColorPalettePairings
#>      col1    col2
#> 1 #56B4E9 #1E6F98
#> 2 #DC851E #813B00
#> 3 #A1A522 #4F5900
#> 4 #B686A1 #835A00
```
