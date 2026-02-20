# getMapIconLabels

Internal function to get list of icons (images) and/or a list of their
labels for use in TADA mapping functions.

## Usage

``` r
getMapIconLabels(icons = TRUE, labels = TRUE)
```

## Arguments

- icons:

  Boolean argument. When icons = TRUE, the list of images used for TADA
  maps is returned. When icons = FALSE, no list of images is returned.
  Default is icons = TRUE.

- labels:

  Boolean argument. When labels = TRUE, the list of labels used for TADA
  maps is returned. When labels = FALSE, no list of labels is returned.
  Default is labels = TRUE.

## Value

Depending on user input either one list (either labels or icons) or a
list containing both the labels list and the icons list.
