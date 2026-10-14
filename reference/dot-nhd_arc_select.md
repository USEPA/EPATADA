# .nhd_arc_select

Select features from a layer using geometry for NHD hi res.

## Usage

``` r
.nhd_arc_select(layer, filter_geom, crs)
```

## Arguments

- layer:

  The already-selected map layer object.

- filter_geom:

  The geometry used to spatially filter features, usually an sf geometry
  or sfc object.

- crs:

  The coordinate reference system for the geometry used in the query,
  usually from sf::st_crs(...).
