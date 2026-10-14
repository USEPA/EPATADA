# .nhd_get_nhdplus

Sends an area of interest (AOI) to hydrogeofetch and asks for a specific
NHDPlus V2 feature type using realization. For NHD Med resolution.

## Usage

``` r
.nhd_get_nhdplus(AOI, realization)
```

## Arguments

- AOI:

  Area of interest. This is usually an sf object representing the site
  or catchment used as input to hydrogeofetch.

- realization:

  Character argument. What kind of NHD feature to retrieve with
  hydrogeofetch, such as: "catchment" or "flowline".
