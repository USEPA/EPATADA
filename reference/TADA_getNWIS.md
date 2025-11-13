# Retrieve and tidy daily values from NWIS (DRAFT - UNDER ACTIVE DEVELOPMENT)

This function interfaces with the USGS National Water Information System
(NWIS) to retrieve daily values (DV) water quality data using the TADA
(Tools for Automated Data Analysis) framework. Users can query data
based on a spatial area of interest (AOI), a vector of state
abbreviations, or a vector of specific site ids, along with relevant
USGS parameter codes, statistics to return, and a date range.

## Usage

``` r
TADA_getNWIS(
  aoi_sf = "null",
  statecode = "null",
  siteid = "null",
  parameter_codes,
  stat_codes = "00003",
  start_date,
  end_date
)
```

## Arguments

- aoi_sf:

  An sf object defining the area of interest. All individual sf features
  (or "rows" in the sf data frame) must be under 118,078 square miles
  (roughly the area of Nevada).

- statecode:

  A character vector of two-letter state abbreviations (e.g., `"CA"`,
  `"NY"`).

- siteid:

  A character vector of USGS site numbers.

- parameter_codes:

  A character vector of NWIS parameter codes to filter for (e.g.,
  `"00060"` for discharge). Parameter codes and names can be found at
  https://help.waterdata.usgs.gov/parameter_cd?group_cd=%

- stat_codes:

  A character vector of statistical types (e.g, the daily mean, the
  daily maximum, etc.) to return. Statistical code names can be found at
  https://help.waterdata.usgs.gov/stat_code. Default is mean ("00003").

- start_date:

  A character string representing the start date for data retrieval in
  `"YYYY-MM-DD"` format.

- end_date:

  A character string representing the end date for data retrieval in
  `"YYYY-MM-DD"` format.

## Value

A tidy `data.frame` containing daily values for each site, date, and
parameter, including a corresponding status code for each measurement.

## Details

Only one of the query arguments (`aoi_sf`, `statecode`, or `siteid`)
should be provided. The function will stop if none or more than one are
provided. Moreover, all sf features must be under 118,078 square miles
(roughly the area of Nevada).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Query by area of interest
locs_sf <- sf::read_sf("inst/extdata/AmericanIndian.shp") %>%
  dplyr::filter(NAME %in% c("Spokane", "Navajo Nation"))
sites_aoi_sf <- TADA_getNWIS(
  aoi_sf = locs_sf,
  parameter_codes =
    c("00060", "00010"),
  start_date = "2020-01-01",
  end_date = "2020-01-31"
)

# Example 2: Query by specific site numbers
sites_specific <- TADA_getNWIS(
  siteid = c("11530500", "11532500"),
  parameter_codes = c("00060", "00010"),
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)

# Example 3: Query by statecode
nwis_data <- TADA_getNWIS(
  statecode = c("RI", "CO"),
  stat_codes = c("00001"),
  parameter_codes = c("00010"),
  start_date = "2020-01-01",
  end_date = "2020-01-02"
)
} # }
```
