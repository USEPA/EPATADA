# TADA_listNWIS (DRAFT - UNDER ACTIVE DEVELOPMENT)

Retrieves available metadata from USGS National Water Information System
(NWIS) based on different spatial queries: area of interest (AOI),
specific sites, or state boundaries. Returns a spatial sf object
containing continuous monitoring site information and available
parameters and statistics. If no data is found, returns an empty sf
object with appropriate column structure.

## Usage

``` r
TADA_listNWIS(aoi_sf = "null", statecode = "null", siteid = "null")
```

## Arguments

- aoi_sf:

  An sf object defining the area of interest. All individual sf features
  (or "rows" in the sf data frame) must be under 118,078 square miles
  (roughly the area of Nevada).

- statecode:

  Character vector of two-letter state codes (e.g., c("CA", "OR")).

- siteid:

  Character vector of USGS site numbers.

## Value

An sf object containing NWIS continuous monitoring site summary
information including:

- site_no: USGS site identification number

- site_name: Station name

- site_type: Description of the site type

- site_type_cd: Site type code

- data_type: Type of data available (e.g., "Daily", "Water Quality")

- data_type_cd: Code identifying type of data/service (e.g. "dv" = Daily
  Values)

- stat_type: Statistic type

- stat_cd: Statistic code

- parameter: Parameter name and description

- parameter_code: Parameter code

- n_obs: Number of observations

- begin_date: Start date of data collection

- end_date: End date of data collection

- geometry

Returns an empty sf object with the same structure if no data is found.

## Details

List USGS continuous monitoring sites with available daily statistics

Only one of the query arguments (`aoi_sf`, `statecode`, or `siteid`)
should be provided. The function will stop if none or more than one are
provided. Moreover, all sf features must be under 118,078 square miles
(roughly the area of Nevada).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Query by area of interest
navajo_sf <- sf::read_sf("inst/extdata/AmericanIndian.shp") %>%
  dplyr::filter(NAME == "Navajo Nation")
sites_aoi_sf <- TADA_listNWIS(aoi_sf = navajo_sf)

# Example 2: Query by specific site numbers
site_nums <- c("11530500", "11532500")
sites_specific <- TADA_listNWIS(siteid = site_nums)

# Example 3: Query by state
sites_state <- TADA_listNWIS(statecode = c("CT", "RI"))
} # }
```
