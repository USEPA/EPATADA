# Suspect coordinates

This function identifies and flags Suspect coordinate data. When
clean_outsideUSA = "no" and clean_imprecise = FALSE, a column will be
appended titled "TADA.SuspectCoordinates.Flag" with the following
flags: 1) If the latitude is less than zero, the row will be flagged
with "LAT_OutsideUSA" (with the exception of American Samoa, Northern
Mariana Islands, and Guam), 2) If the longitude is greater than zero AND
less than 145, the row will be flagged as "LONG_OutsideUSA" (with the
exception of American Samoa, Northern Mariana Islands, and Guam), and 3)
Finally, precision can be measured by the number of decimal places in
the latitude and longitude provided. If either the latitude or longitude
does not have at least three numbers to the right of the decimal point,
the row will be flagged as "Imprecise_lessthan3decimaldigits".
Occasionally latitude and longitude measurements are flagged as outside
of the United States because the data was entered as negative when it
should be positive or vice versa. This function offers the option of
clean_outsideUSA = "change sign" to fix this issue. However, data owners
should fix the raw data through WQX. For assistance with changing raw
data, email the WQX help desk: <WQX@epa.gov>

## Usage

``` r
TADA_FlagCoordinates(
  .data,
  clean_outsideUSA = c("no", "remove", "change sign"),
  clean_imprecise = FALSE,
  flaggedonly = FALSE,
  check_location_metadata = FALSE
)
```

## Arguments

- .data:

  TADA dataframe

- clean_outsideUSA:

  Character argument with options "no", "remove", and "change sign";
  flags coordinates as outside the USA when clean_outsideUSA = "no";
  removes data with coordinates outside of the United States when
  clean_outsideUSA = "remove"; changes sign of lat/long coordinates
  flagged as outside the USA when clean_outside = "change sign"; Default
  is clean_outsideUSA = "no".

- clean_imprecise:

  Boolean argument; removes imprecise data when clean_imprecise = TRUE.
  Default is clean_imprecise = FALSE.

- flaggedonly:

  Boolean argument; Return only flagged data when flaggedonly = TRUE;
  default is flaggedonly = FALSE.

- check_location_metadata:

  Boolean argument; Flags coordinates if location metadata does not
  match with coordinate location; default is check_location_metadata =
  FALSE.

## Value

Returns input TADA dataset with the added "TADA.SuspectCoordinates.Flag"
column. When clean_outsideUSA is "no", "change sign", or clean_imprecise
argument is FALSE, a column flagging rows with the respective QA check
is appended to the input dataframe. When clean_outsideUSA is "remove" or
clean_imprecise is TRUE, "Suspect" or "imprecise" data is removed,
respectively. When flaggedonly is TRUE, the dataframe will be filtered
to show only the data flagged as Suspect, imprecise, or out of the
United States. Defaults are clean_outsideUSA = "no", clean_imprecise =
FALSE, and flaggedonly = FALSE.

## Examples

``` r
# Create a small mock dataset with minimal required columns.
# Rows cover: Pass, LAT_OutsideUSA, LONG_OutsideUSA, American Samoa,
# Northern Mariana Islands, Guam, and an imprecise coordinate.
mock_coords <- data.frame(
  ID = c(
    "Pass_US_mainland",
    "Lat_outside",
    "Long_outside",
    "American_Samoa",
    "Northern_Mariana_Islands",
    "Guam",
    "Imprecise"
  ),
  TADA.LatitudeMeasure = c(
    38.8977,  # Pass
    -5.0000,  # LAT_OutsideUSA
    40.0000,  # LONG_OutsideUSA candidate
    -13.5000, # American Samoa
    15.0000,  # Northern Mariana Islands
    13.4000,  # Guam
    35.12     # Imprecise (< 3 decimal places)
  ),
  TADA.LongitudeMeasure = c(
    -77.0365, # Pass
    -120.0000, # LAT_OutsideUSA
    10.0000,   # LONG_OutsideUSA
    -170.0000, # American Samoa
    145.5000,  # Northern Mariana Islands
    144.8500,  # Guam
    -120.0     # Imprecise (<= 1 decimal place)
  ),
  stringsAsFactors = FALSE
)

# Flag, but do not remove, suspect coordinates:
SuspectCoord_flags <- TADA_FlagCoordinates(mock_coords)

# Return only flagged rows:
SuspectCoord_flags_flaggedonly <- TADA_FlagCoordinates(
  mock_coords,
  flaggedonly = TRUE
)

# Remove data with coordinates outside the USA, but keep imprecise data:
OutsideUSACoord_removed <- TADA_FlagCoordinates(
  mock_coords,
  clean_outsideUSA = "remove"
)

# Change the sign of coordinates flagged as outside the USA:
OutsideUSACoord_changed <- TADA_FlagCoordinates(
  mock_coords,
  clean_outsideUSA = "change sign"
)
#> When clean_outsideUSA == change sign, the sign for any lat/long coordinates flagged as outside of USA are switched. This is a temporary solution. Data owners should fix the raw data to address Suspect coordinates through WQX. For assistance fixing data errors you see in the WQP, email the WQX helpdesk (WQX@epa.gov).

# Remove data with imprecise coordinates, but keep coordinates outside the USA:
ImpreciseCoord_removed <- TADA_FlagCoordinates(
  mock_coords,
  clean_imprecise = TRUE
)

# Remove both imprecise coordinates and coordinates outside the USA:
SuspectCoord_removed <- TADA_FlagCoordinates(
  mock_coords,
  clean_outsideUSA = "remove",
  clean_imprecise = TRUE
)

# Example using location metadata comparison.
# StateCode and CountyCode are required when check_location_metadata = TRUE.
mock_coords_meta <- data.frame(
  ID = c(
    "Pass_US_mainland",
    "Lat_outside"
  ),
  StateCode = c("11", "06"),
  CountyCode = c("001", "001"),
  TADA.LatitudeMeasure = c(
    38.8977,
    -5.0000
  ),
  TADA.LongitudeMeasure = c(
    -77.0365,
    -120.0000
  ),
  stringsAsFactors = FALSE
)

# Flag coordinates not matching metadata:
SuspectCoord_meta <- TADA_FlagCoordinates(
  mock_coords_meta,
  check_location_metadata = TRUE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  69%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |======================================================                |  77%  |                                                                              |========================================================              |  80%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%
```
