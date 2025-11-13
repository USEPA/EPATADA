# Create ActivityStartDateTime column

Copy of internal dataRetrieval create_dateTime function 3/7/2025 See
USGS dataRetrieval importWQP.R file (line 223) offsetLibrary is a
dataframe saved in sysdata.rda You can see where and how it gets called
on line 160

## Usage

``` r
TADA_CreateDateTime(.data, date_col, time_col, tz_col, tz)
```

## Arguments

- .data:

  TADA dataframe

- date_col:

  date column

- time_col:

  time column

- tz_col:

  time zone column

- tz:

  time zone

## Examples

``` r
if (FALSE) { # \dontrun{
# Find web service URLs for each Profile using WQP User Interface
# (https://www.waterqualitydata.us/)
# Example WQP URL:
# https://www.waterqualitydata.us/#statecode=US%3A09&characteristicType=Nutrient&
# startDateLo=04-01-2023&startDateHi=11-01-2023&mimeType=csv&providers=NWIS&
# providers=STEWARDS&providers=STORET

# Define base URL and common components
baseurl <- "https://www.waterqualitydata.us"
filters <- "/search?statecode=US%3A09&characteristicType=Nutrient"
dates <- "&startDateLo=04-01-2023&startDateHi=11-01-2023"
type <- "&mimeType=csv&zip=yes"
providers <- "&providers=NWIS&providers=STEWARDS&providers=STORET"

# Construct URLs for different profiles
station_url <- paste0(
  baseurl, "/data/Station", filters, dates, type, providers
)
result_url <- paste0(
  baseurl, "/data/Result", filters, dates, type,
  "&dataProfile=resultPhysChem", providers
)
project_url <- paste0(
  baseurl, "/data/Project", filters, dates, type, providers
)

# Use TADA_ReadWQPWebServices to load Station, Project,
# and Phys-Chem Result profiles
stationProfile <- TADA_ReadWQPWebServices(station_url)
physchemProfile <- TADA_ReadWQPWebServices(result_url)
projectProfile <- TADA_ReadWQPWebServices(project_url)

# Join all three profiles using TADA_JoinWQPProfiles
TADAProfile <- TADA_JoinWQPProfiles(
  FullPhysChem = physchemProfile,
  Sites = stationProfile,
  Projects = projectProfile
)

# Run TADA_CheckRequiredFields, returns error message
TADA_CheckRequiredFields(TADAProfile)

# Add missing column
TADAProfile1 <- TADA_CreateDateTime(
  .data = TADAProfile,
  date_col = "ActivityStartDate",
  time_col = "ActivityStartTime.Time",
  tz_col = "ActivityStartTime.TimeZoneCode",
  tz = "UTC"
)

# Re-run TADA_CheckRequiredFields, no longer returns error message
TADA_CheckRequiredFields(TADAProfile1)
} # }
```
