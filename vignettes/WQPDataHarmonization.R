## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- results = 'hide', message = FALSE, warning = FALSE----------------------
install.packages("remotes", 
                 repos = "http://cran.us.r-project.org")
library(remotes)

## ---- results = 'hide', message = FALSE, warning = FALSE----------------------
# remotes::install_github("USGS-R/dataRetrieval", dependencies=TRUE)

# if you experience any issues installing TADA, try un-commenting and running
# the line below before the install
# options(download.file.method = "wininet")

remotes::install_github("USEPA/TADA", 
                        ref = "develop",
                        dependencies = TRUE)
library(TADA)


## -----------------------------------------------------------------------------
# Here is an example for Red Lake Band of Chippewa Indians, Minnesota
dataRetrieval_example <- dataRetrieval::readWQPdata(organization = "REDLAKE_WQX", ignore_attributes = TRUE)

## -----------------------------------------------------------------------------
# We will move forward with this example in the remainder of the vignette. 
TADAProfile <- TADAdataRetrieval(organization = "REDLAKE_WQX")

## -----------------------------------------------------------------------------
# WARNING, this can take multiple HOURS to run 
# AK_AL_WaterTemp <- TADABigdataRetrieval(startDate = "2000-01-01", 
#                                     endDate = "2022-12-31", 
#                                     characteristicName = "Temperature, water",
#                                     statecode = c("AK","AL"))
# 
# AllWaterTemp <- TADABigdataRetrieval(characteristicName = "Temperature, water")
#
# AllPhosphorus <- TADABigdataRetrieval(characteristicName = "Phosphorus")
# 
# AllCT <- TADABigdataRetrieval(statecode = "CT")


## -----------------------------------------------------------------------------
colnames(TADAProfile)

TADAProfile_CharSummary <- TADA_summarizeColumn(TADAProfile,
                                                "TADA.CharacteristicName")
TADAProfile_CharSummary

# Remove data for non-water media types
TADAProfile <- dplyr::filter(TADAProfile, 
                             TADA.ActivityMediaName == "WATER")


## -----------------------------------------------------------------------------
#create a map of the world
maps::map()

# draw the sites included in your TADAProfile onto the map
points(TADAProfile$TADA.LongitudeMeasure, 
       TADAProfile$TADA.LatitudeMeasure, 
       col="red", 
       pch=20)

## -----------------------------------------------------------------------------
# flag only
InvalidCoordinateFlags <- InvalidCoordinates(TADAProfile, 
                                             clean_outsideUSA = "no",
                                             clean_imprecise = FALSE, 
                                             errorsonly = TRUE)

# review unique flags in InvalidCoordinateFlags
unique(InvalidCoordinateFlags$TADA.InvalidCoordinates.Flag)

# review unique MonitoringLocationIdentifiers in your flag dataframe
unique(InvalidCoordinateFlags$MonitoringLocationIdentifier)

Unique_InvalidCoordinateFlags <- InvalidCoordinateFlags %>%
  dplyr::select('MonitoringLocationIdentifier',
                'MonitoringLocationName',
                'TADA.InvalidCoordinates.Flag',
                'OrganizationIdentifier',
                'TADA.LongitudeMeasure',
                'TADA.LatitudeMeasure',
                'MonitoringLocationTypeName',
                'CountryCode',
                'StateCode',
                'CountyCode',
                'HUCEightDigitCode',
                'MonitoringLocationDescriptionText',
                'ProjectName',
                'ProjectIdentifier',
                'OrganizationFormalName') %>%
  dplyr::distinct()

# change the sign for all data for sites flagged as outside the USA. 
TADAProfileClean1 <- InvalidCoordinates(TADAProfile,
                                        clean_outsideUSA = "change sign",
                                        clean_imprecise = FALSE,
                                        errorsonly = FALSE)

## -----------------------------------------------------------------------------
#redraw the map
maps::map()

# draw the sites included in your TADAProfile onto the map
points(TADAProfileClean1$TADA.LongitudeMeasure, 
       TADAProfileClean1$TADA.LatitudeMeasure, 
       col="red", 
       pch=20)

## -----------------------------------------------------------------------------
#converts all depth profile data to meters
TADAProfileClean2 <- ConvertDepthUnits(TADAProfileClean1,
                                       unit = "m",
                                       transform = TRUE)

## -----------------------------------------------------------------------------
#Converts all results to WQX target units
TADAProfileClean3 <- ConvertResultUnits(TADAProfileClean2,
                                        transform = TRUE)

## -----------------------------------------------------------------------------
TADAProfileClean4 <- AggregatedContinuousData(TADAProfileClean3,
                                              clean = FALSE)

# uncomment below to create a dataframe of only the aggregated continuous data
# TADAProfile_aggcont <- AggregatedContinuousData(TADAProfileClean3, clean = FALSE, errorsonly = TRUE)

## -----------------------------------------------------------------------------
TADAProfileClean5 <- InvalidMethod(TADAProfileClean4,
                                   clean = TRUE)
TADAProfileClean6 <- InvalidFraction(TADAProfileClean5,
                                     clean = TRUE)
TADAProfileClean7 <- InvalidSpeciation(TADAProfileClean6,
                                       clean = "invalid_only")
TADAProfileClean8 <- InvalidResultUnit(TADAProfileClean7,
                                       clean = "invalid_only")

## -----------------------------------------------------------------------------
TADAProfileClean9 <- AboveNationalWQXUpperThreshold(TADAProfileClean8,
                                                    clean = TRUE)
TADAProfileClean10 <- BelowNationalWQXLowerThreshold(TADAProfileClean9,
                                                     clean = TRUE)

## -----------------------------------------------------------------------------
TADAProfileClean11 <- PotentialDuplicateRowID(TADAProfileClean10,
                                              clean = TRUE,
                                              errorsonly = FALSE)

## -----------------------------------------------------------------------------
TADAProfileClean12 <- QAPPapproved(TADAProfileClean11,
                                   clean = FALSE, 
                                   cleanNA = FALSE)

## -----------------------------------------------------------------------------
TADAProfileClean13 <- QAPPDocAvailable(TADAProfileClean12,
                                       clean = FALSE)

## -----------------------------------------------------------------------------
# multiple options

# print table to console
FilterFields(TADAProfileClean13)

# create object of table
FilterFields_Table = FilterFields(TADAProfileClean13)

# view table
FilterFields_Table


## ---- fig.width=6, fig.height=2, fig.fullwidth=TRUE---------------------------
# print table in console, and generate pie chart
FilterFieldReview("ActivityTypeCode", TADAProfileClean13)

# write table to environment
FilterFieldReview_Table <- FilterFieldReview("ActivityTypeCode",
                                             TADAProfileClean13)

# view table
FilterFieldReview_Table


