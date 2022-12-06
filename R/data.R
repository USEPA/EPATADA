#' Lake Water Temperature
#'
#' TADA example dataset containing all lake water temperature data in the U.S.
#' from Jan. 2000 to July 2022. This dataset can be reproduced 
#' with the following TADABigdataRetrieval call: 
#' 
#' AllWaterTempData <- TADABigdataRetrieval(startDate = "2000-01-01", 
#' endDate = "2022-07-01", 
#' characteristicName = "Temperature, water",
#' siteType = "Lake, Reservoir, Impoundment")
#'
#' @docType data
#' @keywords datasets
#' @name TADAProfile_AllUS_LakeWaterT_Jan2000toJuly2022
#' @usage data(TADAProfile_AllUS_LakeWaterT_Jan2000toJuly2022)
#' @format A data frame with 14237 rows and 103 variables
NULL

#' Utah Nutrient Data
#'
#' A dataset containing Ammonia, Nitrate, and Nitrogen data from Utah:
#'  
#' TADAProfile <- TADAdataRetrieval(statecode = "UT",
#' characteristicName = c("Ammonia", "Nitrate", "Nitrogen"), 
#' startDate = "10-01-2020")
#'
#' @docType data
#' @keywords datasets
#' @name TADAProfile_UT_Nutrients
#' @usage data(TADAProfile_UT_Nutrients)
#' @format A data frame with 6608 rows and 110 variables
NULL