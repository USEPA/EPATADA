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
#' @name WaterTemp_US
#' @usage data(WaterTemp_US)
#' @format A data frame with 14237 rows and 103 variables
NULL

#' Utah Nutrient Data
#'
#' A dataset containing Ammonia, Nitrate, and Nitrogen data from Utah:
#'  
#' TADAProfile <- TADAdataRetrieval(statecode = "UT",
#' characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
#' startDate = "2020-10-01",
#' endDate = "2022-09-30")
#'
#' @docType data
#' @keywords datasets
#' @name Nutrients_Utah
#' @usage data(Nutrients_Utah)
#' @format A data frame with 14587 rows and 137 variables
NULL

#' narrow
#'
#' narrow dataset containing all WQP data for a 5 day period from 1/5/2023-1/10/2023
#'  
#' @docType data
#' @keywords datasets
#' @name narrow
#' @usage data(narrow)
#' @format A data frame with 898 rows and 78 variables
NULL

#' resultphyschem
#'
#' resultphyschem dataset containing all WQP data for a 5 day period from 1/5/2023-1/10/2023
#'  
#' @docType data
#' @keywords datasets
#' @name resultphyschem
#' @usage data(resultphyschem)
#' @format A data frame with 898 rows and 81 variables
NULL

#' station
#'
#' station dataset containing all WQP data for a 5 day period from 1/5/2023-1/10/2023
#'  
#' @docType data
#' @keywords datasets
#' @name station
#' @usage data(station)
#' @format A data frame with 55 rows and 37 variables
NULL

#' TADAProfileClean18_TNonly
#' 
#' A dataset containing Nutrient data from Utah:
#'
#' original pull below:  
#' TADAProfile <- TADAdataRetrieval(statecode = "UT",
#' characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
#' startDate = "2020-10-01",
#' endDate = "2022-09-30")
#'
#' Data was cleaned using the TADA harmonize vignette. Data is filtered down to 
#' only Total Nitrogen.
#'  
#' @docType data
#' @keywords datasets
#' @name TADAProfileClean18_TNonly
#' @usage data(TADAProfileClean18_TNonly)
#' @format A data frame with 5198 rows and 169 variables
NULL

#' Tribal Data
#'
#' Original pull:  
#' TADAProfile <- TADAdataRetrieval(organization = c("REDLAKE_WQX", 
#'                                                  "SFNOES_WQX", 
#'                                                  "PUEBLO_POJOAQUE",
#'                                                  "FONDULAC_WQX",
#'                                                  "PUEBLOOFTESUQUE",
#'                                                  "CNENVSER"),
#'                                 startDate = "2018-01-01")
#'                                 
#' This example includes monitoring data collected from Jan 2018 to present
#' by six organizations: 
#' 1) Red Lake Band of Chippewa Indians,
#' 2) Sac & Fox Nation, 
#' 3) Pueblo of Pojoaque,
#' 4) Minnesota Chippewa Tribe (Fond du Lac Band),
#' 5) Pueblo of Tesuque, and
#' 6) The Chickasaw Nation
#'
#'  
#' @docType data
#' @keywords datasets
#' @name TribalData
#' @usage data(TribalData)
#' @format A data frame with 131558 rows and 137 variables
NULL

