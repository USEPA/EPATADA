#' Data_WaterT_US
#'
#' Example data containing all lake water temperature data in the US
#' from Jan 2000 to Jul 2022. This dataset can be reproduced 
#' with the following TADA_BigDataRetrieval call: 
#' 
#' Data_WaterT_US <- TADA_BigDataRetrieval(startDate = "2000-01-01", 
#' endDate = "2022-07-01", 
#' characteristicName = "Temperature, water",
#' siteType = "Lake, Reservoir, Impoundment")
#'
#' @docType data
#' @keywords datasets
#' @name Data_WaterT_US
#' @usage data(Data_WaterT_US)
#' @format A data frame with 14237 rows and 103 variables
NULL

#' Data_Nutrients_UT
#'
#' A dataset containing Ammonia, Nitrate, and Nitrogen data from Utah:
#'  
#' Data_Nutrients_UT <- TADA_DataRetrieval(statecode = "UT",
#' characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
#' startDate = "2020-10-01",
#' endDate = "2022-09-30")
#'
#' @docType data
#' @keywords datasets
#' @name Data_Nutrients_UT
#' @usage data(Data_Nutrients_UT)
#' @format A data frame with 14587 rows and 148 variables
NULL

#' Data_Narrow_5d
#'
#' Sample Results (narrow) WQP profile containing results for a 5 day period from 1/5/2023-1/10/2023.
#'  
#' @docType data
#' @keywords datasets
#' @name Data_Narrow_5d
#' @usage data(Data_Narrow_5d)
#' @format A data frame with 898 rows and 78 variables
NULL

#' Data_PhysChem_5d
#'
#' Sample Results (physical/chemical metadata) profile containing data for a 5 day period from 1/5/2023-1/10/2023.
#'  
#' @docType data
#' @keywords datasets
#' @name Data_PhysChem_5d
#' @usage data(Data_PhysChem_5d)
#' @format A data frame with 898 rows and 81 variables
NULL

#' Data_Site_5d
#'
#' Site Data Only WQP profile containing all data for a 5 day period from 1/5/2023-1/10/2023.
#'  
#' @docType data
#' @keywords datasets
#' @name Data_Site_5d
#' @usage data(Data_Site_5d)
#' @format A data frame with 55 rows and 37 variables
NULL

#' Data_TP_6Tribes_5y
#' 
#' A dataset containing Total Phosphorus (TP) data from tribal organizations
#' (see ?Data_6Tribes_5y for more information). Data was cleaned using the
#' TADA Module 1 vignette 
#' (see: https://usepa.github.io/TADA/articles/TADAModule1.html).
#' Data is filtered down to "Total Phosphorus, Mixed Forms".
#'  
#' @docType data
#' @keywords datasets
#' @name Data_TP_6Tribes_5y
#' @usage data(Data_TP_6Tribes_5y)
#' @format A data frame with 2498 rows and 172 variables
NULL

#' Data_6Tribes_5y
#'
#' Original pull:  
#' Data_6Tribes_5y <- TADA_DataRetrieval(organization = c("REDLAKE_WQX", 
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
#' @docType data
#' @keywords datasets
#' @name Data_6Tribes_5y
#' @usage data(Data_6Tribes_5y)
#' @format A data frame with 131906 rows and 151 variables
NULL

#' Data_NCTCShepherdstown_HUC12
#'
#' Original pull:  
#' Data_NCTCShepherdstown_HUC12 = TADA::TADA_DataRetrieval(
#'   startDate = "2020-03-14",
#'   endDate = "null",
#'   countycode = "null",
#'   huc = "02070004",
#'   siteid = "null",
#'   siteType = "null",
#'   characteristicName = "null",
#'   characteristicType = "null",
#'   sampleMedia = "null",
#'   statecode = "null",
#'   organization = "null",
#'   project = "null",
#'   applyautoclean = TRUE
#' )
#'  
#' @docType data
#' @keywords datasets
#' @name Data_NCTCShepherdstown_HUC12
#' @usage data(Data_NCTCShepherdstown_HUC12)
#' @format A data frame with 34330 rows and 151 variables
NULL
