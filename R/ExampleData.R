#' Data_Nutrients_UT
#'
#' A dataframe containing Ammonia, Nitrate, and Nitrogen data from Utah:
#'
#' Data_Nutrients_UT <- TADA_DataRetrieval(statecode = "UT",
#' characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
#' startDate = "2020-10-01",
#' endDate = "2022-09-30",
#' applyautoclean = TRUE)
#'
#' @docType data
#' @keywords dataframe
#' @name Data_Nutrients_UT
#' @usage data(Data_Nutrients_UT)
#' @format A dataframe with 14592 rows and 152 variables
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
#'                                       startDate = "2018-01-01"
#'                                       endDate = "2023-01-01")
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
#' @keywords dataframe
#' @name Data_6Tribes_5y
#' @usage data(Data_6Tribes_5y)
#' @format A dataframe with 134050 rows and 149 variables
NULL

#' Data_6Tribes_5y_Harmonized
#'
#' A dataframe containing data from tribal organizations
#' (see ?Data_6Tribes_5y for more information). Data was cleaned using the
#' TADA Module 1 vignette
#' (see: https://usepa.github.io/EPATADA/articles/TADAModule1.html).
#'
#' @docType data
#' @keywords dataframe
#' @name Data_6Tribes_5y_Harmonized
#' @usage data(Data_6Tribes_5y_Harmonized)
#' @format A dataframe with 117124 rows and 166 variables
NULL

#' Data_NCTCShepherdstown_HUC12
#'
#' Original pull:
#' Data_NCTCShepherdstown_HUC12 = TADA_DataRetrieval(
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
#' @keywords dataframe
#' @name Data_NCTCShepherdstown_HUC12
#' @usage data(Data_NCTCShepherdstown_HUC12)
#' @format A dataframe with 39236 rows and 151 variables
NULL

#' Data_R5_TADAPackageDemo
#'
#' Original pull:
#' Data_R5_TADAPackageDemo <- TADA_DataRetrieval(
#'  startDate = "2019-05-01",
#'  endDate = "2019-05-07",
#'  countycode = "null",
#'  huc = "null",
#'  siteid = "null",
#'  siteType = "null",
#'  characteristicName = "null",
#'  characteristicType = "null",
#'  sampleMedia = "null",
#'  statecode = c("IL", "IN", "MI", "MN", "OH", "WI"),
#'  organization = "null",
#'  project = "null",
#'  applyautoclean = FALSE
#' )
#'
#' @docType data
#' @keywords dataframe
#' @name Data_R5_TADAPackageDemo
#' @usage data(Data_R5_TADAPackageDemo)
#' @format A dataframe with 6569 rows and 123 variables
NULL

#' Data_HUC8_02070004_Mod1Output
#'
#' See Module 1 Workflow.R script in vignettes folder
#' This is the final data after running a recommended module 1 workflow.
#' It can be used as a starting point for Modules 2 or 3.
#'
#' @docType data
#' @keywords data frame
#' @name Data_HUC8_02070004_Mod1Output
#' @usage data(Data_HUC8_02070004_Mod1Output)
#' @format A data frame with 84 rows and 161 variables
NULL
