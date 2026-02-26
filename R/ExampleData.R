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
#'                                       endDate = "2023-01-01",
#'                                       ask = FALSE)
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
#' @format A dataframe with 117124 rows and 166 variables
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
#' @format A data frame with 84 rows and 161 variables
NULL


#' Data_MT_MissoulaCounty
#'
#' This example data is used in the Module 2 and 3 vignettes.
#'
#' @docType data
#' @keywords data frame
#' @name Data_MT_MissoulaCounty
#' @format A data frame with 426 rows and 162 variables
NULL

#' Data_MT_AUMLRef
#'
#' An example assessment unit/monitoring location reference data frame for use
#' in testing TADA Module 2 and 3 workflows.
#'
#' @docType data
#' @keywords data frame
#' @name Data_MT_AUMLRef
#' @format A data frame with 56 rows and 6 variables.
NULL

#' Data_MT_AU_UsesRef
#'
#' An example assessment unit/use reference data frame for testing TADA Module 2 and 3 workflows.
#'
#' @docType data
#' @keywords data frame
#' @name Data_MT_AU_UsesRef
#' @format A data frame with 46 rows and 6 variables.
NULL

#' Data_MT_AU_UsesRef_Water
#'
#' Generate Data_MT_AU_UsesRef_Water used in ExampleMod2Workflow.Rmd and
#' ExampleMod3Workflow.Rmd
#'
#' @docType data
#' @name Data_MT_AU_UsesRef_Water
#' @format A data frame with 48 rows and 6 variables.
NULL
