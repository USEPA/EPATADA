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

#' Data_TribalNations
#'
#' This example includes monitoring data collected from (no start or end dates):
#' 1) "REDLAKE_WQX", # Red Lake Band of Chippewa Indians
#' 2) "SFNOES_WQX", # Sac & Fox Nation
#' 3) "FONDULAC_WQX", # Minnesota Chippewa Tribe (Fond du Lac Band)
#' 4) "PUEBLOOFTESUQUE", # Pueblo of Tesuque
#' 5) "UTEMTN", # R8, Ute Mountain Ute Tribe (Colorado)
#' 6) "BLCKFEET" # R8, Blackfeet Nation (Montana)
#'
#' @docType data
#' @keywords dataframe
#' @name Data_TribalNations
#' @format A dataframe with 142991 rows and 152 variables
NULL

#' Data_TribalNations_Harmonized
#'
#' A dataframe containing data from tribal organizations
#' (see ?Data_TribalNations for more information). Data was cleaned using the
#' TADA Module 1 vignette
#' (see: https://usepa.github.io/EPATADA/articles/TADAModule1.html).
#'
#' @docType data
#' @keywords dataframe
#' @name Data_TribalNations_Harmonized
#' @format A dataframe with 75264 rows and 171 columns
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

#' wqx3_fullPhysChem
#'
#' Generate wqx3_fullPhysChem used in
#' WQX3-Migration.Rmd
#'
#' @docType data
#' @name wqx3_fullPhysChem
#' @format A data frame with 44 rows and 193 variables.
NULL

#' Data_Participatory_Scientists
#'
#' Generate Data_Participatory_Scientists used in
#' Participatory-Science-Water-Projects-in-WQX.Rmd
#'
#' @docType data
#' @name Data_Participatory_Scientists
#' @format A data frame with 41723 rows and 150 variables.
NULL

#' Data_Penobscot
#'
#' Generate Data_Penobscot used in
#' PenobscotNationWorkflow.Rmd
#'
#' @docType data
#' @name Data_Penobscot
#' @format A data frame with 41723 rows and 150 variables.
NULL
