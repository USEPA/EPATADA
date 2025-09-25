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


#' Data_MT_MissoulaCounty
#'
#' See Module 2 and 3 workflows for examples of this data set can be used in the
#' TADA analysis workflow.
#'
#' @docType data
#' @keywords data frame
#' @name Data_MT_MissoulaCounty
#' @usage data(Data_MT_MissoulaCounty)
#' @format A data frame with 426 rows and 161 variables
#' @details with minimal data cleaning functions applied to the original WQP query.
#'
#' Data_MT_MissoulaCounty <- TADA_DataRetrieval(
#' startDate = "2020-01-01",
#' endDate = "2022-12-31",
#' statecode = "MT",
#' characteristicName = c(
#' "Escherichia",
#' "Escherichia coli",
#' "pH"
#' ),
#' county = "Missoula County",
#' ask = FALSE) %>%
#' TADA_RunKeyFlagFunctions() %>%
#' TADA_SimpleCensoredMethods() %>%
#' TADA_HarmonizeSynonyms()
NULL

#' Data_MT_AUMLRef
#'
#'An example assessment unit/monitoring location reference data frame for use
#'in testing TADA Module 2 and 3 workflows.
#'
#' @docType data
#' @keywords data frame
#' @name Data_MT_AUMLRef
#' @format A data frame with 56 rows and 6 variables.
#' @usage data(Data_MT_AUMLRef)
#'
#' # create test reference data frame
#' attains.existing.MT <- TADA_GetATTAINSAUMLCrosswalk(org_id = "MTDEQ")
#'
#' # clean existing crosswalk from ATTAINS to make sure WQP monitoring location IDs pulled from ATTAINS are WQP compatible (adds org ID if missing)
#' clean.existing.attains.MT <- TADA_UpdateATTAINSAUMLCrosswalk(org_id = "MTDEQ")
#'
#' # create example user supplied crosswalk (select a few Monitoring Locations from the tada df to use in the example for demonstration purposes)
#' user.supplied.cw <- clean.existing.attains.MT %>%
#'   dplyr::select(
#'     ATTAINS.AssessmentUnitIdentifier,
#'     ATTAINS.MonitoringLocationIdentifier,
#'     ATTAINS.WaterType
#'   ) %>%
#'   dplyr::filter(ATTAINS.MonitoringLocationIdentifier %in% c(
#'     "MDEQ_WQ_WQX-C04CKFKR05", "MDEQ_WQ_WQX-C04KNDYC01", "MDEQ_WQ_WQX-C04KNDYC02",
#'     "MDEQ_WQ_WQX-C04KNDYC04", "MDEQ_WQ_WQX-C04KNDYC54"
#'   )) %>%
#'   dplyr::rename(
#'     AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier,
#'     MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier
#'   ) %>%
#'   # Add an example new assessment unit for demonstration purposes.
#'   dplyr::bind_rows(c(
#'     AssessmentUnitIdentifier = "NEW:EX_MDEQ_WQ_WQX",
#'     MonitoringLocationIdentifier = "NARS_WQX-NWC_MT-10184",
#'     ATTAINS.WaterType = "LAKE, FRESHWATER"
#'   ))
#'
#' MT.AUMLRef <- TADA_CreateAUMLCrosswalk(Data_MT_MissoulaCounty,
#'                                        au_ref = user.supplied.cw,
#'                                        org_id = "MTDEQ",
#'                                        add_catch = FALSE,
#'                                        batch_upload = TRUE
#' )
#'
#' Data_MT_AUMLRef <- MT.AUMLRef$ATTAINS_batchupload %>%
#'   TADA_UpdateATTAINSAUMLCrosswalk( # selected attains_replace = TRUE because all matches currently in ATTAINS are included in this new crosswalk
#'     attains_replace = TRUE,
#'     batch_upload = FALSE,
#'     wqp_data_links = "add",
#'    # ml ids have already  been corrected if needed
#'     update_mlid = FALSE,
#'     org_id = "MTDEQ"
#'   ) %>%
#'   dplyr::mutate(
#'     ATTAINS.WaterType = dplyr::case_when(
#'       ATTAINS.AssessmentUnitIdentifier == "NEW:EX_MDEQ_WQ_WQX" ~ "LAKE, FRESHWATER",
#'      TRUE ~ ATTAINS.WaterType
#'    )
#'   )
#'
#' rm(attains.existing.MT, clean.existing.attains.MT, user.supplied.cw, MT.AUMLRef)
NULL

#' Data_MT_UseAURef
#'
#' An example assessment unit/use reference data frame for testing TADA Module 2 and 3 workflows.
#'
#' @docType data
#' @keywords data frame
#' @name Data_MT_UseAURef
#' @format A data frame with 46 rows and 6 variables.
#' @usage data(Data_MT_UseAURef)
#'
#' Data_MT_UseAURef <- TADA_CreateUseAURef(AUMLRef = Data_MT_AUMLRef, org_id = "MTDEQ")
NULL
