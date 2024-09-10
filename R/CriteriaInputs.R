#' Joins TADA Characteristic (Parameter) Names to ATTAINS_PARAMETER_AND_USE_NAME Reference Table
#'
#' This function joins an ATTAINS reference table by Parameter name. Users will need
#' to define the Criteria (Numeric Standards, duration and frequency) for
#' their assessment methods by a parameter and designated use grouping that 
#' is compatible with the ATTAINS system.
#' 
#'
#' @param .data TADA dataframe
#' 
#' @param entity a character string abbreviation
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, ATTAINS.CharacteristicName, ATTAINS.UseName, ATTAINS.EntityName, 
#' TADA.UserStandardUnit, TADA.UserStandardValue, TADA.UserDurationValue, TADA.UserDurationUnit,. 
#' All values for
#' TADA.UserStandardUnit and TADA.UserStandardValue are NA and must be filled in by the user before
#' use in TADA_SimpleCriteriaComparison.
#' 
#' @export
#'
#' @examples
#' # create criteria reference for Utah nutrients example data set
#' UT_AdditionalCriteriaRef <- TADA_CreateAdditionalCriteriaRef(Data_Nutrients_UT, entity = "Utah")
#' 

TADA_CreateAdditionalCriteriaRef <- function(.data, entity, priorityParam = FALSE, useCategory = c("Aquatic Life", "Human Health", "Other")) {
  ## Aquatic Life Criteria should have both a Duration and Frequency component. That is, 
  #
  #       Duration - defined as the length of time a result was measured ("one hour average", "one hour maximum/minimum")
  #
  #       Frequency - defined as the number of times a Criteria Standard may be exceeded 
  #       (ex. "10% of the times", or "no more than once every three years")
  #
  ## Aquatic Life has both an "acute" and "chronic" component.
  
  ## Human Health Criteria "Water quality criteria for human health contain only a single expression of allowable magnitude; a
  ## criterion concentration generally to protect against long-term (chronic) human health effects. (WQS Handbook Chapter 3, EPA)
  #       
  #       For definition - Frequency would be defined as "lifetime" or "not to exceed ever"?  
  #       For duration - the numeric standard as a "one time maximum/minimum" or "upper/lower limit". Thoughts on this logic?
  #
  #       Human Health Criteria standards will likely depend on many circumstances in which users would need to "calculate". 
  #       How should we address factors for equation based standards for items such as "Fish Consumption", "Bioaccumulation formulas", 
  #       "Use of The Integrated Risk Information System (IRIS) (Barns and Dourson, 1988; Appendix N)", "Carcinogenics vs Non-Carcinogenics"?
  #       Can we assist users with these equation based criteria calculations? Do parameters change regularly for these equation inputs? 
  #
  ## Human Health can be split by "Consumption of organisms" or "Drinking water"
  
  # Designated Use - When a waterbody is designated for more than one use, then the criteria needs to be defined to protect its most sensitive use.
  ## FOUR major types of designated use categories: 1) RECREATION, 2) AQUATIC LIFE - (coldwater/warmwater/salt versus fresh), 
  ## 3) AGRICULTURAL AND INDUSTRIAL USES, and 4) PUBLIC WATER SUPPLIES.
  
  ## NEXT STEPS PLAN:
  ##
  ## Use ATTAINS to pull in UseName and its context2 in Domains Value table. This can perhaps further help to define appropriate definitions for
  ## duration and freqeuncy. Create a user ref table and perform similar 'harmonization' of duration and frequency
  ## IF(USE_NAME maps to HUMAN_HEALTH) -> duration = one time minimum or maximum and frequency = "lifetime/not to exceed ever" - and ask Users to validate
  ## Determine if the logic of mapping to human health is correct
  ##
  
  library(arsenal)
  library(httr)
  library(tidyverse)
  library(dplyr)
  library(jsonlite)
  
  # This ref table pulls in the allowable designated uses by Entity and Parameter. Will be used to join onto the TADA by TADA.CharacteristicName
  ATTAINSParameterUse <- utils::read.csv(system.file("extdata", "ATTAINSParameterUseMapRef.csv", package = "EPATADA"))
  
  # Filters ATTAINSPArameterUse to get use_name by entity
  AllowableUse <- ATTAINSParameterUse %>% 
    dplyr::filter(organization_name == entity) %>%
    # using sample dataset filter for the time being below - ZInc and Nitrogen
    dplyr::filter(parameter %in% c("ZINC", "NITROGEN")) %>%
    dplyr::select(parameter)
  
  # This creates an empty dataframe for user inputs on Criteria and Methodology of assessments
  columns <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText",
    "entity_name",	"use_name",	"waterType",	"Acute/Chronic",	"StandardsGroup",	"TADA.UserStandardValue",
    "TADA.UserStandardUnit",	"TADA.UserDurationValue",	"TADA.UserDurationUnit",	"TADA.UserFrequencyValue",
    "TADA.UserFrequencyUnit",	"TADA.Frequency_(m)",	"MinimumSampleSize",	"AssessmentBegDate",	"AssessmentEndDate",
    "Season",	"OtherParameters"
  )
  
  CriteriaRef <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(CriteriaRef) = columns
  
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Index")
  addWorksheet(wb, "ATTAINSParameterUse")
  addWorksheet(wb, "UserCriteriaRef")
  writeData(wb, 1, x = data.frame(TADA.CharacteristicName = c("Zinc","Nitrogen", "NA"), 
                                  TADA.MethodSpeciationName = c("NO4", "N", "NH4"),
                                  TADA.ResultSampleFractionText = c("Dissolved", "Total", "NA")
                                  # x = .data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]
  ))
  writeData(wb, 1, startCol = 4, x = data.frame(entityName = entity))
  writeData(wb, 1, startCol = 5, x = data.frame(use_name = AllowableUse))
  writeData(wb, 1, startCol = 6, x = data.frame(waterType = unique(.data[, "MonitoringLocationTypeName"])))
  writeData(wb, 1, startCol = 7, x = data.frame(AcuteChronic = c("Acute","Chronic")))
  writeData(wb, 1, startCol = 8, x = data.frame(StandardsGroup = c("Metals", "Nutrients", "Pathogens", "Dissolved Oxygen", "Other", "NA" )))
  writeData(wb, 2, x = ATTAINSParameterUse)
  
  writeData(wb, 3, x = CriteriaRef)
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 1, rows = 2:30, type = "list", value = sprintf("'Index'!$A$2:$A$10"), allowBlank = TRUE, showErrorMsg = FALSE, showInputMsg = FALSE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 2, rows = 2:30, type = "list", value = sprintf("'Index'!$B$2:$B$10"), allowBlank = TRUE, showErrorMsg = FALSE, showInputMsg = FALSE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 3, rows = 2:30, type = "list", value = sprintf("'Index'!$C$2:$C$10"), allowBlank = TRUE, showErrorMsg = FALSE, showInputMsg = FALSE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 4, rows = 2:30, type = "list", value = sprintf("'Index'!$D$2:$D$10"), allowBlank = TRUE, showErrorMsg = FALSE, showInputMsg = FALSE))
  CriteriaRef.xlsx <- wb
  openXL(wb)
  
  
  # ATTAINSUseType <- GET("https://attains.epa.gov/attains-public/api/domains?domainName=UseName") %>%
  #   content(as = "text", encoding = "UTF-8") %>%
  #   fromJSON(flatten = TRUE)
  # 
  # ATTAINSUseType<- ATTAINSUseType[,c("name","context2")]
  # 
  # .data <- .data %>%
  #   dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText) %>%
  #   dplyr::distinct() %>%
  #   dplyr::mutate(TADA.UserStandardValue = NA,
  #                 TADA.UserStandardUnit = NA,
  #                 TADA.UserDurationValue = NA,
  #                 TADA.UserDurationUnit = NA,
  #                 TADA.UserFrequencyValue = NA,
  #                 TADA.UserFrequencyUnit = NA) %>%
  #   dplyr::left_join(., parameterUseMap, by = c("TADA.CharacteristicName" = "parameter"), keep = TRUE) %>%
  #   dplyr::left_join(., ATTAINSUseType, by = c("use_name" = "name")) %>%
  #   dplyr::filter(organization_name == entity) %>%
  #   dplyr::distinct()
  # 
  #  # This will allow users to filter parameters by only the top priority ones. This would assist in filling out
  #  # a list to contribute in defining all priority parameters to share methodology and criteria with other users.
  #  if (priorityParam == TRUE) {
  #    .data <- subset(., parameter %in% utils::read.csv(system.file("extdata", "WQXcharValRef.csv", package = "EPATADA"))[parameters])
  #  } 
  
  return(wb)
  
}

#' Return Duration Criteria Standards Summary
#'
#' This function will ask users to submit a completed TADA.CreateAdditionalCriteriaRef 
#' dataframe. A summary statistics table will be created based on the duration associated
#' with a parameter and designated use row entry. Duration should be defined as the length of
#' time that a parameter was measured for for its standard criterion. Examples of Duration includes
#' a Duration Value and Duration Unit such as "1" for the Duration Value and "hour average" or 
#' "hour minimum" or "hour maximum". 
#' 
#' If a minimum or maximum is used for the duration, then this function will flag any results that exceeds the 
#' Standard Value and Unit threshold. 
#' 
#' If an average is used, then an average based on the defined Duration Value - such as "one hour"- will look at 
#' one hour averages 
#' 
#'
#' @param .data TADA dataframe
#' 
#' @param entity a character string abbreviation
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, TADA.UserStandardUnit, and TADA.UserStandardValue. All values for
#' TADA.UserStandardUnit and TADA.UserStandardValue are NA and must be filled in by the user before
#' use in TADA_SimpleCriteriaComparison.
#' 
#' @export
#'
#' @examples
#' 

TADA_DefineMetalEquationCriteria <- function(.data, .dataCriteriaRef, paramA, paramB, paramD) {
  
  return(.data)
  
}

#' Returns Criteria Standards template dataframe with Rows for each MonitoringLocationIdentifier/MonitoringLocationName
#'
#' This function will ask users to submit a completed TADA.CriteriaRef 
#' dataframe. For each parameter and designated use row entry that is filled with "site-specific"
#' for the standards column, separate rows for unique monitoringLocationName(s) will be generated
#' based on user defined AUID(s) or AU_name(s). 
#' 
#' Users are asked to fill in the TADA.UserStandardValue for each site-specfic defined parameter and designated use
#' standards. Any rows that are not defined as site specific in TADA.UserStandardValue will be kept as is and not changed.
#' 
#'
#' @param .data A TADA dataframe with TADA_GetATTAINS() geospatial function ran.
#' 
#' @param .dataCriteriaRef a dataframe generated from TADA_CreateRef(). 
#' 
#' @param stateCode a character string of the abbreviated state in which the TADA dataframe should be filtered by.
#'
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#' 
#' @export
#'
#' @examples
#' # example dataset with ATTAINS getspatial below
#  # ATTAINS_DATA_UT <- TADA_GetATTAINS(Data_Nutrients_UT)
#' load(ATTAINS_DATA_UT)
#' 

TADA_SiteSpecificStandards <- function(.data = ATTAINS_DATA_UT, .dataCriteriaRef, stateCode, monitoringLocationName, AUID){ #},AUID, MonitoringLocationName){
  
  library(arsenal)
  library(httr)
  library(tidyverse)
  library(dplyr)
  library(jsonlite)
  
  # check for required columns
  req_cols <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode", "ATTAINS.assessmentunitidentifier"
  )
  
  if(!is.character(stateCode)){
    stop('stateCode must be a character input. Please specify the abbreviation of the state name in which that your data is contained')
  }
  # ATTAINS web services - does this need to be pulled in? 
  temp <- GET(paste0("https://attains.epa.gov/attains-public/api/assessmentUnits?stateCode=",stateCode)) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE)
  
  # Filters the TADA_ATTAINS dataframe by the AUID or monitoringLocationName argument parameter in the function
  # and returns a dataframe of all the unique monitoringLocationName by that AUID.
  MonitoringRef <- dplyr::filter(.data$TADA_with_ATTAINS, ATTAINS.assessmentunitname == AUID) %>%
    select("MonitoringLocationName", "MonitoringLocationTypeName", "MonitoringLocationIdentifier", "ATTAINS.assessmentunitname")
  distinct()
  
  .dataCriteriaRef <- .dataCriteriaRef %>%
    dplyr::mutate(., case_when(TADA.UserStandardValue ==  "site specific", ATTAINS.assessmentunitname = AUID, ATTAINS.assessmentunitname = NA)) %>%
    #dplyr::mutate(., case_when(is.na(AUID), monitoringLocationName = NA)) %>%
    dplyr::left_join(., MonitoringRef, by = "ATTAINS.assessmentunitname")
  
  
  return(.data)
}
