#' Create Criteria Ref Table for User Inputs
#'
#' This function returns a .xlsx file named "myfile.xlsx". 
#' All unique combinations of TADA.CharacteristicName, TADA.MethodSpeciationName, and
#' TADA.ResultSampleFractionText will be generated and require users to input the 
#' useName (designated use associated with the parameter), the waterbody Type that this parameter
#' is associated with (if applicable) and whether this parameter and designated use is associated 
#' with a chronic or acute condition. Parameter Groups will also attempt to be filled out automatically 
#' (or should users be required to fill this in themselves?)
#' 
#' Users will have the option to remove or add rows they feel as needed. For example, if NITRATE_AS N_TOTAL
#' criteria standards are specific to a waterbody type (Lakes versus River), then users can add an additional
#' row for NITRATE_AS N_TOTAL to account for each of these conditions.
#' 
#' If there is a parameter that is site specific, then users can input "SiteSpecific" as the TADA.UserStandard value
#' and will have the option of running TADA_SiteSpecific function. Users will input an AU name and all
#' monitoringLocationName found within that AU will be generated as additional rows with all other column values 
#' being the same, with monitoringLocationName being the only difference. 
#' 
#' Users are required to input information on the Standards (Magnitude of concentration) that is not
#' to be exceeded. Users will have the option to run TADA_DefineCriteriaRef (or should this be required?)
#' if duration and frequency specifications are needed. Otherwise, if no duration or frequency values
#' are provided, then the end output when determining impairments will use each row in the TADA
#' dataframe as a discrete observation and use the total number of rows that contains a ResultMeasureValue
#' as the denominator when counting the number of exceedance. 
#' 
#'
#' @param .data TADA dataframe
#' 
#' @param entity a character string abbreviation of the entity's name
#' 
#' @param useNameRep a boolean value. If users would like to have each row to capture all combinations
#' of ATTAINS useName for each Parameter, they can indicate this with "TRUE" and are able to delete rows
#' if that parameter is not applicable to that designated useName.
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, ATTAINS.CharacteristicName, ATTAINS.UseName, ATTAINS.EntityName, 
#' TADA.UserStandardUnit, TADA.UserStandardValue
#' 
#' 
#' @export
#'
#' @examples
#' # create criteria reference for Utah nutrients example data set
#' UT_CriteriaRef <- TADA_CreateCriteriaRef(Data_Nutrients_UT, entity = "Utah")
#' UT_CriteriaRef_with_use <- TADA_CreateCriteriaRef(Data_Nutrients_UT, entity = "Utah", useNameRep = TRUE)
#' 

TADA_CreateCriteriaRef <- function(.data, entity, useNameRep = FALSE) {
  
  library(arsenal)
  library(httr)
  library(tidyverse)
  library(dplyr)
  library(jsonlite)
  
  char = dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")])
  
  # This ref table pulls in the allowable designated uses by Entity and Parameter. Will be used to join onto the TADA by TADA.CharacteristicName
  ATTAINSParameterUse <- utils::read.csv(system.file("extdata", "ATTAINSParameterUseMapRef.csv", package = "EPATADA"))
  
  # Filters ATTAINSPArameterUse to get use_name by entity
  AllowableUse <- ATTAINSParameterUse %>% 
    dplyr::filter(organization_name == entity) %>%
    dplyr::filter(parameter %in% as.list(unique(char$TADA.CharacteristicName))) %>%
    dplyr::select(use_name)
  
  # Remove intermediate char
  rm(char)
  
  # This creates an empty dataframe for user inputs on Criteria and Methodology of assessments
  columns <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "entity",	
    "ATTAINS.useName",	"ATTAINS.waterType", "acuteChronic", "parameterGroup", "TADA.UserStandardValue", "TADA.UserStandardUnit", "TADA.StandardLimit"	
    #,"TADA.UserDurationValue", "TADA.UserDurationUnit", "TADA.UserFrequencyValue", "TADA.UserFrequencyUnit", "TADA.Frequency_(m)",	
    #"minimumSampleSize",	"assessmentBegDate",	"assessmentEndDate", "Season",	"otherParamDependency"
  )
  
  param <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(param) = columns
  
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Index")
  protectWorksheet(wb, "Index", protect = TRUE)
  addWorksheet(wb, "UserCriteriaRef")
  writeData(wb, 1, x = dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]))
  writeData(wb, 1, startCol = 4, x = data.frame(entity = entity))
  writeData(wb, 1, startCol = 5, x = data.frame(ATTAINS.useName = AllowableUse))
  writeData(wb, 1, startCol = 6, x = data.frame(ATTAINS.waterType = unique(.data[, "MonitoringLocationTypeName"])))
  writeData(wb, 1, startCol = 7, x = data.frame(acuteChronic = c("Acute", "Chronic", "NA")))
  writeData(wb, 1, startCol = 8, x = data.frame(parameterGroup = c("Metals", "Nutrients", "Pathogens", "Dissolved Oxygen", "Other", "NA" )))
  writeData(wb, 1, startCol = 11, x = data.frame(parameterGroup = c("Upper", "Lower", "Range")))
  #writeData(wb, 2, x = ATTAINSParameterUse)
  
  param_data_frame <- dplyr::distinct(
    .data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]) %>% 
    mutate(entity = entity)
  
  writeData(wb, 2, x = param)
  writeData(wb, 2, startCol = 1, x = param_data_frame)
  
  n <- nrow(AllowableUse)
  if (useNameRep == TRUE){
    writeData(wb, 2, startCol = 1, 
              x = param_data_frame %>% 
                uncount(n) %>% 
                mutate(ATTAINS.useName = rep(AllowableUse$use_name, length.out = n()))
              )
  }
  
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 1, rows = 2:30, type = "list", value = sprintf("'Index'!$A$2:$A$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 2, rows = 2:30, type = "list", value = sprintf("'Index'!$B$2:$B$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 3, rows = 2:30, type = "list", value = sprintf("'Index'!$C$2:$C$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 4, rows = 2:30, type = "list", value = sprintf("'Index'!$D$2:$D$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 5, rows = 2:30, type = "list", value = sprintf("'Index'!$E$2:$E$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 6, rows = 2:30, type = "list", value = sprintf("'Index'!$F$2:$F$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 7, rows = 2:30, type = "list", value = sprintf("'Index'!$G$2:$G$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 8, rows = 2:30, type = "list", value = sprintf("'Index'!$H$2:$H$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  wb <- saveWorkbook(wb, "inst/extdata/myfile.xlsx", overwrite = T)
  
  rm(columns, param)
  
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
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
  
  return(CriteriaRef)
  
}

#' Create Additional Criteria Ref Table for User Inputs
#' 
#' User will need to have ran TADA_CreateCriteriaRef function first. Current function does replace 
#' the output, so considerations on how to save a user's progress will need to be considered during
#' each step.
#'
#' @param .data TADA dataframe
#' 
#' @param entity a character string abbreviation of the entity's name
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, ATTAINS.CharacteristicName, ATTAINS.UseName, ATTAINS.EntityName, 
#' TADA.UserStandardUnit, TADA.UserStandardValue, TADA.UserDurationValue, TADA.UserDurationUnit,. 
#' All values for
#' 
#' 
#' @export
#'
#' @examples
#' # create criteria reference for Utah nutrients example data set
#' UT_CriteriaRef <- TADA_CreateCriteriaRef(Data_Nutrients_UT, entity = "Utah")
#' UT_CriteriaRef2 <- TADA_CreateAdditionalCriteriaRef(Data_Nutrients_UT)
#' 

TADA_CreateAdditionalCriteriaRef <- function(.data) {
  # Run a check to see if TADA_CreateCriteriaRef has been ran or not
  
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  addCol <- c("minimumSampleSize", "assessmentBegDate", "assessmentEndDate", "Season", "otherParamDependency")
  CriteriaRef[, addCol] <- NA
  
  wb <- loadWorkbook(file = system.file("extdata", "myfile.xlsx", package = "EPATADA"))
  writeData(wb, 2, x = CriteriaRef)
  saveWorkbook(wb, "inst/extdata/myfile.xlsx", overwrite = T)
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")

  return(CriteriaRef)
}

#' Duration and Frequency Criteria Specification
#'
#' This function requires users to run TADA_CreateCriteriaRef and fill out the CriteriaRef file.
#' For each row depending on its parameter group, additional columns for duration and frequency
#' will be generated and filled out. 
#' 
#' If users do not include Duration and Frequency criteria in their ref file, then impairment decisions
#' will be based on each observation in the tada dataframe with no averaging period used and the denominator
#' will be for every observation that matches a row entry in the CriteriaRef as no frequeuncy or duration is defined.
#' 
#' 
#' Users will be required to validate these values for reasonability based on their state's water quality standards criteria. 
#' This function attempts to fill out default duration and frequency values for common parameters. However,
#' each state may deviate and have their own defined and approved criteria standards that they use.
#' 
#' Duration is defined as the averaging time period over which a parameter is measured. 
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
#' UT_CriteriaRef3 <- TADA_DefineCriteriaRef(.data = Data_Nutrients_UT_ATTAINS2)
#' 

TADA_DefineCriteriaRef <- function(.data, parameterGroup, duration, frequency) {
  # Users will be able to specify what duration and/or frequency should be applied for a parameterGroup 
  # (or should this be applied on a TADA.Characteristic level?). 
  
  
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check .data has all of the required columns
  expected_cols <- c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "entity",	
                    "ATTAINS.useName",	"ATTAINS.waterType", "acuteChronic", "parameterGroup", "TADA.UserStandardValue", "TADA.UserStandardUnit", "TADA.StandardLimit"		
                    # "TADA.UserDurationValue", "TADA.UserDurationUnit", "TADA.UserFrequencyValue", "TADA.UserFrequencyUnit", "TADA.Frequency_(m)",	
                    # "minimumSampleSize",	"assessmentBegDate", "assessmentEndDate", "Season",	"otherParamDependency"
  )
  
  TADA_CheckColumns(CriteriaRef, expected_cols)
  
  DefineCritera <- CriteriaRef %>%
    dplyr::mutate(TADA.UserDurationValue = case_when(parameterGroup == "Nutrients" & acuteChronic == "Acute" ~ "hour average",
                                                     parameterGroup == "Nutrients" & acuteChronic == "Chronic" ~ "day average"
                                                     
                                                     ))
  
  # temp <- TADA_CreatePairRef(.data, hardness = TRUE)
  # TADA_PairForCriteriaCalc(temp)
  
  # Attempts to define common duration periods for certain Parameter Groupings, Acute versus Chronic, and by EPA 304d federal standards.
  # Users will be warned to validate these duration periods and to take note these are not mandated standards, but recommended ones.
  
  
  return(DefineCritera)
  
}

#' Assessment Unit and MonitoringLocationName/MonitoringLocationType/MonitoringLocationId Crosswalk
#'
#' This function will 
#' 
#'
#' @param .data A TADA dataframe with TADA_GetATTAINS() geospatial function ran.
#' 
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_AUID_ref <- TADA_AUIDMonitoringLocation(Data_Nutrients_UT_ATTAINS2, AUID = "UT16020102-053_00")
#' 

TADA_AUIDMonitoringLocation <- function(.data, AUID){ 
  
  TADA_with_ATTAINS_subset <- .data %>%
    dplyr::filter(ATTAINS.assessmentunitidentifier == AUID) %>%
    dplyr::select(
      c(
        "MonitoringLocationTypeName","ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationName", "LongitudeMeasure", "LatitudeMeasure"
      )
    ) %>%
    dplyr::distinct(.keep_all = FALSE) %>%
    dplyr::mutate(IncludeorExcludeStation = "Include") %>%
    dplyr::mutate(ApplyUniqueCriteria = NA)
  
  return(TADA_with_ATTAINS_subset)
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
#' SiteSpecific_Ref <- TADA_SiteSpecificStandards(Data_Nutrients_UT_ATTAINS2, AUID = "UT16020102-053_00")
#' 

TADA_SiteSpecificStandards <- function(.data, CriteriaRef, AUID){ 

  # check for required columns
  req_cols <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode", "ATTAINS.assessmentunitidentifier",
    "MonitoringLocationIdentifier", "MonitoringLocationName"
  )
  
  # Must perform a check if user has filled out the CriteriaRef file
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
  # Filters the TADA_ATTAINS dataframe by the AUID or monitoringLocationName argument parameter in the function
  # and returns a dataframe of all the unique monitoringLocationName by that AUID.
  if (!is.null(AUID)){
    AUID_ref <- TADA_AUIDMonitoringLocation(.data, AUID = AUID)
  }
  
  if (is.null(AUID)){
    AUID_ref <- TADA_AUIDMonitoringLocation(.data)
  }
  
  AUID_CriteriaRef <- CriteriaRef %>%
    dplyr::mutate(ATTAINS.assessmentunitidentifier =
      case_when(
        TADA.UserStandardValue ==  "site specific" ~ AUID)
      ) %>%
    dplyr::mutate(TADA.UserStandardValue =
      case_when(
        TADA.UserStandardValue ==  "site specific" ~ NA)
      ) %>%
    #dplyr::mutate(., case_when(is.na(AUID), monitoringLocationName = NA)) %>%
    dplyr::left_join(AUID_ref, by = "ATTAINS.assessmentunitidentifier")
  
  
  return(AUID_CriteriaRef)
}

#' Returns Impairment Decisions for each Row in CriteriaRef
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

TADA_ImpairmentDecision <- function(.data, monitoringLocationName, AUID){ 
  
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
