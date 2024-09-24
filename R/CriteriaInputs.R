#TADA_CreateAUIDRef <- function() # Will pull in initial AUID and MonitoringLocationName crosswalk after GetATTAINS is ran. User input for current cycle is needed.
#TADA_CreateParamUseRef <- function() # Will pull in ATTAINS parameters, its allowable ATTAINS Use, and ATTAINS Group Name as an index - comes from RATTAINS package. Users will be able to modify if needed to define their own list of allowable use by parameter name.

#TADA_UserStandardsRef(AUIDRef = TRUE, ParamUseRef = TRUE) # Will reference TADA_CreateAUIDRef and TADA_GetParamUseRef. If these refs are not provided, runs these ref functions and use these as the ref for allowable values Index.
#TADA_AdditionalUserStandardsRef(AUIDRef = TRUE, ParamUseRef = TRUE) # Optional to provide additional filtering context (highly recommended if users are looking to use this tool for their WQS criteria and assessment)

#TADA_CreateDurationFreqRef <- function()
  
#TADA_UserCriteriaRef() # Aggregates all Criteria into a single reference file that is compatible with an entity's water quality standards.
#TADA_ImpairmentDecision()

#' Assessment Unit and MonitoringLocationName/MonitoringLocationType/MonitoringLocationId Crosswalk
#'
#' This function will pull in all MonitoringLocationName/MonitoringLocationType/MonitoringLocationId 
#' for AUID(s) from a TADA dataframe with ATTAINS data. This function requires users to have already 
#' ran TADA_GetATTAINS(). Users are able to specify which AUID(S) to pull in for this ref file as an
#' argument value.
#' 
#' Users are expected to modify this AUID ref file with the appropriate AUID and 
#' MonitoringLocationName/MonitoringLocationType/MonitoringLocationId crosswalk
#' for the current Assessment cycle.
#' 
#' Users can decide to "Include or Exclude" a MonitoringLocation within an AU if desired. This can be used
#' if a MoniotringLocation would still like to be crosswalked to the AU but may only be applicable for certain
#' parameters.
#' 
#'
#' @param .data A TADA dataframe with TADA_GetATTAINS() geospatial function ran.
#' 
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_AUID_ref <- TADA_CreateAUIDRef(Data_Nutrients_UT_ATTAINS$TADA_with_ATTAINS, AUID = "UT16020102-053_00")
#' Data_Nutrients_AUID_ref2 <- TADA_CreateAUIDRef(Data_Nutrients_UT_ATTAINS$TADA_with_ATTAINS)
#' 

TADA_CreateAUIDRef <- function(.data, AUID = NULL){ 
  
  if(is.null(AUID)){
    warning("No AUID(s) were selected for filtering. Creating a dataframe for all AUID and 
            MonitoringLocationName/MonitoringLocationType/MonitoringLocationId.")
  }
  
  expected_cols <- c(
    "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier"
  )
  
  # Make this error message compatible with running TADA_GetATTAINS()
  TADA_CheckColumns(.data, expected_cols)
  
  # Runs slow if pulling in all AUID, should consider trying to run a faster option if possible.
  TADA_with_ATTAINS_subset <- .data %>%
    dplyr::filter(if (is.null(AUID)) TRUE 
      else ATTAINS.assessmentunitidentifier == AUID
      ) %>%
    dplyr::select(
      c(
        "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "LongitudeMeasure", "LatitudeMeasure"
      )
    ) %>%
    dplyr::distinct(.keep_all = FALSE) %>%
    dplyr::mutate(IncludeorExcludeStation = "Include") %>%
    # dplyr::mutate(ApplyUniqueCriteria = NA)
  
  return(TADA_with_ATTAINS_subset)
}

#' Parameter and designated use crosswalk by Entity
#' 
#' Users should define a list of allowable designated use values for each Parameter
#' that is being assessed. This function will use a reference file created from the rATTAINS
#' package of previous allowable values from 
#'
#' @param .data A TADA dataframe with TADA_GetATTAINS() geospatial function ran.
#' 
#' @return A data frame with all the MonitoringLocationIdentifier Sites for a defined AU.
#' 
#' @export
#'
#' @examples
#' Data_Nutrients_UT_ATTAINS <- load("data.Rda")
#' Data_Nutrients_ParamUse_ref <- TADA_CreateParamUseRef(Data_Nutrients_UT, entity = "Utah")
#' 

TADA_CreateParamUseRef <- function(.data = NULL, entity = NULL){ 
  
  # check for required columns if a .data is provided. Not a needed argument, but if users want this ref file
  # to be compatible and filtered by parameters found within their dataframe, they can provide their TADA dataframe.
  req_cols <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText"
  )
  
  # This ref table pulls in the allowable designated uses by Entity and Parameter. Will be used to join onto the TADA by TADA.CharacteristicName
  ATTAINSParameterUse <- utils::read.csv(system.file("extdata", "ATTAINSParameterUseMapRef.csv", package = "EPATADA"))
  # WQXCharacteristicRef
  
  # Filters ATTAINSPArameterUse to get use_name by entity
  ref <- ATTAINSParameterUse %>% 
    # dplyr::filter(parameter %in% as.list(unique(char$TADA.CharacteristicName))) %>%
    dplyr::select(organization_name, parameter, use_name) %>%
    dplyr::arrange(parameter)
   
  if(!is.null(entity)) {
    ref <- ref %>% dplyr::filter(organization_name == entity)
  }
  
  if(!is.null(.data)){
    TADA_CheckColumns(.data, req_cols)
    
    char = dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")])
    
    ref <- ref %>% dplyr::filter(parameter %in% as.list(unique(char$TADA.CharacteristicName)))
  }
  
  return(ref)
}

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
#' 
#' Data_Nutrients_UT <- TADA_FindNearbySites(Data_Nutrients_UT)
#' UT_CriteriaRef <- TADA_CreateStandardsRef(Data_Nutrients_UT)
#' UT_CriteriaRef <- TADA_CreateStandardsRef(Data_Nutrients_UT, ParamUseRef = Data_Nutrients_ParamUse_ref, AUIDRef = Data_Nutrients_AUID_ref, useNameRep = TRUE)
#' UT_CriteriaRef2 <- TADA_CreateStandardsRef(Data_Nutrients_UT, ParamUseRef = Data_Nutrients_ParamUse_ref, AUIDRef = Data_Nutrients_AUID_ref2, useNameRep = TRUE)
#' UT_CriteriaRef_with_use <- TADA_CreateStandardsRef(Data_Nutrients_UT, useNameRep = TRUE, ParamUseRef = Data_Nutrients_ParamUse_ref)
#' 

TADA_CreateStandardsRef <- function(.data, default = FALSE, ParamUseRef = NULL, AUIDRef = NULL, useNameRep = FALSE) {
  
  # check to see if user-supplied ref is a df
  # ParamUseRef table pulls in the allowable designated uses by Entity and Parameter. Will be used to join onto the TADA by TADA.CharacteristicName
  # Users can choose to submit their own ParamUseRef file with edits, or to use the default pairings created from TADA_CreateParamUseRef()
  if (!is.null(ParamUseRef) & !is.character(ParamUseRef)) {
    if (!is.data.frame(ParamUseRef)) {
      stop("TADA_CreateStandardsRef: 'ParamUseRef' must be a data frame with three columns: organization_name, parameter, use_name")
    }

    if (is.data.frame(ParamUseRef)) {
      col.names <- c(
        "organization_name", "parameter", "use_name"
      )

      ref.names <- names(ParamUseRef)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'ParamUseRef' must be a data frame with three columns: organization_name, parameter, use_name")
      }
    }
  }
  
  if (!is.character(AUIDRef)) {
    if (!is.data.frame(AUIDRef)) {
      stop("TADA_CreateStandardsRef: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
    }
    
    if (is.data.frame(AUIDRef)) {
      col.names <- c(
        "ATTAINS.assessmentunitname","ATTAINS.assessmentunitidentifier",
        "MonitoringLocationIdentifier", "MonitoringLocationName", "MonitoringLocationTypeName", "LongitudeMeasure", "LatitudeMeasure"
      )
      
      ref.names <- names(AUIDRef)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'AUIDRef' must be a data frame with seven columns: ATTAINS.assessmentunitname, ATTAINS.assessmentunitidentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, LongitudeMeasure, LatitudeMeasure")
      }
    }
  }
  
  char = dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")])
  
  # If AUIDref is provided, then all unique combinations of AUID and MonitoringLocations 
  
  # Pulls in StateCode to get entity's name
  stateCode <- utils::read.csv(system.file("extdata", "statecode.csv", package = "EPATADA"))
  
  # Returns the name of the entity based on the stateCo
  entity_name <- filter(stateCode, STATE == unique(Data_Nutrients_UT$StateCode))[[2]]
  
  # If user does not supply their own ParamUseRef file, then the function will run TADA_CreateParamUseRef() to pull in this ref with no user edits.
  if(is.null(ParamUseRef)){
    warning("ParamUseRef was not provided. Allowable values may not contain all possible values in ATTAINS.UseName. It is recommended 
            for users to have run TADA_CreateParamUseRef() and add, remove or review allowable values for a parameter and
            designated use prior to running this function.")
    ParamUseRef <- TADA_CreateParamUseRef(.data, entity_name)
  }
  
  # Filters ATTAINSPArameterUse to get use_name by entity
  AllowableUse <- ParamUseRef %>% 
    dplyr::filter(organization_name == entity_name) %>%
    dplyr::filter(parameter %in% as.list(unique(char$TADA.CharacteristicName))) %>%
    dplyr::select(use_name)
  
  # Remove intermediate char
  rm(char)
  
  # This creates an empty dataframe for user inputs on Criteria and Methodology of assessments
  columns <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "entity", 
    "ATTAINS.assessmentunitname", "ATTAINS.UseName",	"ATTAINS.WaterType", "ATTAINS.CharacteristicGroup",
    "TADA.UserAcuteChronic", "TADA.UserStandardValue", "TADA.UserStandardUnit", "TADA.StandardLimit"	
  )
  
  param <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(param) = columns
  
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Index")
  protectWorksheet(wb, "Index", protect = TRUE)
  addWorksheet(wb, "UserCriteriaRef")
  header_st <- createStyle(textDecoration = "Bold")
  writeData(wb, 2, startCol = 1, x = param, headerStyle = header_st)
  openxlsx::setColWidths(wb, 2, cols = 1:ncol(param), widths = "auto")
  
  writeData(wb, 1, x = dplyr::distinct(.data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]))
  writeData(wb, 1, startCol = 4, x = data.frame(entity = entity_name))
  writeData(wb, 1, startCol = 5, x = data.frame(ATTAINS.assessmentunitname = unique(AUIDRef$ATTAINS.assessmentunitname)))
  writeData(wb, 1, startCol = 6, x = data.frame(ATTAINS.UseName = AllowableUse))
  writeData(wb, 1, startCol = 7, x = data.frame(ATTAINS.WaterType = unique(.data[, "MonitoringLocationTypeName"])))
  writeData(wb, 1, startCol = 8, x = data.frame(ATTAINS.CharacteristicGroup = c("Metals", "Nutrients", "Microbiological", "Dissolved Oxygen", "Other", "NA" )))
  writeData(wb, 1, startCol = 9, x = data.frame(TADA.UserAcuteChronic = c("Acute", "Chronic", "NA")))
  writeData(wb, 1, startCol = 12, x = data.frame(parameterGroup = c("Upper", "Lower", "Range")))
  #writeData(wb, 2, x = ATTAINSParameterUse)
  
  # param_data_frame <- dplyr::distinct(
  #   +     Data_Nutrients_UT[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]) %>% 
  #   +     mutate(entity = "utah") %>%
  #   +     uncount(nrow(ParamUseRef)) %>%
  #   +     mutate(ATTAINS.assessmentunitname = rep(Data_Nutrients_AUID_ref$ATTAINS.assessmentunitname, length.out = n()))
  # > View(param_data_frame)
  
  param_data_frame <- dplyr::distinct(
    .data[,c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText")]) %>% 
    mutate(entity = entity_name) %>%
    uncount(length(unique(AUIDRef$ATTAINS.assessmentunitname))) %>%
    mutate(ATTAINS.assessmentunitname = rep(unique(AUIDRef$ATTAINS.assessmentunitname), length.out = n()))
  
  AUID_data_frame <- dplyr::distinct(
    AUIDRef[,"ATTAINS.assessmentunitname"]) %>% 
    mutate(entity = entity_name)
  
  writeData(wb, 2, x = param, headerStyle = header_st)
  writeData(wb, 2, startCol = 1, x = param_data_frame, headerStyle = header_st)
  
  n <- nrow(AllowableUse)
  if (useNameRep == TRUE){
    writeData(
      wb, 2, startCol = 1, 
        x = 
        param_data_frame %>% 
        uncount(n) %>% 
        mutate(ATTAINS.UseName = rep(AllowableUse$use_name, length.out = n())),
      headerStyle = header_st
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
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 9, rows = 2:30, type = "list", value = sprintf("'Index'!$I$2:$I$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  suppressWarnings(dataValidation(wb, sheet = "UserCriteriaRef", cols = 11, rows = 2:30, type = "list", value = sprintf("'Index'!$K$2:$K$100"), allowBlank = TRUE, showErrorMsg = TRUE, showInputMsg = TRUE))
  wb <- saveWorkbook(wb, "inst/extdata/myfile.xlsx", overwrite = T)
  # wb <- saveWorkbook(wb, "downloads/myfile.xlsx", overwrite = T)
  
  rm(columns, param)
  
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
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
#' UT_CriteriaRef <- TADA_CreateStandardRef(Data_Nutrients_UT, entity = "Utah")
#' UT_CriteriaRef2 <- TADA_CreateAdditionalStandardsRef(Data_Nutrients_UT)
#' 

TADA_CreateAdditionalStandardsRef <- function(.data, ref = "null") {
  
  # Run a check to see if TADA_CreateCriteriaRef has been ran or not
  # check to see if user-supplied ref is a df
  if (!is.character(ref)) {
    if (!is.data.frame(ref)) {
      stop("TADA_CreateAdditionalStandardsRef: 'ref' must be a data frame with eleven columns: TADA.CharacteristicName,
         TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
         TADA.PairingGroup.Rank, and TADA.PairingGroup.")
    }
    
    if (is.data.frame(ref)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode",
        "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText",
        "TADA.PairingGroup.Rank", "TADA.PairingGroup"
      )
      
      ref.names <- names(ref)
      
      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_CreateStandardsRef: 'ref' must be a data frame with six columns: TADA.CharacteristicName,
         TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
         TADA.PairingGroup.Rank, and TADA.PairingGroup.")
      }
    }
  }
  
  
  # Additional CriteriaRef definitions should be defined. 
  # Allowable values for these columns will be needed to run TADA functionalities down the line.
  # Examples on why each column represents should be shown as well.
  # TADA.MinimumSampleSize = numeric, how many observations are needed. Will this be based on samples per duration period (ex. daily average) or 
  #     for each discrete observation? 
  # TADA.A
  
  
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  addCol <- c(
    "TADA.MinimumSampleSize", "TADA.AssessmentBegDate", "TADA.AssessmentEndDate", 
    "TADA.Season", "TADA.otherParamDependency", "TADA.MultipleStandards"
    )
  
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
#' UT_CriteriaRef3 <- TADA_DefineCriteriaRef()
#' 

TADA_DefineCriteriaRef <- function(ref, autofill = TRUE) {
  # Users will be able to specify what duration and/or frequency should be applied for a parameterGroup 
  # (or should this be applied on a TADA.Characteristic level?). 
  
  
  CriteriaRef <- openxlsx::read.xlsx(system.file("extdata", "myfile.xlsx", package = "EPATADA"), sheet = "UserCriteriaRef")
  
  # check .data has all of the required columns
  expected_cols <- c("TADA.CharacteristicName", "TADA.MethodSpeciationName",	"TADA.ResultSampleFractionText", "entity", 
                     "ATTAINS.UseName",	"ATTAINS.WaterType", "ATTAINS.CharacteristicGroup",
                     "TADA.UserAcuteChronic", "TADA.UserStandardValue", "TADA.UserStandardUnit", "TADA.StandardLimit"
  )
  
  # TADA_CheckColumns(CriteriaRef, expected_cols)
  
  col_Criteria <- c("TADA.UserDurationValue", "TADA.UserDurationPeriod", "TADA.UserFrequencyValue", "TADA.UserFrequency")
  
  if(autofill == TRUE){
    CriteriaRef[, col_Criteria] <-NA
    DefineCritera <- CriteriaRef %>%
    # Attempts to capture common duration and frequency values based on Parameter Group definition.
    ## Should we consider this on a parameter level instead? Does Parameter Group encompass too many different parameters
    ## that don't follow the same criteria?
    dplyr::mutate(
      TADA.UserDurationValue = case_when(
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Acute" ~ 1,
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Chronic" ~ 4,
        ATTAINS.CharacteristicGroup == "Pathogens" & TADA.UserAcuteChronic == "Acute" ~ 1, #E. coli, Enterococci
        ATTAINS.CharacteristicGroup == "Nutrients" ~ 1
        ) 
      )%>%
    dplyr::mutate(
      TADA.UserDurationPeriod = case_when(
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Acute" ~ "hour average",
        ATTAINS.CharacteristicGroup == "Metals" & TADA.UserAcuteChronic == "Chronic" ~ "day average",
        ATTAINS.CharacteristicGroup == "Pathogens" & TADA.UserAcuteChronic == "Acute" ~ "hour geometric mean", #E. coli, Enterococci
        ATTAINS.CharacteristicGroup == "Nutrients" ~ "monthly mean"
                                                       
      ))
  }
  
  if(autofill == FALSE){
    CriteriaRef[, col_Criteria] <-NA
  }
  # temp <- TADA_CreatePairRef(.data, hardness = TRUE)
  # TADA_PairForCriteriaCalc(temp)
  
  # Attempts to define common duration periods for certain Parameter Groupings, Acute versus Chronic, and by EPA 304d federal standards.
  # Users will be warned to validate these duration periods and to take note these are not mandated standards, but recommended ones.
  
  
  return(DefineCritera)
  
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
