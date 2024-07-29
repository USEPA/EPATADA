#' Joins TADA Characteristic (Parameter) Names to ATTAINS_PARAMETER_AND_USE_NAME Reference Table
#'
#' This function joins an ATTAINS reference table by Parameter name through CAS number and 
#' other matches to the TADA_SimpleCriteriaRef data frame to pull in allowable designated 
#' use by parameter and Entity that is compatible with the ATTAINS system.
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

TADA_CreateAdditionalCriteriaRef <- function(.data, entity, priorityParam = FALSE) {
  
  
  parameterUseMap <- utils::read.csv(system.file("extdata", "ATTAINSParameterUseMapRef.csv", package = "EPATADA"))
  TADA_Simp
    
  .data <- .data %>%
    dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText) %>%
    dplyr::distinct() %>%
    dplyr::mutate(TADA.UserStandardValue = NA,
                  TADA.UserStandardUnit = NA,
                  TADA.UserDurationValue = NA,
                  TADA.UserDurationUnit = NA,
                  TADA.UserFrequencyValue = NA,
                  TADA.UserFrequencyUnit = NA) %>%
    dplyr::left_join(., parameterUseMap, by = c("TADA.CharacteristicName" = "parameter"), keep = TRUE) %>%
    dplyr::filter(organization_name == entity)
  
   if (priorityParam == TRUE) {
     .data <- filter(.data)
   } 
  
  return(.data)
  
}

#' Define Criteria Standards Metal Equations
#'
#' This function will ask users to specify the parameters needed for common metal 
#' 
#' # Should param(s) be added to easily export this as an Excel or csv file for users to edit?
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
#' # create criteria reference for Utah nutrients example data set
#' UT_AdditionalCriteriaRef <- TADA_CreateAdditionalCriteriaRef(Data_Nutrients_UT, entity = "Utah")
#' 

TADA_DefineMetalEquationCriteria <- function(.data, paramA, paramB, paramD) {
  
  return(.data)
  
}