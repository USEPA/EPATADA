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

TADA_CreateAdditionalCriteriaRef <- function(.data, entity, priorityParam = FALSE, useCategory = c("Aquatic Life", "Human Health", "")) {
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
  
  ## Use ATTAINS to pull in UseName and its context2 in Domains Value table. This can perhaps further help to define appropriate definitions for
  ## duration and freqeuncy.
  
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
  
   # This will allow users to filter parameters by only the top priority ones. This would assist in filling out
   # a list to contribute in defining all priority parameters to share methodology and criteria with other users.
   if (priorityParam == TRUE) {
     .data <- filter(.data)
   } 
  
  return(.data)
  
}

#' Return Duration Criteria Standards Summary
#'
#' This function will ask users to either submit a completed TADA.CreateAdditionalCriteriaRef 
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
#' # create criteria reference for Utah nutrients example data set
#' UT_AdditionalCriteriaRef <- TADA_CreateAdditionalCriteriaRef(Data_Nutrients_UT, entity = "Utah")
#' 

TADA_DefineMetalEquationCriteria <- function(.data, paramA, paramB, paramD) {
  
  return(.data)
  
}