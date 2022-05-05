#' Convert Units to WQX Target Units
#' 
#' *** placeholder text for function description
#'
#' @param .data TADA dataset
#' @param convert Boolean argument; changes ResultMeasure.MeasureUnitCode to WQX
#' target unit and converts ResultMeasureValue to corresponding target unit when
#' convert = TRUE. Default is convert = FALSE.
#' @param flag Boolean argument; appends WQX.ResultMeasureValue.UnitConversion and
#' WQX.DetectionLimitMeasureValue.UnitConversion columns, indicating if data can be
#' converted. "Convert" means data can be converted, "NoResultValue" means data
#' cannot be converted because there is no ResultMeasureValue, and "NoTargetUnit" 
#' means data cannot be converted because the original unit is not associated 
#' with a target unit in WQX. Default is flag = TRUE.
#'
#' @return 
#' 
#' @export

WQXTargetUnits <- function(.data, convert = FALSE, flag = TRUE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has all of the required columns
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode", 
           "DetectionQuantitationLimitMeasure.MeasureValue",
           "DetectionQuantitationLimitMeasure.MeasureUnitCode") %in% 
         colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # check if convert is boolean
  if(is.logical(convert) == FALSE){
    stop("convert argument must be Boolean (TRUE or FALSE)")
  }
  # check if flag is boolean
  if(is.logical(flag) == FALSE){
    stop("flag argument must be Boolean (TRUE or FALSE)")
  }
  # check that both convert and flag do NOT equal FALSE
  if(convert == FALSE & flag == FALSE){
    stop("Both 'convert' and 'flag' arguments equal FALSE, which would return
         the input dataset unchanged. One or both arguments must be equal to 
         TRUE.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode", 
           "DetectionQuantitationLimitMeasure.MeasureValue",
           "DetectionQuantitationLimitMeasure.MeasureUnitCode") %in% 
         colnames(.data)) == TRUE) {
    
    # if class(ResultMeasureValue) != numeric, run special char function
    if(class(.data$ResultMeasureValue) != "numeric") {
      .data <- MeasureValueSpecialCharacters(.data)
    }
    
    # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
    unit.ref <- TADA::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" & 
                      Status == "Valid")
    
    # define raw.data
    raw.data <- .data
    
    # join unit.ref to raw.data
    check.data <- merge(raw.data, unit.ref[, c("Characteristic", "Source",
                                               "Value", "Value.Unit",
                                               "Conversion.Factor")],
                        by.x = c("CharacteristicName", "ActivityMediaName", "ResultMeasure.MeasureUnitCode"),
                        by.y = c("Characteristic", "Source", "Value"), all.x = TRUE)
    # rename columns
    flag.data <- check.data %>%
      dplyr::rename(WQX.TargetUnit = Value.Unit) %>%
      dplyr::rename(ConversionFactor = Conversion.Factor)
    
      # if temp data exists, calculate conversion factor
      if (all(is.na(match(c("deg F", "deg K"), 
                        flag.data$ResultMeasure.MeasureUnitCode))) == FALSE) {
      
        # Calculate deg F and deg C, replace Conversion factor values
        flag.data <- flag.data %>%
          # apply function row by row
          dplyr::rowwise() %>%
          # create flag column
          dplyr::mutate(ConversionFactor = dplyr::case_when(
            ResultMeasure.MeasureUnitCode == "deg F" ~ 
              as.numeric(((ResultMeasureValue - 32)*(5/9))/ResultMeasureValue),
            ResultMeasure.MeasureUnitCode == "deg K" ~ 
              as.numeric((ResultMeasureValue - 273.15)/ResultMeasureValue),
            TRUE ~ ConversionFactor))
      }
    
    # add flag column when flag = TRUE
    if(flag == TRUE) {
    # add WQX.ResultMeasureValue.UnitConversion column
    flag.data <- flag.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(WQX.ResultMeasureValue.UnitConversion = dplyr::case_when(
        (!is.na(ResultMeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Convert"),
        is.na(ResultMeasureValue) ~ as.character("NoResultValue"),
        is.na(WQX.TargetUnit) ~ as.character("NoTargetUnit")))
    
    # add WQX.DetectionLimitMeasureValue.UnitConversion column
    flag.data <- flag.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(WQX.DetectionLimitMeasureValue.UnitConversion = dplyr::case_when(
        (!is.na(DetectionQuantitationLimitMeasure.MeasureValue) &
           !is.na(WQX.TargetUnit)) ~ as.character("Convert"),
        is.na(DetectionQuantitationLimitMeasure.MeasureValue) ~ as.character("NoDetectionLimitValue"),
        is.na(WQX.TargetUnit) ~ as.character("NoTargetUnit")))
    }
    
    if(convert == FALSE) {

      # reorder column names to match .data
        # get .data column names
      col.order <- colnames(.data)
        # add flag columns to the list
      col.order <- append(col.order, c("WQX.TargetUnit", 
                                       "ConversionFactor", 
                                       "WQX.ResultMeasureValue.UnitConversion",
                                       "WQX.DetectionLimitMeasureValue.UnitConversion"))
        # reorder columns in flag.data
      flag.data <- flag.data[, col.order]
        # place flag columns next to relevant fields
      flag.data <- flag.data %>%
        dplyr::relocate(c("WQX.TargetUnit", 
                        "ConversionFactor", 
                        "WQX.ResultMeasureValue.UnitConversion"), 
                        .after = "ResultMeasure.MeasureUnitCode") %>%
        dplyr::relocate("WQX.DetectionLimitMeasureValue.UnitConversion", 
                        .after = "DetectionQuantitationLimitMeasure.MeasureUnitCode")
      
      
      warning("Conversions required for range checks and TADATargetUnit conversions -- Unit conversions, data summaries, and data calculations may be affected.")
      return(flag.data)
    }
    
    if(convert == TRUE) {
      
      # Convert result measure value to Target Unit only if target unit exists
      clean.data <- flag.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # apply conversions where there is a target unit, use original value if no target unit
        dplyr::mutate(ResultMeasureValue = dplyr::case_when(
          !is.na(WQX.TargetUnit) ~ 
            (ResultMeasureValue * ConversionFactor),
          is.na(WQX.TargetUnit) ~ ResultMeasureValue))
      
      # populate ResultMeasure.MeasureUnitCode
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use target unit where there is a target unit, use original unit if no target unit
        dplyr::mutate(ResultMeasure.MeasureUnitCode = dplyr::case_when(
          !is.na(WQX.TargetUnit) ~ WQX.TargetUnit,
          is.na(WQX.TargetUnit) ~ ResultMeasure.MeasureUnitCode))
      
      # Convert detection limit measure value to Target Unit only if target unit exists
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # apply conversions where there is a target unit, use original value if no target unit
        dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureValue = dplyr::case_when(
          !is.na(WQX.TargetUnit) ~ 
            (DetectionQuantitationLimitMeasure.MeasureValue * ConversionFactor),
          is.na(WQX.TargetUnit) ~ DetectionQuantitationLimitMeasure.MeasureValue))
      
      # populate DetectionQuantitationLimitMeasure.MeasureUnitCode
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use target unit where there is a target unit, use original unit if no target unit
        dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureUnitCode = dplyr::case_when(
          !is.na(WQX.TargetUnit) ~ WQX.TargetUnit,
          is.na(WQX.TargetUnit) ~ DetectionQuantitationLimitMeasure.MeasureUnitCode))
      
      if(flag == TRUE) {
      # edit WQX.ResultMeasureValue.UnitConversion column
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # create flag column
        dplyr::mutate(WQX.ResultMeasureValue.UnitConversion = dplyr::case_when(
          (!is.na(ResultMeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Converted"),
          TRUE ~ WQX.ResultMeasureValue.UnitConversion)) 
        
        # edit WQX.DetectionLimitMeasureValue.UnitConversion column
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # create flag column
        dplyr::mutate(WQX.DetectionLimitMeasureValue.UnitConversion = dplyr::case_when(
          (!is.na(DetectionQuantitationLimitMeasure.MeasureValue) & !is.na(WQX.TargetUnit)) ~ as.character("Converted"),
          TRUE ~ WQX.DetectionLimitMeasureValue.UnitConversion))
      }
      
      # remove extraneous columns, fix field names
      clean.data <- clean.data %>%
      dplyr::select(-c("ConversionFactor", "WQX.TargetUnit"))
      
      # reorder column names to match .data
      # get .data column names
      col.order <- colnames(.data)
      # add ResultUnitConversion column to the list if flag = TRUE
      if(flag == TRUE){
        col.order <- append(col.order, c("WQX.ResultMeasureValue.UnitConversion",
                                        "WQX.DetectionLimitMeasureValue.UnitConversion"))
      }
      # reorder columns in clean.data
      clean.data <- clean.data[, col.order]
      # place flag columns next to relevant fields if flag = TRUE
      if(flag == TRUE){
        clean.data <- clean.data %>%
          dplyr::relocate("WQX.ResultMeasureValue.UnitConversion", 
                          .after = "ResultMeasure.MeasureUnitCode") %>%
          dplyr::relocate("WQX.DetectionLimitMeasureValue.UnitConversion", 
                          .after = "DetectionQuantitationLimitMeasure.MeasureUnitCode")
      }
      
      return(clean.data)
    }
  }
}


#' Transform Characteristic, Speciation, and Unit values to TADA Standards
#' 
#' *** placeholder text for function description
#'
#' @param .data TADA dataset
#' @param ref Optional argument to specify which dataframe to use as a reference
#' file. The primary use for this argument is when a user has generated a
#' harmonization reference file unique to their data, and they made changes to 
#' that file.
#' @param transform Boolean argument; 
#' @param flag Boolean argument; appends WQX.ResultMeasureValue.UnitConversion and
#' DetectionLimitMeasureValue.UnitConversion columns, indicating if data can be
#' converted. "Convert" means data can be converted, "NoResultValue" means data
#' cannot be converted because there is no ResultMeasureValue, and "NoTargetUnit" 
#' means data cannot be converted because the original unit is not associated 
#' with a target unit in WQX. Default is flag = FALSE.
#'
#' @return 
#' 
#' @export

HarmonizeData <- function(.data, ref, transform = FALSE, flag = TRUE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop(".data must be of class 'data.frame'")
  }
  # check ref is of class data.frame, if it exists
  if(!missing(ref)) {
    if(("data.frame" %in% class(ref)) == FALSE ) {
      stop("ref must be of class 'data.frame'")
    }
  } 
  # check .data has all of the required columns
  if(all(c("CharacteristicName", "ResultSampleFractionText",
           "MethodSpecificationName",
           "ResultMeasure.MeasureUnitCode") %in% 
         colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # check ref has all of the required columns
  if(!missing(ref)) {
    if((all(c("TADA.Suggested.CharacteristicName",
                 "TADA.Suggested.sample.fraction",
                 "TADA.Suggested.speciation",
                 "TADA.Suggested.result.unit") %in% 
               colnames(ref))) == FALSE) {
      stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
    }
  }
  # check if transform is boolean
  if(is.logical(transform) == FALSE){
    stop("transform argument must be Boolean (TRUE or FALSE)")
  }
  # check if flag is boolean
  if(is.logical(flag) == FALSE){
    stop("flag argument must be Boolean (TRUE or FALSE)")
  }
  # check that both convert and flag do NOT equal FALSE
  if(transform == FALSE & flag == FALSE){
    stop("Both 'transform' and 'flag' arguments equal FALSE, which would return
         the input dataset unchanged. One or both arguments must be equal to 
         TRUE.")
  }
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == TRUE) {
    
    # if class(ResultMeasureValue) != numeric, run special char function
    if(class(.data$ResultMeasureValue) != "numeric") {
      .data <- MeasureValueSpecialCharacters(.data)
    }
    
    # define harm.ref
      # if input for ref exists, use that data
    if(!missing(ref)) {
      harm.ref <- ref
    } 
      # if input for ref does not exist, use raw harmonization template
    if(missing(ref)) {
      harm.ref <- read.csv(system.file("extdata", "HarmonizationTemplate.csv",
                                       package = "TADA"))
        # remove extraneous characters in first column
      colnames(harm.ref)[1] <- gsub('^...','',colnames(harm.ref)[1])
    }
    
    # join harm.ref to .data
    flag.data <- merge(.data, harm.ref, 
                       by.x = c("CharacteristicName", "ResultSampleFractionText",
                                "MethodSpecificationName",
                                "ResultMeasure.MeasureUnitCode"),
                       by.y = c("CharacteristicName", "ResultSampleFractionText",
                                "MethodSpecificationName",
                                "ResultMeasure.MeasureUnitCode"), 
                       all.x = TRUE)
    
    # remove extraneous columns, fix field names
    flag.data <- flag.data %>%
      # remove ".x" suffix from column names
      dplyr::rename_at(dplyr::vars(dplyr::ends_with(".x")), 
                       ~stringr::str_replace(., "\\..$","")) %>%
      # remove columns with ".y" suffix
      dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    
    # reorder column names to match .data
      # get .data column names
    col.order <- colnames(.data)
      # add flag columns to the list
    col.order <- append(col.order, c("TADAPollutantGroup", 
                                     "CharacteristicNameUserSupplied", 
                                     "TADA.SuggestedCharacteristicName",
                                     "TADA.CharacteristicNameAssumptions",
                                     "TADA.SuggestedSampleFraction",
                                     "TADA.FractionAssumptions",
                                     "TADA.SuggestedSpeciation",
                                     "TADA.SpeciationAssumptions",
                                     "TADA.SpeciationConversionFactor",
                                     "TADA.SuggestedResultUnit",
                                     "TADA.UnitConversionFactor",
                                     "TADA.UnitConversionCoefficient",
                                     "CombinationValidity",
                                     "TADA.ComparableDataIdentifier",
                                     "TADA.TotalN_TotalP_CharacteristicNames_AfterSummation",
                                     "TADA.TotalN_TotalP_Summation_Identifier",
                                     "TADA.TotalN_TotalP_ComboLogic"))
      # reorder columns in flag.data
    flag.data <- flag.data[, col.order]
      # place flag columns next to relevant fields
    flag.data <- flag.data %>%
      dplyr::relocate("TADAPollutantGroup", 
                      .before = "CharacteristicName") %>%
      dplyr::relocate(c("CharacteristicNameUserSupplied", 
                        "TADA.SuggestedCharacteristicName",
                        "TADA.CharacteristicNameAssumptions"), 
                      .after = "CharacteristicName") %>%
      dplyr::relocate(c("TADA.SuggestedSampleFraction",
                        "TADA.FractionAssumptions"), 
                      .after = "ResultSampleFractionText") %>%
      dplyr::relocate(c("TADA.SuggestedSpeciation",
                        "TADA.SpeciationAssumptions",
                        "TADA.SpeciationConversionFactor"), 
                      .after = "MethodSpecificationName") %>%
      dplyr::relocate(c("TADA.SuggestedResultUnit",
                        "TADA.UnitConversionFactor",
                        "TADA.UnitConversionCoefficient"), 
                      .after = "ResultMeasure.MeasureUnitCode")
    
    # if transform = FALSE and flag = TRUE, return flag.data
    if((transform == FALSE) & (flag == TRUE)) {
      warning("Transformations required to use subsequent TADA tools.")
      return(flag.data)
    }
    
    # if transform = TRUE, transform data
    if(transform == TRUE){
      
      # CharacteristicName
        # replace CharacteristicName with TADA.SuggestedCharacteristicName
        clean.data <- flag.data %>%
          # apply function row by row
          dplyr::rowwise() %>%
          # use TADA suggested name where there is a suggested name, use original name if no suggested name
          dplyr::mutate(CharacteristicName = dplyr::case_when(
            !is.na(TADA.SuggestedCharacteristicName) ~ TADA.SuggestedCharacteristicName,
            is.na(TADA.SuggestedCharacteristicName) ~ CharacteristicName))
      
      # ResultSampleFractionText
        # replace ResultSampleFractionText with TADA.SuggestedSampleFraction
        clean.data <- clean.data %>%
          # apply function row by row
          dplyr::rowwise() %>%
          # use TADA suggested frac where there is a suggested frac, use original frac if no suggested frac
          dplyr::mutate(ResultSampleFractionText = dplyr::case_when(
            !is.na(TADA.SuggestedSampleFraction) ~ TADA.SuggestedSampleFraction,
            is.na(TADA.SuggestedSampleFraction) ~ ResultSampleFractionText))
      
        
      # ResultMeasure.MeasureUnitCode
        # replace ResultMeasure.MeasureUnitCode with TADA.SuggestedResultUnit
        clean.data <- clean.data %>%
          # apply function row by row
          dplyr::rowwise() %>%
          # use TADA suggested unit where there is a suggested unit, use original unit if no suggested unit
          dplyr::mutate(ResultMeasure.MeasureUnitCode = dplyr::case_when(
            !is.na(TADA.SuggestedResultUnit) ~ TADA.SuggestedResultUnit,
            is.na(TADA.SuggestedResultUnit) ~ ResultMeasure.MeasureUnitCode)) %>%
          # if conversion factor exists, multiply by ResultMeasureValue
          dplyr::rowwise() %>%
          dplyr::mutate(ResultMeasureValue = dplyr::case_when(
            !is.na(TADA.UnitConversionFactor) ~ 
              (TADA.UnitConversionFactor*ResultMeasureValue),
            is.na(TADA.UnitConversionFactor) ~ ResultMeasureValue))
        
      # MethodSpecificationName
        # replace MethodSpecificationName with TADA.SuggestedSpeciation
        clean.data <- clean.data %>%
          # apply function row by row
          dplyr::rowwise() %>%
          # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
          dplyr::mutate(MethodSpecificationName = dplyr::case_when(
            !is.na(TADA.SuggestedSpeciation) ~ TADA.SuggestedSpeciation,
            is.na(TADA.SuggestedSpeciation) ~ MethodSpecificationName)) %>%
          # if conversion factor exists, multiply by ResultMeasureValue
          dplyr::rowwise() %>%
          dplyr::mutate(ResultMeasureValue = dplyr::case_when(
            !is.na(TADA.SpeciationConversionFactor) ~ 
              (TADA.SpeciationConversionFactor*ResultMeasureValue),
            is.na(TADA.SpeciationConversionFactor) ~ ResultMeasureValue))
      
      # remove conversion columns
      clean.data <- clean.data %>%
        dplyr::select(-c("TADA.SuggestedCharacteristicName", 
                         "TADA.SuggestedSampleFraction",
                         "TADA.SuggestedSpeciation",
                         "TADA.SpeciationConversionFactor",
                         "TADA.SuggestedResultUnit",
                         "TADA.UnitConversionFactor"))
      
      # if flag = TRUE, return clean.data
      if(flag == TRUE) {
        return(clean.data)
      }
      
      # remove all appended columns if flag = FALSE
      if(flag == FALSE) {
        # remove all appended columns
        clean.data <- clean.data %>%
          dplyr::select(-c("TADAPollutantGroup",
                           "CharacteristicNameUserSupplied",
                           "CombinationValidity",
                           "TADA.CharacteristicNameAssumptions",
                           "TADA.FractionAssumptions",
                           "TADA.SpeciationAssumptions",
                           "TADA.UnitConversionCoefficient",
                           "TADA.ComparableDataIdentifier",
                           "TADA.TotalN_TotalP_CharacteristicNames_AfterSummation",
                           "TADA.TotalN_TotalP_Summation_Identifier",
                           "TADA.TotalN_TotalP_ComboLogic"))
        
        # return clean.data
        return(clean.data)
      }
    }
  }
}
    
    
    