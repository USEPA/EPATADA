#' Convert Units to WQX Target Units
#' 
#' *** placeholder text for function description
#'
#' @param .data TADA dataset
#' @param convert Boolean argument; changes ResultMeasure.MeasureUnitCode to WQX
#' target unit and converts ResultMeasureValue to corresponding target unit when
#' convert = TRUE. Default is convert = FALSE.
#' @param flag Boolean argument; appends ResultMeasureValue.UnitConversion and
#' DetectionLimitMeasureValue.UnitConversion columns, indicating if data can be
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
  # check .data has any of the required columns
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
    
    # if values aren't upper case
    # duplicate and capitalize CharName, ActivityMediaName, and ResultMeasureUnitCode columns in .data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    raw.data$Media.Upper <- toupper(raw.data$ActivityMediaName)
    raw.data$Unit.Upper <- toupper(raw.data$ResultMeasure.MeasureUnitCode)
    
    # join unit.ref to raw.data
    check.data <- merge(raw.data, unit.ref[, c("Characteristic", "Source",
                                               "Value", "Value.Unit",
                                               "Conversion.Factor")],
                        by.x = c("CharacteristicName", "ActivityMediaName", "ResultMeasure.MeasureUnitCode"),
                        by.y = c("Characteristic", "Source", "Value"), all.x = TRUE)
    # rename columns
    check.data <- check.data %>%
      dplyr::rename(TargetUnit = Value.Unit) %>%
      dplyr::rename(ConversionFactor = Conversion.Factor)
    
      # if temp data exists, calculate conversion factor
      if (all(is.na(match(c("deg F", "deg K"), 
                        check.data$ResultMeasure.MeasureUnitCode))) == FALSE) {
      
        # Calculate deg F and deg C, replace Conversion factor values
        check.data <- check.data %>%
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
    # add ResultMeasureValue.UnitConversion column
    check.data <- check.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(ResultMeasureValue.UnitConversion = dplyr::case_when(
        (!is.na(ResultMeasureValue) & !is.na(TargetUnit)) ~ as.character("Convert"),
        is.na(ResultMeasureValue) ~ as.character("NoResultValue"),
        is.na(TargetUnit) ~ as.character("NoTargetUnit")))
    
    # add DetectionLimitMeasureValue.UnitConversion column
    check.data <- check.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(DetectionLimitMeasureValue.UnitConversion = dplyr::case_when(
        (!is.na(DetectionQuantitationLimitMeasure.MeasureValue) &
           !is.na(TargetUnit)) ~ as.character("Convert"),
        is.na(DetectionQuantitationLimitMeasure.MeasureValue) ~ as.character("NoDetectionLimitValue"),
        is.na(TargetUnit) ~ as.character("NoTargetUnit")))
    }
    
    # remove extraneous columns
    flag.data <- check.data %>%
      dplyr::select(-c("Char.Upper", "Media.Upper", "Unit.Upper"))
    
    if(convert == FALSE) {

      # reorder column names to match .data
      # get .data column names
      col.order <- colnames(.data)
      # add flag columns to the list
      col.order <- append(col.order, c("TargetUnit", 
                                       "ConversionFactor", 
                                       "ResultMeasureValue.UnitConversion",
                                       "DetectionLimitMeasureValue.UnitConversion"))
      # reorder columns in flag.data
      flag.data <- flag.data[, col.order]
      
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
          !is.na(TargetUnit) ~ 
            (ResultMeasureValue * ConversionFactor),
          is.na(TargetUnit) ~ ResultMeasureValue))
      
      # populate ResultMeasure.MeasureUnitCode
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use target unit where there is a target unit, use original unit if no target unit
        dplyr::mutate(ResultMeasure.MeasureUnitCode = dplyr::case_when(
          !is.na(TargetUnit) ~ TargetUnit,
          is.na(TargetUnit) ~ ResultMeasure.MeasureUnitCode))
      # capitalize ResultMeasure.MeasureUnitCode column for consistency 
      clean.data$ResultMeasure.MeasureUnitCode <- toupper(clean.data$ResultMeasure.MeasureUnitCode)
      
      # Convert detection limit measure value to Target Unit only if target unit exists
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # apply conversions where there is a target unit, use original value if no target unit
        dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureValue = dplyr::case_when(
          !is.na(TargetUnit) ~ 
            (DetectionQuantitationLimitMeasure.MeasureValue * ConversionFactor),
          is.na(TargetUnit) ~ DetectionQuantitationLimitMeasure.MeasureValue))
      
      # populate DetectionQuantitationLimitMeasure.MeasureUnitCode
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use target unit where there is a target unit, use original unit if no target unit
        dplyr::mutate(DetectionQuantitationLimitMeasure.MeasureUnitCode = dplyr::case_when(
          !is.na(TargetUnit) ~ TargetUnit,
          is.na(TargetUnit) ~ DetectionQuantitationLimitMeasure.MeasureUnitCode))
      # capitalize DetectionQuantitationLimitMeasure.MeasureUnitCode column for consistency 
      clean.data$DetectionQuantitationLimitMeasure.MeasureUnitCode <- 
        toupper(clean.data$DetectionQuantitationLimitMeasure.MeasureUnitCode)
      
      if(flag == TRUE) {
      # edit ResultMeasureValue.UnitConversion column
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # create flag column
        dplyr::mutate(ResultMeasureValue.UnitConversion = dplyr::case_when(
          (!is.na(ResultMeasureValue) & !is.na(TargetUnit)) ~ as.character("Converted"),
          TRUE ~ ResultMeasureValue.UnitConversion)) 
        
        # edit DetectionLimitMeasureValue.UnitConversion column
      clean.data <- clean.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # create flag column
        dplyr::mutate(DetectionLimitMeasureValue.UnitConversion = dplyr::case_when(
          (!is.na(DetectionQuantitationLimitMeasure.MeasureValue) & !is.na(TargetUnit)) ~ as.character("Converted"),
          TRUE ~ DetectionLimitMeasureValue.UnitConversion))
      }
      
      # remove extraneous columns, fix field names
      clean.data <- clean.data %>%
      dplyr::select(-c("ConversionFactor", "TargetUnit"))
      
      # reorder column names to match .data
      # get .data column names
      col.order <- colnames(.data)
      if(flag == TRUE){
      # add ResultUnitConversion column to the list
      col.order <- append(col.order, c("ResultMeasureValue.UnitConversion",
                                        "DetectionLimitMeasureValue.UnitConversion"))
      }
      # reorder columns in clean.data
      clean.data <- clean.data[, col.order]
      
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
#' @param clean Boolean argument; changes ResultMeasure.MeasureUnitCode to WQX
#' target unit and converts ResultMeasureValue to corresponding target unit when
#' convert = TRUE. Default is convert = FALSE.
#' @param flag Boolean argument; appends ResultMeasureValue.UnitConversion and
#' DetectionLimitMeasureValue.UnitConversion columns, indicating if data can be
#' converted. "Convert" means data can be converted, "NoResultValue" means data
#' cannot be converted because there is no ResultMeasureValue, and "NoTargetUnit" 
#' means data cannot be converted because the original unit is not associated 
#' with a target unit in WQX. Default is flag = FALSE.
#'
#' @return 
#' 
#' @export

HarmonizeData <- function(.data, ref, clean = FALSE, flag = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has any of the required columns
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  # check if clean is boolean
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
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == TRUE) {
    
    
  }
}
