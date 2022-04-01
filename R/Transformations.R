#' Convert Units to WQX Target Units
#' 
#' *** placeholder text for function description
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes data that is above the upper WQX
#' threshold from the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return When clean = FALSE, a column flagging rows with data that are above
#' the upper WQX threshold is appended to the input data set. When clean = TRUE,
#' data that is above the upper WQX threshold is removed from the dataset.
#' 
#' @export

WQXTargetUnits <- function(.data, convert = FALSE){
  
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
  
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ActivityMediaName", "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode") %in% colnames(.data)) == TRUE) {
    
    # filter WQXcharVal.ref to include only valid CharacteristicUnit in water media
    unit.ref <- TADA::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicUnit" & Source == "WATER" & 
                      Status == "Valid")
    
    # define raw.data
    raw.data <- .data
    # duplicate and capitalize CharName, ActivityMediaName, and ResultMeasureUnitCode columns in .data
    raw.data$Char.Upper <- toupper(raw.data$CharacteristicName)
    raw.data$Media.Upper <- toupper(raw.data$ActivityMediaName)
    raw.data$Unit.Upper <- toupper(raw.data$ResultMeasure.MeasureUnitCode)
    
    # join unit.ref to raw.data
    check.data <- merge(raw.data, unit.ref[, c("Characteristic", "Source",
                                               "Value", "Value.Unit",
                                               "Conversion.Factor")],
                        by.x = c("Char.Upper", "Media.Upper", "Unit.Upper"),
                        by.y = c("Characteristic", "Source", "Value"), all.x = TRUE)
    # rename columns
    check.data <- check.data %>%
      dplyr::rename(TargetUnit = Value.Unit) %>%
      dplyr::rename(ConversionFactor = Conversion.Factor)
    
      # if temp data exists, calculate conversion factor
      if (all(is.na(match(c("deg F", "deg K"), 
                        check.data$ResultMeasure.MeasureUnitCode))) == FALSE) {
      
        # create numeric result value column (for calculations)
        check.data$ResultMeasureValue.Numeric <- suppressWarnings(
          as.numeric(check.data$ResultMeasureValue))
      
        # Calculate deg F and deg C, replace Conversion factor values
        check.data <- check.data %>%
          # apply function row by row
          dplyr::rowwise() %>%
          # create flag column
          dplyr::mutate(ConversionFactor = dplyr::case_when(
            ResultMeasure.MeasureUnitCode == "deg F" ~ 
              as.numeric(((ResultMeasureValue.Numeric - 32)*(5/9))/ResultMeasureValue.Numeric),
            ResultMeasure.MeasureUnitCode == "deg K" ~ 
              as.numeric((ResultMeasureValue.Numeric - 273.15)/ResultMeasureValue.Numeric),
            TRUE ~ ConversionFactor))
      
        # remove numeric result column
        check.data <- dplyr::select(check.data, -ResultMeasureValue.Numeric)
      }
    
    # add ResultUnitConversion column
    flag.data <- check.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(ResultUnitConversion = dplyr::case_when(
        !is.na(TargetUnit) ~ as.character("OK"),
        (is.na(TargetUnit) & is.na(ResultMeasureValue)) ~ as.character("NoResultValue"),
        is.na(TargetUnit) ~ as.character("NoTargetUnit")))
    
    # remove extraneous columns
    flag.data <- flag.data %>%
      dplyr::select(-c("Char.Upper", "Media.Upper", "Unit.Upper"))
    
    if(convert == FALSE) {

      # reorder column names to match .data
      # get .data column names
      col.order <- colnames(.data)
      # add flag columns to the list
      col.order <- append(col.order, c("TargetUnit", 
                                       "ConversionFactor", "ResultUnitConversion"))
      # reorder columns in flag.data
      flag.data <- flag.data[, col.order]
      
      warning("Conversion required for range checks and TADATargetUnit conversions.
      Unit conversions and data summaries and calculations may be affected")
      return(flag.data)
    }
    
    if(convert == TRUE) {
      
      # copy ResultMeasureValue to ResultMeasureValue.Original
      flag.data$ResultMeasureValue.Original <- flag.data$ResultMeasureValue
      
      # convert original result measure value to numeric, if it's not already
      if(class(flag.data$ResultMeasureValue.Original) != "numeric") {
        # convert result measure value class to numeric
        flag.data$ResultMeasureValue.Original <- suppressWarnings(
          as.numeric(flag.data$ResultMeasureValue.Original))
      }
      
      # Convert result measure value to Target Unit only if target unit exists
      flag.data <- flag.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # apply conversions where there is a target unit, use original value if no target unit
        dplyr::mutate(ResultMeasureValue = dplyr::case_when(
          !is.na(TargetUnit) ~ 
            (ResultMeasureValue.Original * ConversionFactor),
          is.na(TargetUnit) ~ ResultMeasureValue.Original))
      
      # rename ResultMeasure.MeasureUnitCode to ResultMeasureUnitCode.Original and Target Unit to ResultMeasure.MeasureUnitCode 
      flag.data <- flag.data %>%
        dplyr::rename(ResultMeasureUnitCode.Original = ResultMeasure.MeasureUnitCode) 
      
      # populate ResultMeasure.MeasureUnitCode
      flag.data <- flag.data %>%
        # apply function row by row
        dplyr::rowwise() %>%
        # use target unit where there is a target unit, use original unit if no target unit
        dplyr::mutate(ResultMeasure.MeasureUnitCode = dplyr::case_when(
          !is.na(TargetUnit) ~ TargetUnit,
          is.na(TargetUnit) ~ ResultMeasureUnitCode.Original))
      # capitalize ResultMeasure.MeasureUnitCode column for consistency 
      flag.data$ResultMeasure.MeasureUnitCode <- toupper(flag.data$ResultMeasure.MeasureUnitCode)
      
      # edit ResultUnitConversion column
      clean.data <- flag.data %>%
      # apply function row by row
      dplyr::rowwise() %>%
      # create flag column
      dplyr::mutate(ResultUnitConversion = dplyr::case_when(
        !is.na(ResultMeasure.MeasureUnitCode) ~ as.character("Converted"),
        TRUE ~ ResultUnitConversion))  
      
      # remove extraneous columns, fix field names
      clean.data <- clean.data %>%
      dplyr::select(-c("ConversionFactor", "TargetUnit"))
      
      # reorder column names to match .data
      # get .data column names
      col.order <- colnames(.data)
      # add PotentialDupRowID column to the list
      col.order <- append(col.order, c("ResultMeasureValue.Original",
                                       "ResultMeasureUnitCode.Original",
                                       "ResultUnitConversion"))
      # reorder columns in clean.data
      clean.data <- clean.data[, col.order]
      
      return(clean.data)
      
    } else {
      stop("convert argument must be Boolean (TRUE or FALSE)")
    }
  }
}