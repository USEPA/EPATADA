#' Depth Unit Conversion
#' 
#' **placeholder text for function description
#'
#' @param .data TADA dataset
#' @param execute Boolean input indicating if conversions should be executed. 
#' The default is FALSE, where the output is .data with a TargetUnit and a 
#' ConversionFactor column appended. If execute = TRUE, the output does not 
#' append additional columns and applies the conversion to the ActivityDepthHeightMeasure.MeasureUnitCode
#'
#' @return Full TADA dataset with depth units 
#' @export
#' 


TADAconvertDepthUnit <- function(.data, execute = FALSE){
  
  # check that .data object is compatible with TADA
  if(TADAprofileCheck(.data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(TADAprofileCheck(.data) == TRUE) {
    # read in unit conversion reference table from sysdata.rda
    unit.ref <- TADA:::unit.ref
    # Join target unit and conversion factor from unit.ref to .data by ActivityDepthHeightMeasure.MeasureUnitCode
    TADA.data <- merge(.data, unit.ref[, c("Original.Unit", "Target.Unit", "Conversion.Factor")],
                     by.x = "ActivityDepthHeightMeasure.MeasureUnitCode", 
                     by.y = "Original.Unit", 
                     all.x = TRUE)
    # if execute = FALSE, output data
    if(execute == FALSE) {
      return(TADA.data)
    }
    # if execute = TRUE, apply conversion
    if(execute == TRUE) {
      # divide ActivityDepthHeightMeasure.MeasureValue by ConversionFactor
      TADA.data$ActivityDepthHeightMeasure.MeasureValue = 
        TADA.data$ActivityDepthHeightMeasure.MeasureValue/TADA.data$Conversion.Factor
      # replace ActivityDepthHeightMeasure.MeasureUnitCode values with TargetUnit values
      TADA.data$ActivityDepthHeightMeasure.MeasureUnitCode =
        TADA.data$Target.Unit
      # remove appended columns (target unit and conversion factor)
      TADA.data <- dplyr::select(TADA.data, -c("Target.Unit", "Conversion.Factor"))
      
      return(TADA.data)
    } else {
      stop("'execute' argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Apply Conversions
#' 
#' **placeholder text for function description
#'
#' @param .data TADA dataset
#' @param Include Conversions to execute (should input be a column name? Or 
#' groups of columns, which have their own group ID? Where would we put this
#' group ID info, another reference table? within the function?)
#' @param Exclude ibid, but conversions to not execute
#'
#' @return Full TADA dataset with data conversions executed. Function defaults 
#' to convert the following: (**note all columns/type of data included in 
#' conversions).
#' Users can specify which conversions to exclude.
#' @export
#' 


TADAapplyConversions <- function(.data, Include, Exclude){
  
}