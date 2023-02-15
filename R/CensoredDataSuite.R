#' Identify and flag censored data
#' 
#' This function combs through the multitude of ways in which censored data are
#' documented in the WQP and makes a determination for data handling.
#' 
#' @param .data An autocleaned TADA dataframe
#' 
#' @return A TADA dataframe with additional columns named TADA.CensoredDataType and TADA.CensoredDataFlag
#' 
#' @export

idCensoredData <- function(.data){
  if(!c("TADA.ResultMeasureValue","TADA.DetectionQuantitationLimitMeasure.MeasureValue")%in%names(.data)){
    .data <- ConvertSpecialChars(.data, "ResultMeasureValue")
    .data <- ConvertSpecialChars(.data, "DetectionQuantitationLimitMeasure.MeasureValue")
  }
} 