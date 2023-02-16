#' Identify and flag censored data
#' 
#' This function identifies censored data (over- and non-detects) and checks whether 
#' discrepancies exist in the metadata. It then fills the TADA.ResultMeasureValue
#' and TADA.ResultMeasureUnit with detection limit values and units, and converts
#' the TADA.DataType column to "Censored"
#' 
#' @param .data An autocleaned TADA dataframe
#' 
#' @return A TADA dataframe with additional columns named TADA.CensoredDataType and TADA.CensoredDataFlag
#' 
#' @export

idCensoredData <- function(.data){
  if(!"TADA.ResultMeasureValue"%in%names(.data)){
    .data <- ConvertSpecialChars(.data, "ResultMeasureValue")
  }
  if(!"TADA.DetectionQuantitationLimitMeasure.MeasureValue"%in%names(.data)){
  .data <- ConvertSpecialChars(.data, "DetectionQuantitationLimitMeasure.MeasureValue")
  }
  if(!"TADA.ResultMeasure.MeasureUnitCode"%in%names(.data)){
    .data$TADA.ResultMeasure.MeasureUnitCode = .data$ResultMeasure.MeasureUnitCode
  }

# check .data has all of the required columns
expected_cols <- c(
  "ResultDetectionConditionText",
  "DetectionQuantitationLimitTypeName",
  "DetectionQuantitationLimitMeasure.MeasureValue",
  "DetectionQuantitationLimitMeasure.MeasureUnitCode"
)
checkColumns(.data, expected_cols)

## Notification: function will use RV as-is, even if populated with detection limit
print("Note: This function operates on rows where the TADA.ResultMeasureValue is NA. Detection limit values populated as a result value will not be considered for censored data analyses.")

## First step: identify censored data
cens = .data%>%dplyr::filter(is.na(TADA.ResultMeasureValue)&!is.na(TADA.DetectionQuantitationLimitMeasure.MeasureValue))
not_cens = .data%>%dplyr::filter(!ResultIdentifier%in%cens$ResultIdentifier)

## Bring in det cond reference table
cond.ref = GetDetCondRef()%>%dplyr::rename(ResultDetectionConditionText = Name)%>%dplyr::select(ResultDetectionConditionText, TADA.Detection_Type)

## Join to censored data
cens = dplyr::left_join(cens, cond.ref)

## Bring in det limit type reference table
limtype.ref = GetDetLimitRef()%>%dplyr::rename(DetectionQuantitationLimitTypeName = Name)%>%dplyr::select(DetectionQuantitationLimitTypeName, TADA.Limit_Type)

## Join to censored data
cens = dplyr::left_join(cens, limtype.ref)

## Create flag for condition and limit type combinations
cens = cens%>%dplyr::mutate(TADA.Censored_Flag = dplyr::case_when(
  TADA.Detection_Type==TADA.Limit_Type ~ as.character("No Conflict"),
  !TADA.Detection_Type==TADA.Limit_Type ~ as.character("Conflict")
))

## Fill TADA.ResultMeasureValue and Unit with detection limit info
cens$TADA.ResultMeasureValue = cens$TADA.DetectionQuantitationLimitMeasure.MeasureValue
cens$TADA.ResultMeasure.MeasureUnitCode = cens$DetectionQuantitationLimitMeasure.MeasureUnitCode
cens$TADA.ResultMeasureValue.DataTypeFlag = "Censored"

flag.data = plyr::rbind.fill(not_cens, cens)

flag.data = flag.data%>%
  dplyr::relocate("TADA.ResultMeasure.MeasureUnitCode",.after = "ResultMeasure.MeasureUnitCode")

return(flag.data)
}

#' Censored Data Solutions
#' 
#' This function groups data in a user-specified fashion and determines the frequency of
#' non-detects. Given the frequency, it offers a suggested censored data solution.
#' 
#' @param .data An autocleaned TADA dataframe
#' @param group_cols A vector of column names over which the function will group data to determine applicable censored data solutions.
#' 
#' @return A TADA dataframe with an added TADA.Censored_Suggest column that specifies a suitable method for handling censored data.
#' 
#' @export

suggestCensoredMethod <- function(){
  
}


#' Simple Tools for Censored Data Handling
#' 
#' This function offers simple tools for censored data handling, including filling
#' in the values as-is, half of the detection limit, or a random number between 0
#' and the detection limit. It does not depend upon the rest of the data in the sample 
#' 
#' @param .data A post-idCensoredData() TADA dataframe
#' @param method A text string indicating the type of method used to populate censored data value. Can be set to "halflimit" (default), or "randombelowlimit".
#' 
#' @return A TADA dataframe with an additional column named TADA.Censored_Method, which documents the method used to fill censored data values.
#' 
#' @export


simpleCensoredMethods <- function(.data, method = "halflimit"){
  # check .data has all of the required columns
  expected_cols <- c(
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "DetectionQuantitationLimitMeasure.MeasureValue",
    "DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )
  
  ## Notification: function will use RV as-is, even if populated with detection limit
  if("TADA.Censored_Flag"%in%names(.data)){
    if("Conflict"%in%.data$TADA.Censored_Flag){
      warning("One or more censored data records have conflicting information in the ResultDetectionConditionText and DetectionQuantitationLimitTypeName columns. Consider evaluating these records before proceeding.")
    }
  }
  
  if(method=="halflimit"){
    .data$TADA.ResultMeasureValue = ifelse(.data$TADA.ResultMeasureValue.DataTypeFlag=="Censored",.data$TADA.ResultMeasureValue*0.5,.data$TADA.ResultMeasureValue)
    .data$TADA.Censored_Method = "Half Detection Limit Value"
  }
  if(method=="randombelowlimit"){
    .data$multiplier = runif(dim(.data)[1],0,1)
    .data$TADA.ResultMeasureValue = ifelse(.data$TADA.ResultMeasureValue.DataTypeFlag=="Censored",.data$TADA.ResultMeasureValue*.data$multiplier,.data$TADA.ResultMeasureValue)
    .data$TADA.Censored_Method = "Random Value Between 0 and Detection Limit"
    .data = .data%>%dplyr::select(-multiplier)
  }
  
  return(.data)
}


