#' Simple Tools for Censored Data Handling
#' 
#' This function determines if detection limit type and detection condition are parsimonious
#' before applying simple tools for non-detect and over-detect data handling, including filling
#' in the values as-is, X times the detection limit, or a random number between 0
#' and the LOWER detection limit. These methods do NOT depend upon censored data frequency
#' in the dataset.
#' 
#' @param .data A post-idCensoredData() TADA dataframe
#' @param nd_method A text string indicating the type of method used to populate a non-detect (lower limit) data value. Can be set to "multiplier" (default),"randombelowlimit", or "as-is".
#' @param nd_multiplier A number to be multiplied to the LOWER detection limit for each entry to obtain the censored data value. Must be supplied if nd_method = "multiplier". Defaults to 0.5, or half the detection limit.
#' @param od_method A text string indicating the type of method used to populate an over-detect (upper limit) data value. Can be set to "multiplier" or "as-is" (default).
#' @param od_multiplier A number to be multiplied to the UPPER detection limit for each entry to obtain the censored data value. Must be supplied if od_method = "multiplier". Defaults to 0.5, or half the detection limit.
#' 
#' @return A TADA dataframe with an additional column named TADA.Censored_Method, which documents the method used to fill censored data values.
#' 
#' @export


simpleCensoredMethods <- function(.data, nd_method = "multiplier", nd_multiplier = 0.5, od_method = "as-is", od_multiplier = "null"){
  # check .data has all of the required columns
  expected_cols <- c(
    "ResultDetectionConditionText",
    "DetectionQuantitationLimitTypeName",
    "TADA.ResultMeasureValue.DataTypeFlag"
  )
  
  # check that multiplier is provided if method = "multiplier"
  if(nd_method=="multiplier"&nd_multiplier=="null"){
    stop("Please provide a multiplier for the lower detection limit handling method of 'multiplier'. Typically, the multiplier value is between 0 and 1.")
  }
  if(od_method=="multiplier"&od_multiplier=="null"){
    stop("Please provide a multiplier for the upper detection limit handling method of 'multiplier'. Typically, the multiplier value is between 0 and 1.")
  }
  
  ## First step: identify censored data
  cens = .data%>%dplyr::filter(TADA.ResultMeasureValue.DataTypeFlag=="Result Value/Unit Copied from Detection Limit")
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
    TADA.Detection_Type=="Non-Detect"&TADA.Limit_Type=="Non-Detect" ~ as.character("Non-Detect"),
    TADA.Detection_Type=="Over-Detect"&TADA.Limit_Type=="Over-Detect" ~ as.character("Over-Detect"),
    TADA.Detection_Type=="Other"&TADA.Limit_Type=="Other" ~ as.character("Other Condition/Limit Populated"),
    !TADA.Detection_Type==TADA.Limit_Type ~ as.character("Conflict between Condition and Limit")
  ))
  
  ## warn when some limit metadata may be problematic
  if("Conflict between Condition and Limit"%in%cens$TADA.Censored_Flag){
    num = length(cens$TADA.Censored_Flag[cens$TADA.Censored_Flag=="Conflict between Condition and Limit"])
    warning(paste0(num," records in supplied dataset have conflicting detection condition and detection limit type information. These records will not be included in detection limit handling calculations."))
  }
  
  cens = cens%>%dplyr::select(-TADA.Detection_Type, -TADA.Limit_Type)
  
  # split out over detects and non detects
  nd = subset(cens, cens$TADA.Censored_Flag=="Non-Detect")
  od = subset(cens, cens$TADA.Censored_Flag=="Over-Detect")
  other = subset(cens, !cens$ResultIdentifier%in%c(nd$ResultIdentifier,od$ResultIdentifier))
  
  # ND handling
  if(dim(nd)[1]>0){
    if(nd_method=="multiplier"){
      nd$TADA.ResultMeasureValue = nd$TADA.ResultMeasureValue*nd_multiplier
      nd$TADA.Censored_Method = paste0("Detection Limit Value Multiplied by ",nd_multiplier)
    }
    if(nd_method=="randombelowlimit"){
      nd$multiplier = runif(dim(nd)[1],0,1)
      nd$TADA.ResultMeasureValue = nd$TADA.ResultMeasureValue*nd$multiplier
      nd$TADA.Censored_Method = paste0("Random Value Between 0 and Detection Limit Using this Multiplier: ",round(nd$multiplier,digits=3))
      nd = nd%>%dplyr::select(-multiplier)
    }
    if(nd_method=="as-is"){
      nd$TADA.Censored_Method = "Detection Limit Value Unchanged"
    }
  }
  # OD handling
  if(dim(od)[1]>0){
    if(od_method=="multiplier"){
      od$TADA.ResultMeasureValue = od$TADA.ResultMeasureValue*od_multiplier
      od$TADA.Censored_Method = paste0("Detection Limit Value Multiplied by ",od_multiplier)
    }
    if(od_method=="as-is"){
      od$TADA.Censored_Method = "Detection Limit Value Unchanged"
    }
  }
  
  .data = plyr::rbind.fill(not_cens, nd, od, other)
  .data = OrderTADACols(.data)
  return(.data)
}
