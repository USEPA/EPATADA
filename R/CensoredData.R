# Censored Data

## Two objectives:
# TransformCensoredData
# GenerateCensoredDataStats

#' Generate list of unique values in a given field
#'
#' Function substitutes monitoring device/method detection limits (if available) as result values when applicable.
#'
#' @param transform Boolean argument with two possible values, “TRUE” and “FALSE”.
#' Default is transform = TRUE.
#' 
#' @param .data Optional argument; TADA dataframe
#'
#' @return When transform=TRUE, monitoring device/method detection limits (if available) are substituted as result values and units.
#' When transform = FALSE, monitoring device/method detection limits (if available) are NOT substituted as result values and units - 
#' Instead, columns are appended to rows that may include censored data. The flag indicates 1) if the row contains censored data, and 2)
#' if monitoring device/method detection limits are available.  
#' 
#' @export
#'

TransformCensoredData <- function(transform, .data) {
  
  # check .data is of class data.frame
  if (!missing(.data)) {
    if (("data.frame" %in% class(.data)) == FALSE) {
      stop(".data must be of class 'data.frame'")
    }
  }
  # execute function after checks are passed 
  {
    
  }
}

    