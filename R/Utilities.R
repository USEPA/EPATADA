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


ApplyConversions <- function(.data, Include, Exclude){
  
}


#' AutoFilter
#' 
#' This function can be used to autofilter and simplify a WQP dataset. 
#' After applying this function, the dataset will only contain result values for
#' water media types or chemicals in tissue (e.g. mercury in fish tissue).
#' More complex biological data (counts and macroinvertebrates) is removed. 
#' The function looks at the following fields to autofilter: 
#' ActivityMediaName, ActivityMediaSubDivisionName, AssemblageSampledName 
#'
#' @param data TADA dataset
#' @param clean Clean argument indicates whether flag columns should be appended 
#' to the data (clean = FALSE), or flagged data is transformed/filtered from the 
#' dataset and no columns are appended (clean = TRUE).
#'
#' @return Full TADA dataset with flags or data removed
#' @export



AutoFilter <- function(.data, clean = TRUE){
  
  field.names <- colnames(.data)
  
  if(TADAprofileCheck(.data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(TADAprofileCheck(.data) == TRUE) {
    
    if(clean == TRUE) {
      # Remove all data where media name does NOT equal WATER (ignore punctuation)
      cleandata <- dplyr::filter(data, ActivityMediaName == "Water")
      
      return(cleandata)
    }
    
    if(clean == FALSE) {
      # NEED TO EDIT TO ADD FLAGS, currently removes other water Water
      flagdata <- dplyr::filter(data, ActivityMediaName == "water")   
      
      return(flagdata)
    } else {
      stop("'clean' argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Filter
#' 
#' @param .data TADA dataset
#' 
#' @return Full TADA dataset with data removed
#' 
#' @export


Filter <- function(.data){

# Remove all data where media name does NOT equal WATER (ignore punctuation)
dplyr::filter(.data, ActivityMediaName == "Water")

  return(.data)
}


#' RemoveEmptyColumns
#' 
#' @param .data TADA dataset
#' 
#' @return Full TADA dataset with empty data columns removed
#' 
#' @export

RemoveEmptyColumns <- function(.data){
  # Remove columns with only NAs
  .data %>%
    select(where(~!all(is.na(.x))))
}
