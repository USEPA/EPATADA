#' Check for Uncommon Analytical Methods
#' 
#' Function that checks the validity of each characteristic-analytical method 
#' combination in the dataset. When clean = TRUE, rows with invalid 
#' characteristic-analytical method combinations are removed. 
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes "Invalid" characteristic-analytical 
#' method combinations from the dataset when clean = TRUE. Default is 
#' clean = FALSE.
#'
#' @return When clean = FALSE, a column indicating the validity of the 
#' combination of CharacteristicName, ResultAnalyticalMethod/MethodIdentifier, 
#' and ResultAnalyticalMethod/MethodIdentifierContext values is appended to the
#' input data set. When clean = FALSE, the column is appended, but "Invalid" 
#' rows will be removed from the dataset.
#' 
#' @export
#' 


UncommonAnalyticalMethodID <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier", 
           "ResultAnalyticalMethod.MethodIdentifierContext") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ResultAnalyticalMethod.MethodIdentifier", 
           "ResultAnalyticalMethod.MethodIdentifierContext") %in% colnames(.data)) == TRUE) {
    # delete existing flag column
    if(("UncommonAnalyticalMethod" %in% colnames(.data)) == TRUE) {
      .data <- dplyr::select(.data, -UncommonAnalyticalMethod)
    }
    # read in speciation reference table from sysdata.rda and filter
    meth.ref <- TADA:::WQXcharVal.ref %>%
      dplyr::filter(Type == "CharacteristicMethod")
    # duplicate and capitalize CharName and result unit columns in .data
    .data$Char.Upper <- toupper(.data$CharacteristicName)
    # join "Status" column to .data by CharacteristicName, Source (Media), and Value (unit)
    check.data <- merge(.data, unit.ref[, c("Characteristic", "Source", "Status", "Value")],
                        by.x = c("Char.Upper", "ResultAnalyticalMethod.MethodIdentifier", 
                                 "ResultAnalyticalMethod.MethodIdentifierContext"),
                        by.y = c("Characteristic", "Value", "Source"), all.x = TRUE)
    
    # remove Char.Upper column and rename Status column
    check.data <- check.data %>%
      dplyr::select(-c("Char.Upper", "Unit.Upper")) %>%
      dplyr::rename(UncommonAnalyticalMethod = Status)
    # rename values in UncommonAnalyticalMethod
    check.data["UncommonAnalyticalMethod"][check.data["UncommonAnalyticalMethod"] == "Valid"] <- "N"
    check.data["UncommonAnalyticalMethod"][check.data["UncommonAnalyticalMethod"] == "Invalid"] <- "Y"
    # rename NA values to Unknown in UncommonAnalyticalMethod column 
    check.data["UncommonAnalyticalMethod"][is.na(check.data["UncommonAnalyticalMethod"])] <- "Unknown"
    
    if(clean == FALSE) {
      return(check.data)
    }
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(check.data, UncommonAnalyticalMethod != "Y")
      
      return(clean.data)
    } else {
      stop("clean argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' Check for Aggregated Continuous Data
#' 
#' The Water Quality Portal (WQP) is not designed to store high-frequency 
#' (i.e. sensor) data. However, sometimes data providers choose to aggregate
#' their continuous data and submit it to WQP as one value. This type of data
#' may not be suitable for a water quality assessment, therefore, this function
#' uses metadata submitted by data providers to flags rows with aggregated 
#' continuous data. When clean = TRUE, rows with aggregated continuous data
#' are removed from the dataset.
#'
#' @param .data TADA dataset
#' @param clean Boolean argument; removes aggregated continuous data from 
#' the dataset when clean = TRUE. Default is clean = FALSE.
#'
#' @return 
#' @export

AggregatedContinuousData <- function(.data, clean = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has required columns
  if(all(c("ResultDetectionConditionText") %in% colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # execute function after checks are passed
  if(all(c("ResultDetectionConditionText") %in% colnames(.data)) == TRUE) {
    
    # flag continuous data
      # make cont.data data frame
    cont.data <- dplyr::filter(.data, 
                               ResultDetectionConditionText == "Reported in Raw Data (attached)")
      # append ContDataFlag column
    cont.data$AggregatedContinuousData <- "Y"
      # join cont.data to flag.data
    flag.data <- merge(.data, cont.data, all.x = TRUE) 
    
    if(clean == FALSE) {
      return(flag.data)
    }
    if(clean == TRUE) {
      # filter out invalid characteristic-unit-media combinations
      clean.data <- dplyr::filter(flag.data, !(AggregatedContinuousData %in% "Y"))
      
      return(clean.data)
    } else {
      stop("clean argument must be Boolean (TRUE or FALSE)")
    }
  }
}



#' MediaNotWater
#' 
#' **Placeholder text for function description
#'
#' @param data TADA dataset
#' @param clean Clean argument indicates whether flag columns should be appended 
#' to the data (clean = FALSE), or flagged data is transformed/filtered from the 
#' dataset and no columns are appended (clean = TRUE).
#'
#' @return Full TADA dataset with flags or data removed
#' @export

MediaNotWater <- function(data, clean = TRUE){
  
  field.names <- colnames(data)
  
  if(TADAprofileCheck(data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. 
         Use either the full physical/chemical profile downloaded from WQP or 
         download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(TADAprofileCheck(data) == TRUE) {
    
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