
library(dplyr)

#' TADA Auto Clean
#' 
#' **Placeholder text for function description
#'
#' @param .data TADA dataset
#' @param FlaggedData Boolean argument indicating whether output will have columns appended to flag data or the output will be a cleaned dataset.
#'
#' @return Full TADA dataset with duplicate records and continuous data
#' @export
#'
#' @examples WQP.QCed <- TADAautoClean(WQP.raw)

TADAautoClean <- function(.data, FlaggedData = TRUE){
  
  field.names <- colnames(.data)
  excluded.fields <- c("ActivityIdentifier", "ActivityConductingOrganizationText",
                       "OrganizationFormalName", "OrganizationIdentifier",
                       "ProjectIdentifier", "ResultCommentText", "ActivityCommentText")
  dupe.fields <- field.names[!field.names %in% excluded.fields]
  
  if(TADAprofileCheck(.data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  if(TADAprofileCheck(.data) == TRUE) {
    
    if(FlaggedData == FALSE) {
      # Remove type 1 duplicate
      clean.data <- .data[!duplicated(.data),]   
      # Remove type 2 duplicate
      clean.data <- clean.data[!duplicated(clean.data[dupe.fields]),]
      # Remove continuous data
      clean.data <- filter(clean.data, 
                           ResultDetectionConditionText != "Reported in Raw Data (attached)" |
                             is.na(ResultDetectionConditionText))
      
      return(clean.data)
    }
    
    if(FlaggedData == TRUE) {
      # Remove type 1 duplicate
      flag.data <- .data[!duplicated(.data),]   
      # Flag type 2 duplicate
      flag.data$Duplicate.2 <- as.integer(duplicated(flag.data[dupe.fields]) |
                                            duplicated(flag.data[dupe.fields],
                                                       fromLast = TRUE))
      # Flag continuous data
      # make cont.data data frame
      cont.data <- filter(flag.data, 
                          ResultDetectionConditionText == "Reported in Raw Data (attached)")
      # append ContDataFlag column
      cont.data$ContDataFlag <- 1
      # join cont.data to flag.data
      flag.data <- merge(flag.data, cont.data, all.x = TRUE) 
      
      return(flag.data)
    } else {
      stop("FlaggedData argument must be Boolean (TRUE or FALSE)")
    }
  }
}


#' TADA Remove Empty Columns
#' 
#' **Placeholder text for function description
#'
#' @param .data TADA dataset
#'
#' @return Full TADA dataset without columns containing only NA values
#' @export
#'
#' @examples WQP.QCed <- TADAremoveEmptyColumns(WQP.raw)

TADAremoveEmptyColumns <- function(.data){
  # Remove columns with only NAs
  .data %>%
    select(where(~!all(is.na(.x))))
  
}

# WQP.raw <- readWQPdata(organization = c("USGS-NJ", "21NJBCH"), Sitetype = c(
#   "Lake, Reservoir, Impoundment", "Stream"), Samplemedia = c("water", "Water"),
#   startDate = "01-01-2017", endDate = "01-01-2022")