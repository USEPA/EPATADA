
library(dplyr)

#' Title
#'
#' @param .data Full physical/chemical results dataset from WQP
#'
#' @return Full physical/chemical results dataset with duplicate records and continuous data
#' @export
#'
#' @examples WQP.QCed <- TADAautoClean(WQP.raw)

TADAautoClean <- function(.data){
  
  excluded.columns <- c("ActivityIdentifier", "ActivityConductingOrganizationText",
                        "OrganizationFormalName", "OrganizationIdentifier",
                        "ProjectIdentifier", "ResultCommentText", "ActivityCommentText")
  
  WQP <- .data %>% 
    # Remove type 1 duplicate data
    distinct() %>%
    distinct(across(-all_of(excluded.columns)), .keep_all = TRUE) %>%
    # Remove continuous data
    filter(ResultDetectionConditionText != "Reported in Raw Data (attached)" |
             is.na(ResultDetectionConditionText))
  
}


#' Title
#'
#' @param .data Full physical/chemical results dataset from WQP
#'
#' @return Full physical/chemical results dataset without columns containing only NA values
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