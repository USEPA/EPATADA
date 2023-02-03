#' @title Generate Map
#'
#' @description 
#' Function will plot WQP stations on a map. Stations with an invalid or imprecise
#' latitude or longitude will be colored red; all other stations will be colored 
#' blue. 
#'
#' @param .data TADA dataframe
#' @param flag Boolean argument; set equal to "TRUE" or "FALSE".
#'
#' @return When flag = TRUE, this function colors invalid
#' or imprecise latitude or longitude values red; and colors all other stations blue.
#' When flag = FALSE, all stations are colored blue. The default is flag = TRUE.
#'
#' @export
#' 
#'

GenerateMap <- function(.data, flag = TRUE) { 
  
#create a map of the world
maps::map()

# uncomment below to create USA base map instead
# maps::map("usa")

# uncomment below to add state outlines to usa map
# maps::map("state", add=TRUE)

# uncomment below to add county outlines to usa map
# maps::map("county", add=TRUE)

# uncomment below to draw map of specific state instead, with county lines
# maps::map('county', 'utah')

# draw the site locations onto the map
graphics::points(.data$LongitudeMeasure, .data$LatitudeMeasure, col="red", pch=20) 

}


#' @title Summarize data downloaded for each Characteristic
#' 
#' @description 
#' Function to summarize the number of sites and records downloaded from the 
#' WQP for each CharacteristicName.
#' 
#' @param .data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record.
#' 
#' @return 
#' Saves a .csv file containing the total number of sites and records downloaded
#' from the Water Quality Portal for each requested characteristic name.
#'  
#' @export
#'  

SummarizeCharacteristics <- function(.data){
  
  # Summarize WQP data pull
  wqp_summary <- .data %>%
    dplyr::group_by(CharacteristicName) %>%
    dplyr::summarize(n_sites = length(unique(MonitoringLocationIdentifier)),
                     n_records = length(ResultMeasureValue),
                     .groups = 'drop') %>%
    dplyr::select(CharacteristicName, n_sites, n_records)
  
}
