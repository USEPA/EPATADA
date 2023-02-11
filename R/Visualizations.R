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
