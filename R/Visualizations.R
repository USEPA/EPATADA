#' @title Summarize data downloaded by specified column
#' 
#' @description 
#' Function to summarize the number of sites and records downloaded from the 
#' WQP for each unique column group.
#' 
#' @param .data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record.
#' @param col A text string name of the column the user would like summarized.
#' 
#' @return A dataframe containing a column for each unique element, the number of
#' sites with that element populated, and the number of records with that element
#' populated.
#'  
#' @export
#'  

SummarizeColumn <- function(.data,col){
  .data$summ = .data[,col]
  # Summarize WQP data pull
  wqp_summary <- .data %>%
    dplyr::group_by(summ) %>%
    dplyr::summarize(n_sites = length(unique(MonitoringLocationIdentifier)),
                     n_records = length(TADA.ResultMeasureValue),
                     .groups = 'drop') %>%
    dplyr::select(summ, n_sites, n_records)
  names(wqp_summary)[names(wqp_summary)=="summ"] = col
  return(wqp_summary)
}
