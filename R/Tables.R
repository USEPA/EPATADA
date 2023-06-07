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

TADA_summarizeColumn <- function(.data,col="TADA.CharacteristicName"){
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


#' Generate Statistics Table
#' 
#' @param .data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record. Data frame must include the columns
#' 'TADA.ComparableDataIdentifier" and 'MonitoringLocationIdentifier' to run 
#' this function. The 'TADA.ComparableDataIdentifier' can be added to the data
#' frame by running the function HarmonizeData().
#' 
#' @return stats table
#' 
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data(TADAProfileCleanTP)
#' # TADAProfileCleanTP dataframe is clean, harmonized, and filtered
#' # down to one Comparable Data Identifier
#' 
#' # Create stats table:
#' TADAProfileCleanTP_stats <- TADA_stats(TADAProfileCleanTP)
#' 

TADA_stats <- function(.data){

  StatsTable <- .data %>%
    dplyr::group_by(TADA.ComparableDataIdentifier) %>%
    dplyr::summarize(Location_Count = length(unique(MonitoringLocationIdentifier)),
                     Measurement_Count = length(unique(ResultIdentifier)), 
                     # To build this fence we take 1.5 times the IQR and then subtract this value 
                     # from Q1 and add this value to Q3. This gives us the minimum and maximum fence
                     # posts that we compare each observation to. Any observations that are more than
                     # 1.5 IQR below Q1 or more than 1.5 IQR above Q3 are considered outliers
                     NA_Count = sum(is.na(.data$TADA.ResultMeasureValue)),
                     UpperFence = (stats::quantile(TADA.ResultMeasureValue, c(.75))+(1.5*stats::IQR(TADA.ResultMeasureValue))), 
                     LowerFence = (stats::quantile(TADA.ResultMeasureValue, c(.25))-(1.5*stats::IQR(TADA.ResultMeasureValue))), 
                     Min = min(TADA.ResultMeasureValue),
                     Mean = mean(TADA.ResultMeasureValue),
                     Max = max(TADA.ResultMeasureValue), 
                     Percentile_5th = stats::quantile(TADA.ResultMeasureValue, .05),
                     Percentile_10th = stats::quantile(TADA.ResultMeasureValue, .10),
                     Percentile_15th = stats::quantile(TADA.ResultMeasureValue, .15),
                     Percentile_25th = stats::quantile(TADA.ResultMeasureValue, .25),
                     Percentile_50th_Median = stats::quantile(TADA.ResultMeasureValue, .50),
                     Percentile_75th = stats::quantile(TADA.ResultMeasureValue, .75),
                     Percentile_85th = stats::quantile(TADA.ResultMeasureValue, .85),
                     Percentile_95th = stats::quantile(TADA.ResultMeasureValue, .95),
                     Percentile_98th = stats::quantile(TADA.ResultMeasureValue, .98)
                     )
  
  return(StatsTable)
}
