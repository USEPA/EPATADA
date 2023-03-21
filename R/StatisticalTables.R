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

SummarizeColumn <- function(.data,col="TADA.CharacteristicName"){
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

#' Create Boxplot
#' 
#' @param filtered.data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record. Data frame must include the columns
#' 'TADA.ComparableDataIdentifier", 'TADA.ResultMeasureValue', and
#' 'TADA.ResultMeasure.MeasureUnitCode' to run this function. These columns can 
#' be added to the data frame by running the function HarmonizeData(). The data frame
#' must be filtered down to a single comparable data identifier to run this function.
#' 
#' @return outliers
#' 
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data("Nutrients_Utah_Clean_TNonly")
#' # TADAProfileClean18_TNonly dataframe is clean, harmonized, and filtered
#' # down to one Comparable Data Identifier
#' 
#' # Create boxplot:
#' TADA_percentiles(TADAProfileClean18_TNonly)
#' 

TADA_percentiles <- function(filtered.data) {
  
  what = SummarizeColumn(TADAProfileClean18_TNonly, col="TADA.ComparableDataIdentifier")
  
  percentiles = stats::quantile(filtered.data$TADA.ResultMeasureValue, 
                                c(.05, .10, .15, .25, .50, .75, 
                                  .85, .95, .98))
  
  stats = summary(filtered.data$TADA.ResultMeasureValue)
  
  library(outliers)
  UpperBound=quantile(filtered.data$TADA.ResultMeasureValue, 
                      c(.75))+(1.5*IQR(filtered.data$TADA.ResultMeasureValue))
  LowerBound=quantile(filtered.data$TADA.ResultMeasureValue, 
                      c(.25))-(1.5*IQR(filtered.data$TADA.ResultMeasureValue))
  chisq.out.test(filtered.data$TADA.ResultMeasureValue)

  return(percentiles)
}

