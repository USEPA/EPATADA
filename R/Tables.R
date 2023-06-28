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

TADA_SummarizeColumn <- function(.data,col="TADA.CharacteristicName"){
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
#' TADAProfileCleanTP_stats <- TADA_Stats(TADAProfileCleanTP)
#' 

TADA_Stats <- function(.data){

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

#' Summarize Data Stats
#' 
#' This function creates a summary table of the percentage of non-detects by 
#' specified ID columns. It can be used to determine the best method for handling 
#' censored data estimation methods that depend upon the distribution of the dataset.
#' 
#' @param .data A TADA dataframe
#' @param spec_cols A vector of column names to be used as aggregating variables when summarizing censored data information.
#' @return A summary dataframe yielding measurement ncounts, censored data ncounts, 
#' and percent of dataset that is censored, aggregated by user-defined grouping 
#' variables. Also produces a column "TADA.Censored.Note" that identifies 
#' when there is sufficient non-censored data to estimate censored data using statistical
#' methods including Maximum Likelihood Estimation, Robust ROS and Kaplan Meier.
#' The decision tree used to identify applicable statistical analyses is based 
#' on the Baseline Assessment of Left-Censored Environmental Data Using R Tech Note.
#' More info can be found here: https://www.epa.gov/sites/default/files/2016-05/documents/tech_notes_10_jun2014_r.pdf
#' 
#' 
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data(TADAProfileCleanTP)
#' # TADAProfileCleanTP dataframe is clean, harmonized, and filtered
#' # down to one Comparable Data Identifier
#' 
#' # Create TADA_SummarizeCensoredData table:
#' TADAProfileCleanTP_TADA_SummarizeCensoredData <- TADA_SummarizeCensoredData(TADAProfileCleanTP)
#' 

TADA_SummarizeCensoredData <- function(.data, spec_cols = c("TADA.CharacteristicName","TADA.ResultMeasure.MeasureUnitCode","TADA.ResultSampleFractionText","TADA.MethodSpecificationName")){
  
  if(any(is.na(.data$TADA.ResultMeasureValue))){
    warning("Dataset contains data missing both a result value and a detection limit. Suggest removing or handling. See TADA Harmonization vignette for an example.")
  }
  
  if(!"TADA.CensoredData.Flag"%in%names(.data)){
    cens = TADA_IDCensoredData(.data)
  }else{
    cens = .data
  }
  
  sum_low = cens%>%dplyr::group_by_at(spec_cols)%>%
    dplyr::filter(TADA.CensoredData.Flag%in%c("Non-Detect", "Uncensored"))%>%
    dplyr::summarise(Measurement_Count = length(unique(ResultIdentifier)), Censored_Count = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag=="Non-Detect"]), Percent_Censored = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag=="Non-Detect"])/length(TADA.CensoredData.Flag)*100, Censoring_Levels = length(unique(TADA.ResultMeasureValue[TADA.CensoredData.Flag=="Non-Detect"])))%>%
    dplyr::filter(Censored_Count>0)%>%
    dplyr::mutate("TADA.CensoredData.Flag" = "Non-Detect")
  
  sum_hi = cens%>%dplyr::group_by_at(spec_cols)%>%
    dplyr::filter(TADA.CensoredData.Flag%in%c("Over-Detect", "Uncensored"))%>%
    dplyr::summarise(Measurement_Count = length(unique(ResultIdentifier)), Censored_Count = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag=="Over-Detect"]), Percent_Censored = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag=="Over-Detect"])/length(TADA.CensoredData.Flag)*100, Censoring_Levels = length(unique(TADA.ResultMeasureValue[TADA.CensoredData.Flag=="Over-Detect"])))%>%
    dplyr::filter(Censored_Count>0)%>%
    dplyr::mutate("TADA.CensoredData.Flag" = "Over-Detect")
  
  sum_all = plyr::rbind.fill(sum_low, sum_hi)
  
  sum_all = sum_all%>%dplyr::mutate(TADA.Censored.Note = dplyr::case_when(
    Percent_Censored>80 ~ as.character("Percent censored too high for estimation methods"), # greater than 80, cannot estimate
    Percent_Censored<50&Censoring_Levels>1 ~ as.character("Kaplan-Meier"), # less than 50% censored, and multiple censoring levels (no minimum n)
    Percent_Censored<50 ~ as.character("Robust Regression Order Statistics"), # less than 50% censored and one censoring level (no minimum n?)
    Measurement_Count>=50 ~ as.character("Maximum Likelihood Estimation"), # 50%-80% censored, 50 or more measurements
    Measurement_Count<50 ~ as.character("Robust Regression Order Statistics"), # 50%-80% censored, less than 50 measures
  ))
  if(dim(sum_all)[1]==0){
    print("No censored data to summarize. Returning empty data frame.")
  }
  return(sum_all)
}