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

TADA_SummarizeColumn <- function(.data, col = "TADA.CharacteristicName") {
  .data$summ <- .data[, col]
  # Summarize WQP data pull
  wqp_summary <- .data %>%
    dplyr::group_by(summ) %>%
    dplyr::summarize(
      n_sites = length(unique(MonitoringLocationIdentifier)),
      n_records = length(TADA.ResultMeasureValue),
      .groups = "drop"
    ) %>%
    dplyr::select(summ, n_sites, n_records)
  names(wqp_summary)[names(wqp_summary) == "summ"] <- col
  return(wqp_summary)
}

#' Generate Statistics Table
#'
#' This function creates a summary table of the dataset based on grouping
#' columns. The 'TADA.ComparableDataIdentifier' column is the required and
#' default grouping column, but the user may include additional columns if
#' desired. The summary table includes the measurement count, location count,
#' censored data stats, min, max, and percentile stats, and a suggested
#' non-detect estimation method. The estimation method is based on the following
#' article: Baseline Assessment of Left-Censored Environmental Data Using R Tech
#' Note. More info can be found here:
#' https://www.epa.gov/sites/default/files/2016-05/documents/tech_notes_10_jun2014_r.pdf
#' Suggested methods are based on the measurement count, the number of
#' non-detects in the dataset, and the number of censoring levels (detection
#' limit types) and methods include Maximum Likelihood Estimation, Robust ROS
#' and Kaplan Meier.
#'
#' @param .data TADA data frame containing the data downloaded from the WQP,
#'   where each row represents a unique data record. Data frame must include the
#'   columns 'TADA.ResultMeasureValue', 'TADA.ResultMeasure.MeasureUnitCode',
#'   'TADA.ResultSampleFractionText', 'TADA.MethodSpeciationName',
#'   'TADA.ComparableDataIdentifier', 'TADA.CensoredData.Flag',
#'   'DetectionQuantitationLimitTypeName', and 'MonitoringLocationIdentifier' to
#'   run this function. The 'TADA.ComparableDataIdentifier' can be added to the
#'   data frame by running the function TADA_CreateComparableID().
#'
#' @param group_cols This function automatically uses
#'   'TADA.ComparableDataIdentifier' as a grouping column. However, the user may
#'   want to summarize their dataset by additional grouping columns. For
#'   example, a user may want to create a summary table where each row is
#'   specific to one comparable data identifier AND one monitoring location.
#'   This input would look like: group_cols = c("MonitoringLocationIdentifier")
#'
#' @return stats table
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_6Tribes_5y_Harmonized)
#' # Create stats table:
#' Data_6Tribes_5y_Harmonized_stats <- TADA_Stats(Data_6Tribes_5y_Harmonized)
#'
TADA_Stats <- function(.data, group_cols = c("TADA.ComparableDataIdentifier")) {
  if (any(is.na(.data$TADA.ResultMeasureValue))) {
    sumNAs <- length(.data$TADA.ResultMeasureValue[is.na(.data$TADA.ResultMeasureValue)])
    print(paste0("Dataset contains ", sumNAs, " results missing both a TADA result value and a detection limit. These values will not be represented in the stats summary table. Suggest removing or handling."))
  }

  if (!"TADA.CensoredData.Flag" %in% names(.data)) {
    .data <- TADA_IDCensoredData(.data)
  }

  if (!"TADA.CensoredData.Flag" %in% names(.data)) {
    .data <- TADA_IDCensoredData(.data)
  }

  if ("TADA.NutrientSummation.Flag" %in% names(.data)) {
    print("Note: Your dataset contains TADA-generated total nutrient results, which have fewer columns populated with metadata. This might affect how groups are displayed in the stats table.")
  }

  group_cols <- unique(c("TADA.ComparableDataIdentifier", group_cols))

  StatsTable <- .data %>%
    dplyr::filter(!is.na(TADA.ResultMeasureValue)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      Location_Count = length(unique(MonitoringLocationIdentifier)),
      Measurement_Count = length(unique(ResultIdentifier)),
      Non_Detect_Count = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Non-Detect")]),
      Non_Detect_Pct = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Non-Detect")]) / length(TADA.CensoredData.Flag) * 100,
      Non_Detect_Lvls = length(unique(DetectionQuantitationLimitTypeName[TADA.CensoredData.Flag %in% c("Non-Detect")])),
      Over_Detect_Count = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Over-Detect")]),
      Over_Detect_Pct = length(TADA.CensoredData.Flag[TADA.CensoredData.Flag %in% c("Over-Detect")]) / length(TADA.CensoredData.Flag) * 100,
      # To build this fence we take 1.5 times the IQR and then subtract this value
      # from Q1 and add this value to Q3. This gives us the minimum and maximum fence
      # posts that we compare each observation to. Any observations that are more than
      # 1.5 IQR below Q1 or more than 1.5 IQR above Q3 are considered outliers
      UpperFence = (stats::quantile(TADA.ResultMeasureValue, c(.75)) + (1.5 * stats::IQR(TADA.ResultMeasureValue))),
      LowerFence = (stats::quantile(TADA.ResultMeasureValue, c(.25)) - (1.5 * stats::IQR(TADA.ResultMeasureValue))),
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
    ) %>%
    dplyr::mutate(ND_Estimation_Method = dplyr::case_when(
      Non_Detect_Pct == 0 ~ as.character("No non-detects to estimate"),
      Non_Detect_Pct > 80 ~ as.character("Percent censored too high for estimation methods"), # greater than 80, cannot estimate
      Non_Detect_Pct < 50 & Non_Detect_Lvls > 1 ~ as.character("Kaplan-Meier"), # less than 50% censored, and multiple censoring levels (no minimum n)
      Non_Detect_Pct < 50 ~ as.character("Robust Regression Order Statistics"), # less than 50% censored and one censoring level (no minimum n?)
      Measurement_Count >= 50 ~ as.character("Maximum Likelihood Estimation"), # 50%-80% censored, 50 or more measurements
      Measurement_Count < 50 ~ as.character("Robust Regression Order Statistics")
    )) # 50%-80% censored, less than 50 measures

  # StatsTable = StatsTable[,!names(StatsTable)%in%c("Non_Detect_Pct","Non_Detect_Lvls","Over_Detect_Pct")]

  return(StatsTable)
}
