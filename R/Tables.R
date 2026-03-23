#' @title Summarize data downloaded by specified column
#'
#' @description
#' Function to summarize the number of sites and records downloaded from the
#' WQP for each unique column group.
#'
#' @param .data TADA dataframe containing the data downloaded from the WQP, where
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
  wqp_summary <- .data |>
    dplyr::group_by(summ) |>
    dplyr::summarize(
      n_sites = length(unique(TADA.MonitoringLocationIdentifier)),
      n_records = length(TADA.ResultMeasureValue),
      .groups = "drop"
    ) |>
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
#' The output is formatted for readability:
#' - Continuous statistics (fences, min, mean, max, and percentiles) are rounded
#'   to a user-specified number of significant figures via `base::signif()`.
#' - Percentage fields (Non_Detect_Pct, Over_Detect_Pct) are rounded to a
#'   user-specified number of decimal places via `base::round()`.
#'
#' @param .data TADA dataframe containing the data downloaded from the WQP,
#'   where each row represents a unique data record. Dataframe must include the
#'   columns 'TADA.ResultMeasureValue', 'TADA.ResultMeasure.MeasureUnitCode',
#'   'TADA.ResultSampleFractionText', 'TADA.MethodSpeciationName',
#'   'TADA.ComparableDataIdentifier', 'TADA.CensoredData.Flag',
#'   'DetectionQuantitationLimitTypeName', and 'TADA.MonitoringLocationIdentifier' to
#'   run this function. The 'TADA.ComparableDataIdentifier' can be added to the
#'   dataframe by running the function TADA_CreateComparableID().
#' @param group_cols Character vector of additional grouping columns to include
#'   along with 'TADA.ComparableDataIdentifier'. For example:
#'   `group_cols = c("TADA.MonitoringLocationIdentifier")`.
#' @param sig_figs Integer. Number of significant figures to display for
#'   continuous statistics (UpperFence, LowerFence, Min, Mean, Max, and
#'   percentiles). Default is 3. Uses `base::signif()`.
#' @param pct_digits Integer. Number of decimal places to display for percentage
#'   fields (Non_Detect_Pct, Over_Detect_Pct). Default is 1. Uses `base::round()`.
#'
#' @return A dataframe (stats table) with one row per group and the following
#'   columns: Location_Count, Measurement_Count, Non_Detect_Count,
#'   Non_Detect_Pct, Non_Detect_Lvls, Over_Detect_Count, Over_Detect_Pct,
#'   UpperFence, LowerFence, Min, Mean, Max, Percentile_5th, Percentile_10th,
#'   Percentile_15th, Percentile_25th, Percentile_50th_Median, Percentile_75th,
#'   Percentile_85th, Percentile_95th, Percentile_98th, and ND_Estimation_Method.
#'
#' @details
#' - Missing values in `TADA.ResultMeasureValue` are excluded (`na.rm = TRUE`)
#'   when computing summary statistics.
#' - If `TADA.CensoredData.Flag` is not present, the function calls
#'   `TADA_IDCensoredData()` to create it.
#' - If `TADA.NutrientSummation.Flag` is present, an informational note is printed.
#' - This function also suggests a ND_Estimation_Method following general guidance
#'   (Kaplan-Meier, ROS, MLE) based on censored percentage, censoring levels,
#'   and measurement count.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_6Tribes_5y_Harmonized)
#'
#' # Default rounding: 3 significant figures for continuous stats, 1 decimal for percentages
#' Data_6Tribes_5y_Harmonized_stats <- TADA_Stats(Data_6Tribes_5y_Harmonized)
#'
#' # Custom rounding: 4 significant figures and whole-number percentages
#' Data_6Tribes_5y_Harmonized_stats_rounded <- TADA_Stats(
#'   Data_6Tribes_5y_Harmonized,
#'   sig_figs = 4,
#'   pct_digits = 0
#' )
TADA_Stats <- function(
  .data,
  group_cols = c("TADA.ComparableDataIdentifier"),
  sig_figs = 3,
  pct_digits = 1
) {
  if (any(is.na(.data$TADA.ResultMeasureValue))) {
    sumNAs <- length(.data$TADA.ResultMeasureValue[is.na(
      .data$TADA.ResultMeasureValue
    )])
    print(paste0(
      "Dataset contains ",
      sumNAs,
      " results missing both a TADA result value and a detection limit. These values will not be represented in the stats summary table. Suggest removing or handling."
    ))
  }

  if (!"TADA.CensoredData.Flag" %in% names(.data)) {
    .data <- TADA_IDCensoredData(.data)
  }

  if ("TADA.NutrientSummation.Flag" %in% names(.data)) {
    message(
      "Note: Your dataset contains TADA-generated total nutrient results, which have fewer columns populated with metadata. This might affect how groups are displayed in the stats table."
    )
  }

  group_cols <- unique(c("TADA.ComparableDataIdentifier", group_cols))

  StatsTable <- .data |>
    dplyr::filter(!is.na(TADA.ResultMeasureValue)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarize(
      Location_Count = length(unique(TADA.MonitoringLocationIdentifier)),
      Measurement_Count = length(unique(ResultIdentifier)),
      Non_Detect_Count = length(TADA.CensoredData.Flag[
        TADA.CensoredData.Flag %in% c("Non-Detect")
      ]),
      Non_Detect_Pct = length(TADA.CensoredData.Flag[
        TADA.CensoredData.Flag %in% c("Non-Detect")
      ]) /
        length(TADA.CensoredData.Flag) *
        100,
      Non_Detect_Lvls = length(unique(DetectionQuantitationLimitTypeName[
        TADA.CensoredData.Flag %in% c("Non-Detect")
      ])),
      Over_Detect_Count = length(TADA.CensoredData.Flag[
        TADA.CensoredData.Flag %in% c("Over-Detect")
      ]),
      Over_Detect_Pct = length(TADA.CensoredData.Flag[
        TADA.CensoredData.Flag %in% c("Over-Detect")
      ]) /
        length(TADA.CensoredData.Flag) *
        100,
      # To build this fence we take 1.5 times the IQR and then subtract this value
      # from Q1 and add this value to Q3. This gives us the minimum and maximum fence
      # posts that we compare each observation to. Any observations that are more than
      # 1.5 IQR below Q1 or more than 1.5 IQR above Q3 are considered outliers
      UpperFence = (stats::quantile(
        TADA.ResultMeasureValue,
        c(.75),
        na.rm = TRUE
      ) +
        (1.5 * stats::IQR(TADA.ResultMeasureValue, na.rm = TRUE))),
      LowerFence = (stats::quantile(
        TADA.ResultMeasureValue,
        c(.25),
        na.rm = TRUE
      ) -
        (1.5 * stats::IQR(TADA.ResultMeasureValue, na.rm = TRUE))),
      Min = min(TADA.ResultMeasureValue, na.rm = TRUE),
      Mean = mean(TADA.ResultMeasureValue, na.rm = TRUE),
      Max = max(TADA.ResultMeasureValue, na.rm = TRUE),
      Percentile_5th = stats::quantile(
        TADA.ResultMeasureValue,
        .05,
        na.rm = TRUE
      ),
      Percentile_10th = stats::quantile(
        TADA.ResultMeasureValue,
        .10,
        na.rm = TRUE
      ),
      Percentile_15th = stats::quantile(
        TADA.ResultMeasureValue,
        .15,
        na.rm = TRUE
      ),
      Percentile_25th = stats::quantile(
        TADA.ResultMeasureValue,
        .25,
        na.rm = TRUE
      ),
      Percentile_50th_Median = stats::quantile(
        TADA.ResultMeasureValue,
        .50,
        na.rm = TRUE
      ),
      Percentile_75th = stats::quantile(
        TADA.ResultMeasureValue,
        .75,
        na.rm = TRUE
      ),
      Percentile_85th = stats::quantile(
        TADA.ResultMeasureValue,
        .85,
        na.rm = TRUE
      ),
      Percentile_95th = stats::quantile(
        TADA.ResultMeasureValue,
        .95,
        na.rm = TRUE
      ),
      Percentile_98th = stats::quantile(
        TADA.ResultMeasureValue,
        .98,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ND_Estimation_Method = dplyr::case_when(
        Non_Detect_Pct == 0 ~ as.character("No non-detects to estimate"),
        Non_Detect_Pct > 80 ~ as.character(
          "Percent censored too high for estimation methods"
        ),
        Non_Detect_Pct < 50 & Non_Detect_Lvls > 1 ~ as.character(
          "Kaplan-Meier"
        ),
        Non_Detect_Pct < 50 ~ as.character(
          "Robust Regression Order Statistics"
        ),
        Measurement_Count >= 50 ~ as.character("Maximum Likelihood Estimation"),
        Measurement_Count < 50 ~ as.character(
          "Robust Regression Order Statistics"
        )
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "UpperFence",
          "LowerFence",
          "Min",
          "Mean",
          "Max",
          "Percentile_5th",
          "Percentile_10th",
          "Percentile_15th",
          "Percentile_25th",
          "Percentile_50th_Median",
          "Percentile_75th",
          "Percentile_85th",
          "Percentile_95th",
          "Percentile_98th"
        )),
        ~ signif(.x, sig_figs)
      ),
      dplyr::across(
        dplyr::all_of(c("Non_Detect_Pct", "Over_Detect_Pct")),
        ~ round(.x, pct_digits)
      )
    )

  return(StatsTable)
}
