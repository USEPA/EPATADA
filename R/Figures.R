#' Create Boxplot
#' 
#' @param filtered.data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record. Data frame must include the columns
#' 'TADA.ComparableDataIdentifier", 'TADA.ResultMeasureValue', and
#' 'TADA.ResultMeasure.MeasureUnitCode' to run this function. These columns can 
#' be added to the data frame by running the function HarmonizeData(). The data frame
#' must be filtered down to a single comparable data identifier to run this function.
#' 
#' @return A plotly boxplot figure showing the median, 25th percentile, 75th percentile, 
#' upper fence, lower fence, minimum, maximum, and data outliers for the given 
#' comparable data identifier.
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
#' TADA_boxplot(TADAProfileClean18_TNonly)
#' 

TADA_boxplot <- function(filtered.data) {
  # check .data is data.frame
  checkType(filtered.data, "data.frame", "Input object")
  # check .data has required columns
  checkColumns(filtered.data, c("TADA.ComparableDataIdentifier", "TADA.ResultMeasureValue", "TADA.ResultMeasure.MeasureUnitCode"))
  # check TADA.ComparableDataIdentifier column is filtered to one identifier
  if (length(unique(filtered.data$TADA.ComparableDataIdentifier)) > 1) {
    warning("Boxplot function cannot run with more than 1 comparable data identifier. Please filter dataframe and rerun function.")
  }
  
  # execute function after checks have passed

  # units
  unit <- unique(filtered.data$TADA.ResultMeasure.MeasureUnitCode)
  y_label <- paste0("Level (", unit, ")")
  
  # boxplot stats
  values <- filtered.data$TADA.ResultMeasureValue
  # 25th percentile (calculated using "type 7" method which is default for quantile function,
  # but ATSDR tool uses "type 6")
  quant_25 <- signif(stats::quantile(values, 0.25, type = 7), 5)
  # 75th percentile (see note above)
  quant_75 <- signif(stats::quantile(values, 0.75, type = 7), 5)
  # median for box center line
  box_median <- median(values)
  # mean
  box_mean <- mean(values)
  # standard deviation
  box_sd <- sd(values)
  # interquantile range (length of box)
  box_iqr <- quant_75 - quant_25
  # upper threshold for upper whisker
  upper_thresh <- quant_75 + 1.5*box_iqr
  # lower threshold for lower whisker
  lower_thresh <- quant_25 - 1.5*box_iqr
  # find max of values below upper threshold
  box_upper_row <- which(values == max(values[values <= upper_thresh]))
  box_upper <- values[[box_upper_row[[1]]]]
  # find min of values above lower threshold
  box_lower_row <- which(values == min(values[values >= lower_thresh]))
  box_lower <- values[[box_lower_row[[1]]]]
  
  # construct plotly boxplot
  base_boxplot <- plotly::plot_ly(y = list(values), type = "box", 
                                  q1 = quant_25, median = box_median,
                                  q3 = quant_75, lowerfence = box_lower,
                                  hoverinfo = 'y',
                                  upperfence = box_upper, boxpoints = "outliers",
                                  marker = list(color = "#7cb5ec", fill = "#c0e9ff"),
                                  color = I("#c0e9ff"),
                                  stroke = I("#7cb5ec"))
  
  # boxplot layout and labels
  base_boxplot <- base_boxplot %>% 
    plotly::layout(
      xaxis = list(showticklabels = FALSE),
      yaxis = list(title = y_label, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
                   hoverformat = ',.4r', linecolor = "black", rangemode = 'tozero', 
                   showgrid = FALSE, tickcolor= "black"),
      hoverlabel=list(bgcolor="white")
    ) %>% 
    plotly::config(displayModeBar = FALSE)
  
  return(base_boxplot)
}