#' Create Boxplot
#' 
#' @param filtered.data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record. Data frame must include the columns 'TADA.CharacteristicName' OR
#' 'TADA.ComparableDataIdentifier', 'TADA.ResultMeasureValue', and
#' 'TADA.ResultMeasure.MeasureUnitCode' to run this function. 'TADA.ComparableDataIdentifier' can 
#' be added to the data frame by running the function HarmonizeData(). The data frame
#' must be filtered down to a single characteristic with a consistent unit or comparable data identifier to run this function.
#' 
#' @param id_col The column in the dataset used to identify the characteristic plotted in the boxplot. May be set to "TADA.CharacteristicName" or "TADA.ComparableDataIdentifier" if user would rather use a post-harmonization dataset that groups multiple TADA.CharacteristicNames into one. 
#' 
#' @return A plotly boxplot figure showing the median, 25th percentile, 75th percentile, 
#' upper fence, lower fence, minimum, maximum, and data outliers for the given 
#' characteristic or comparable data identifier.
#' 
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data("TADAProfileClean18_TNonly")
#' # TADAProfileClean18_TNonly dataframe is clean, harmonized, and filtered
#' # down to one Comparable Data Identifier
#' 
#' # Create boxplot:
#' TADA_boxplot(TADAProfileClean18_TNonly, id_col = "TADA.ComparableDataIdentifier")
#' 

TADA_boxplot <- function(filtered.data, id_col = c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")) {
  # check .data is data.frame
  checkType(filtered.data, "data.frame", "Input object")
  # check id_col matches one of the options
  id_col <- match.arg(id_col)
  # check .data has required columns
  checkColumns(filtered.data, id_col)
  # check .data has required columns
  checkColumns(filtered.data, c("TADA.ResultMeasureValue", "TADA.ResultMeasure.MeasureUnitCode"))
  # check id_col is filtered to one characteristic or identifier
  if (length(unique(filtered.data[,id_col])) > 1) {
    stop(paste0(id_col, " field contains more than one unique value. Boxplot function cannot run with more than 1 unique characteristic or comparable data identifier. Please filter dataframe and rerun function."))
  }
  # check that units are the same across all data points
  if (length(unique(filtered.data$TADA.ResultMeasure.MeasureUnitCode)) > 1) {
    warning("Dataset contains more than one result unit. Plotting results with multiple units in one boxplot is not advised. Please filter or harmonize dataframe and rerun function.")
  }
  
  # execute function after checks have passed

  # units
  unit <- unique(filtered.data$TADA.ResultMeasure.MeasureUnitCode)
  char <- unique(filtered.data[,id_col])
  y_label <- paste0(char, " (", unit, ")")
  
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
                                  marker = list(color = "#00bde3"),
                                  stroke = I("#005ea2"))
  
  # boxplot layout and labels
  base_boxplot <- base_boxplot %>% 
    plotly::layout(
      xaxis = list(showticklabels = FALSE),
      yaxis = list(title = y_label, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
                   hoverformat = ',.4r', linecolor = "black", rangemode = 'tozero', 
                   showgrid = FALSE, tickcolor= "black"),
      hoverlabel=list(bgcolor="white"),
      title = paste0("Boxplot of ", char, " (", unit, ")"), 
      plot_bgcolor = "#e5ecf6"
    ) %>% 
    plotly::config(displayModeBar = FALSE)
  
  return(base_boxplot)
}

#' Create Histogram
#' 
#' @param filtered.data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record. Data frame must include the columns 'TADA.CharacteristicName' OR
#' 'TADA.ComparableDataIdentifier', 'TADA.ResultMeasureValue', and
#' 'TADA.ResultMeasure.MeasureUnitCode' to run this function. 'TADA.ComparableDataIdentifier' can  
#' be added to the data frame by running the function HarmonizeData(). The data frame
#' must be filtered down to a single characteristic with a consistent unit or comparable data identifier to run this function.
#' 
#' @param id_col The column in the dataset used to identify the characteristic plotted in the histogram. May be set to "TADA.CharacteristicName" or "TADA.ComparableDataIdentifier" if user would rather use a post-harmonization dataset that groups multiple TADA.CharacteristicNames into one. 
#' 
#' @return A plotly histogram figure showing the distribution of sample values 
#' for the given characteristic or comparable data identifier.
#' 
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data("TADAProfileClean18_TNonly")
#' # TADAProfileClean18_TNonly dataframe is clean, harmonized, and filtered
#' # down to one Comparable Data Identifier
#' 
#' # Create histogram:
#' TADA_hist(TADAProfileClean18_TNonly, id_col = "TADA.ComparableDataIdentifier")

TADA_hist <- function(filtered.data, id_col = c("TADA.CharacteristicName", "TADA.ComparableDataIdentifier")) {
  # check .data is data.frame
  checkType(filtered.data, "data.frame", "Input object")
  # check id_col matches one of the options
  id_col <- match.arg(id_col)
  # check .data has required columns
  checkColumns(filtered.data, id_col)
  checkColumns(filtered.data, c("TADA.ResultMeasureValue", "TADA.ResultMeasure.MeasureUnitCode"))
  # check id_col is filtered to one characteristic or identifier
  if (length(unique(filtered.data[,id_col])) > 1) {
    stop(paste0(id_col, " field contains more than one unique value. Histogram function cannot run with more than 1 unique characteristic or comparable data identifier. Please filter dataframe and rerun function."))
  }
  # check that units are the same across all data points
  if (length(unique(filtered.data$TADA.ResultMeasure.MeasureUnitCode)) > 1) {
    warning("Dataset contains more than one result unit. Plotting results with multiple units in one histogram is not advised. Please filter or harmonize dataframe and rerun function.")
  }
  
  # execute function after checks have passed
  
  # units
  unit <- unique(filtered.data$TADA.ResultMeasure.MeasureUnitCode)
  char <- unique(filtered.data[,id_col])
  x_label <- paste0(char, " (", unit, ")")
  y_label <- paste0("Frequency
  (Total of ", nrow(filtered.data), " Samples)")
  
  # data for all_data trace
  all_data <- filtered.data
  # data for remove_outliers trace
  values <- filtered.data$TADA.ResultMeasureValue
  quant_25 <- stats::quantile(all_data$TADA.ResultMeasureValue, 0.25, type = 7)
  quant_75 <- stats::quantile(all_data$TADA.ResultMeasureValue, 0.75, type = 7)
  box_iqr <- quant_75 - quant_25
  upper_thresh <- quant_75 + 1.5*box_iqr
  lower_thresh <- quant_25 - 1.5*box_iqr
  box_upper_row <- which(values == max(values[values <= upper_thresh]))
  box_upper <- values[[box_upper_row[[1]]]]
  box_lower_row <- which(values == min(values[values >= lower_thresh]))
  box_lower <- values[[box_lower_row[[1]]]]
  no_outliers <- subset(filtered.data, filtered.data$TADA.ResultMeasureValue>=box_lower & filtered.data$TADA.ResultMeasureValue<=box_upper)
  
  histogram <- plotly::plot_ly() %>%
    plotly::add_histogram(x = all_data$TADA.ResultMeasureValue,
              xbins = list(start = min(all_data$TADA.ResultMeasureValue)),
              marker = list(color = "#00bde3"),
              stroke = I("#005ea2"),
              bingroup = 1,
              name = "<b>All Data<b>"
              ) %>%
    plotly::add_histogram(x = no_outliers$TADA.ResultMeasureValue,
              xbins = list(start = min(all_data$TADA.ResultMeasureValue)),
              marker = list(color = "#00bde3"),
              stroke = I("#005ea2"),
              bingroup = 1,
              name = paste0("<b>Outliers Removed</b>", "\nUpper Threshold: ", box_upper, "\nLower Threshold: ", box_lower),
              visible = "legendonly"
              )
  
  # histogram layout and labels
  histogram <- histogram %>% 
  plotly::layout(
    xaxis = list(title = x_label, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
                 hoverformat = ',.4r', linecolor = "black", rangemode = 'tozero', 
                 showgrid = FALSE, tickcolor= "black"),
    yaxis = list(title = y_label, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
                 hoverformat = ',.4r', linecolor = "black", rangemode = 'tozero', 
                 showgrid = FALSE, tickcolor= "black"), 
    hoverlabel=list(bgcolor="white"),
    title = paste0(char, " vs. Frequency"), 
    plot_bgcolor = "#e5ecf6",
    barmode = "overlay",
    legend = list(title = list(text = "<b>Select 'Outliers Removed' \nand Deselect 'All Data' \nto View a Subset of the Data<b>"))
  ) %>% 
    plotly::config(displayModeBar = TRUE)
  
  return(histogram)
}

#' Create Overview Map
#' 
#' @param .data TADA data frame containing the data downloaded from the WQP, where
#' each row represents a unique data record. Data frame must include the columns
#' 'MonitoringLocationIdentifier','MonitoringLocationName','TADA.LatitudeMeasure', 
#' 'TADA.LongitudeMeasure', 'ResultIdentifier', 'ActivityStartDate', 'TADA.CharacteristicName',
#' and 'OrganizationIdentifier' to run this function. 
#' 
#' @return A leaflet map that shows all sites in the data frame, where larger point sizes
#' indicate more results collected at a site, and darker point colors indicate more
#' characteristics measured at that site. Users can click on points on the map to see
#' a pop-up window with exact counts for results, characteristics, and organizations
#' associated with each site.
#' 
#' @export
#' 
#' @examples
#' # Load example dataset:
#' data("Nutrients_Utah.rda")
#' 
#' # Create map:
#' TADAOverviewMap(Nutrients_Utah)
#' 

TADAOverviewMap <- function(.data){
  sumdat = .data%>%dplyr::group_by(MonitoringLocationIdentifier,MonitoringLocationName,TADA.LatitudeMeasure, TADA.LongitudeMeasure)%>%dplyr::summarise("Sample_Count" = length(unique(ResultIdentifier)), "Visit_Count" = length(unique(ActivityStartDate)), "Parameter_Count" = length(unique(TADA.CharacteristicName)), "Organization_Count" = length(unique(OrganizationIdentifier)))
  sumdat$radius = 3
  sumdat$radius = ifelse(sumdat$Sample_Count>10,5,sumdat$radius)
  sumdat$radius = ifelse(sumdat$Sample_Count>50,8,sumdat$radius)
  sumdat$radius = ifelse(sumdat$Sample_Count>100,10,sumdat$radius)
  sumdat$radius = ifelse(sumdat$Sample_Count>200,15,sumdat$radius)
  sumdat$radius = ifelse(sumdat$Sample_Count>500,20,sumdat$radius)
  sumdat$radius = ifelse(sumdat$Sample_Count>1500,30,sumdat$radius)
  
  pal <- leaflet::colorBin(
    palette = "Blues",
    domain = sumdat$Parameter_Count)
  map = leaflet::leaflet()%>%
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
    leaflet::clearShapes()%>% # get rid of whatever was there before if loading a second dataset
    leaflet::fitBounds(lng1 = min(sumdat$TADA.LongitudeMeasure), lat1 = min(sumdat$TADA.LatitudeMeasure), lng2 = max(sumdat$TADA.LongitudeMeasure), lat2 = max(sumdat$TADA.LatitudeMeasure))%>% # fit to bounds of data in tadat$raw
    leaflet::addCircleMarkers(data = sumdat, lng=~TADA.LongitudeMeasure, lat=~TADA.LatitudeMeasure, color="black",fillColor=~pal(Parameter_Count), fillOpacity = 0.7, stroke = TRUE, weight = 1.5, radius=sumdat$radius,
                              popup = paste0("Site ID: ", sumdat$MonitoringLocationIdentifier,
                                             "<br> Site Name: ", sumdat$MonitoringLocationName,
                                             "<br> Sample Count: ", sumdat$Sample_Count,
                                             "<br> Visit Count: ", sumdat$Visit_Count,
                                             "<br> Parameter Count: ", sumdat$Parameter_Count))%>%
    leaflet::addLegend("bottomright", pal = pal, values =sumdat$Parameter_Count,
                       title = "Characteristics",
                       opacity = 0.5
    )
  return(map)
}

#' Field Values Pie Chart
#'
#' Function creates a ggplot2 pie chart showing the relative proportions of values in a given field in a TADA dataset.
#' 
#' @param .data TADA dataframe
#' @param field The field (column) the user would like to see represented in a pie chart.
#' @param characteristicName Optional. Defaults to "null". A vector of TADA-converted (all caps) WQP characteristics a user may provide to filter the results to one or more characteristics of interest. "null" will show a summary table for the whole dataset.
#'
#' @return A ggplot2 pie chart.
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Nutrients_Utah)
#' 
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter: 
#' fieldValuesPie(Nutrients_Utah, field = "TADA.CharacteristicName")
#' 


fieldValuesPie <- function(.data,field="null",characteristicName="null"){
  
  dat = fieldValuesTable(.data = .data, field = field, characteristicName = characteristicName)
  
  dat$Value = dat[,field]
  dat$Legend = paste0(dat$Value, " - ", dat$Count, " results")
  
  # define number of colors required for pie chart
  colorCount <- length(unique(dat$Legend))
  
  # define color palette
  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
  
  # create pie chart
  pie <- ggplot2::ggplot(dat, ggplot2::aes(x = "", y = Count, fill = Legend)) +
    ggplot2::scale_fill_manual(values = getPalette(colorCount), name = field) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() 
  
  return(pie)
}