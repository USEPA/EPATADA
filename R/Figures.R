#' Create Boxplot(s)
#'
#' @param .data TADA data frame containing the data downloaded from the
#'   WQP, where each row represents a unique data record. Data frame must
#'   include the columns 'TADA.ComparableDataIdentifier',
#'   'TADA.ResultMeasureValue', and 'TADA.ResultMeasure.MeasureUnitCode' to run
#'   this function. 'TADA.ComparableDataIdentifier' can be added to the data
#'   frame by running the function TADA_HarmonizeSynonyms(). The user can include
#'   additional grouping columns in the id_cols input. If more than one group
#'   exists in the dataset (i.e. two or more unique comparable data
#'   identifiers), the function creates a list of plots, where each list element
#'   name is a unique group identifier.
#'
#' @param id_cols The column(S) in the dataset used to identify the unique groups
#'   to be plotted. Defaults to 'TADA.ComparableDataIdentifier'.
#'
#' @return A list of plotly boxplot figures showing the median, 25th percentile,
#'   75th percentile, upper fence, lower fence, minimum, maximum, and data
#'   outliers for each unique data group.
#'
#' @export
#'
#' @examples
#' # Create a single boxplot using defaults. The input dataframe in this example
#' # includes only one unique TADA.ComparableDataIdentifier:
#' # Load example dataset:
#' data(Data_6Tribes_5y_Harmonized)
#' # Filter data down to a single TADA.ComparableDataIdentifier
#' df <- dplyr::filter(Data_6Tribes_5y_Harmonized, TADA.ComparableDataIdentifier == "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L")
#' # Generate single boxplot
#' TADA_Boxplot(df, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create multiple boxplots with additional grouping columns and view the first
#' # plot in list. In this example, we will group data in the input dataframe
#' # by both the TADA.ComparableDataIdentifier and the OrganizationIdentifier
#' Boxplots_TPbyOrg <- TADA_Boxplot(df, id_cols = c("TADA.ComparableDataIdentifier", "OrganizationIdentifier"))
#' # This example generates 4 box plots.
#' Boxplots_TPbyOrg[[1]]
#' Boxplots_TPbyOrg[[2]]
#' Boxplots_TPbyOrg[[3]]
#'
#' # Create multiple boxplots with additional grouping columns and view the first
#' # plot in list. In this example, we will group data in the input dataframe
#' # by both the TADA.ComparableDataIdentifier and the MonitoringLocationTypeName
#' # (e.g. stream, reservoir, canal, etc.)
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' Boxplot_output <- TADA_Boxplot(Data_Nutrients_UT, id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationTypeName"))
#' # This example generates 45 box plots.
#' Boxplot_output[[2]]
#' Boxplot_output[[25]]
#' Boxplot_output[[40]]
#'
TADA_Boxplot <- function(.data, id_cols = c("TADA.ComparableDataIdentifier")) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }
  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    warning("TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended: plotting without it may produce errors in the plot.")
  }

  # check .data has required columns
  TADA_CheckColumns(.data, id_cols)

  # check .data has required columns
  TADA_CheckColumns(.data, c(
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  ))

  start <- dim(.data)[1]

  .data <- subset(.data, !is.na(.data$TADA.ResultMeasureValue))

  end <- dim(.data)[1]

  if (!start == end) {
    net <- start - end
    print(paste0("Plotting function removed ", net, " results where TADA.ResultMeasureValue = NA. These results cannot be plotted."))
  }

  .data <- .data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::mutate(Group = dplyr::cur_group_id())

  boxplots <- list()

  for (i in 1:max(.data$Group)) {
    plot.data <- subset(.data, .data$Group == i)
    groupid <- paste0(unique(plot.data[, id_cols]), collapse = " ")
    groupid <- gsub("_NA", "", groupid)
    groupid <- gsub("_", " ", groupid)

    # units
    unit <- unique(plot.data$TADA.ResultMeasure.MeasureUnitCode)

    # boxplot stats
    values <- plot.data$TADA.ResultMeasureValue
    # 25th percentile (calculated using "type 7" method which is default for quantile function,
    # but ATSDR tool uses "type 6")
    quant_25 <- signif(stats::quantile(values, 0.25, type = 7), 5)
    # 75th percentile (see note above)
    quant_75 <- signif(stats::quantile(values, 0.75, type = 7), 5)
    # median for box center line
    box_median <- stats::median(values)
    # mean
    box_mean <- mean(values)
    # standard deviation
    box_sd <- stats::sd(values)
    # interquantile range (length of box)
    box_iqr <- quant_75 - quant_25
    # upper threshold for upper whisker
    upper_thresh <- quant_75 + 1.5 * box_iqr
    # lower threshold for lower whisker
    lower_thresh <- quant_25 - 1.5 * box_iqr
    # find max of values below upper threshold
    if (suppressWarnings(is.infinite(max(values[values <= upper_thresh])))) {
      box_upper <- max(values)
    } else {
      box_upper_row <- which(values == max(values[values <= upper_thresh]))
      box_upper <- values[[box_upper_row[[1]]]]
    }
    # find min of values above lower threshold
    if (suppressWarnings(is.infinite(min(values[values >= lower_thresh])))) {
      box_lower <- min(values)
    } else {
      box_lower_row <- which(values == min(values[values >= lower_thresh]))
      box_lower <- values[[box_lower_row[[1]]]]
    }
    # construct plotly boxplot
    base_boxplot <- plotly::plot_ly(
      y = list(values), type = "box",
      q1 = quant_25, median = box_median,
      q3 = quant_75, lowerfence = box_lower,
      hoverinfo = "y",
      upperfence = box_upper, boxpoints = "outliers",
      marker = list(color = "#00bde3"),
      stroke = I("#005ea2")
    )

    # figure margin
    mrg <- list(
      l = 50, r = 20,
      b = 20, t = 55,
      pad = 0
    )

    # boxplot layout and labels
    base_boxplot <- base_boxplot %>%
      plotly::layout(
        xaxis = list(showticklabels = FALSE),
        yaxis = list(
          title = unit, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
          showgrid = FALSE, tickcolor = "black"
        ),
        hoverlabel = list(bgcolor = "white"),
        title = paste0("Boxplot of \n", groupid),
        plot_bgcolor = "#e5ecf6",
        margin = mrg
      ) %>%
      plotly::config(displayModeBar = FALSE)

    # create boxplot for all groupid's
    boxplots[[i]] <- base_boxplot

    names(boxplots)[i] <- groupid
  }

  if (length(boxplots) == 1) {
    boxplots <- boxplots[[1]]
  }

  return(boxplots)
}

#' Create Histogram(s)
#'
#' @param .data TADA data frame containing the data downloaded from the
#'   WQP, where each row represents a unique data record. Data frame must
#'   include the columns 'TADA.ComparableDataIdentifier',
#'   'TADA.ResultMeasureValue', and 'TADA.ResultMeasure.MeasureUnitCode' to run
#'   this function. 'TADA.ComparableDataIdentifier' can be added to the data
#'   frame by running the function TADA_HarmonizeSynonyms(). The user can include
#'   additional grouping columns in the id_cols input. If more than one group
#'   exists in the dataset (i.e. two or more unique comparable data
#'   identifiers), the function creates a list of plots, where each list element
#'   name is a unique group identifier.
#'
#' @param id_cols The column(S) in the dataset used to identify the unique groups
#'   to be plotted. Defaults to 'TADA.ComparableDataIdentifier'.
#'
#' @return A list of plotly histogram figures showing the distribution of sample values
#' for each data group.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_6Tribes_5y_Harmonized)
#'
#' # Create a histogram for each comparable data group (TADA.ComparableDataIdentifier)
#' # in the input dataframe:
#' TADA_Histogram(Data_6Tribes_5y_Harmonized, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create a single histogram using defaults. The input dataframe in this example
#' # is filtered so it includes only one TADA.ComparableDataIdentifier
#' df <- dplyr::filter(Data_6Tribes_5y_Harmonized, TADA.ComparableDataIdentifier == "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L")
#' TADA_Histogram(df, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create multiple histograms with additional grouping columns and view the first
#' # plot in list. In this example, we will group by both TADA.ComparableDataIdentifier
#' # and MonitoringLocationTypeName (e.g. stream, reservoir, canal, etc.)
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' Histogram_output <- TADA_Histogram(Data_Nutrients_UT, id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationTypeName"))
#' # This example generates 45 histograms
#' Histogram_output[[10]]
#' Histogram_output[[25]]
#' Histogram_output[[35]]
#'
TADA_Histogram <- function(.data, id_cols = c("TADA.ComparableDataIdentifier")) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }
  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    warning("TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended: plotting without it may produce errors in the plot.")
  }

  # check .data has required columns
  TADA_CheckColumns(.data, id_cols)

  # check .data has required columns
  TADA_CheckColumns(.data, c(
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  ))

  start <- dim(.data)[1]

  .data <- subset(.data, !is.na(.data$TADA.ResultMeasureValue))

  end <- dim(.data)[1]

  if (!start == end) {
    net <- start - end
    print(paste0("Plotting function removed ", net, " results where TADA.ResultMeasureValue = NA. These results cannot be plotted."))
  }

  .data <- .data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::mutate(Group = dplyr::cur_group_id())

  histograms <- list()

  for (i in 1:max(.data$Group)) {
    plot.data <- subset(.data, .data$Group == i)
    groupid <- paste0(unique(plot.data[, id_cols]), collapse = " ")
    groupid <- gsub("_NA", "", groupid)
    groupid <- gsub("_", " ", groupid)

    # units
    unit <- unique(plot.data$TADA.ResultMeasure.MeasureUnitCode)
    y_label <- "Frequency"

    # histogram stats
    # data for remove_outliers trace
    values <- plot.data$TADA.ResultMeasureValue
    quant_25 <- stats::quantile(values, 0.25, type = 7)
    quant_75 <- stats::quantile(values, 0.75, type = 7)
    box_iqr <- quant_75 - quant_25
    upper_thresh <- quant_75 + 1.5 * box_iqr
    lower_thresh <- quant_25 - 1.5 * box_iqr
    # find max of values below upper threshold
    if (suppressWarnings(is.infinite(max(values[values <= upper_thresh])))) {
      box_upper <- max(values)
    } else {
      box_upper_row <- which(values == max(values[values <= upper_thresh]))
      box_upper <- values[[box_upper_row[[1]]]]
    }
    # find min of values above lower threshold
    if (suppressWarnings(is.infinite(min(values[values >= lower_thresh])))) {
      box_lower <- min(values)
    } else {
      box_lower_row <- which(values == min(values[values >= lower_thresh]))
      box_lower <- values[[box_lower_row[[1]]]]
    }
    no_outliers <- subset(plot.data, plot.data$TADA.ResultMeasureValue >= box_lower & plot.data$TADA.ResultMeasureValue <= box_upper)

    histogram <- plotly::plot_ly() %>%
      plotly::add_histogram(
        x = plot.data$TADA.ResultMeasureValue,
        xbins = list(start = min(plot.data$TADA.ResultMeasureValue)),
        marker = list(color = "#00bde3"),
        stroke = I("#005ea2"),
        bingroup = 1,
        name = "<b>All Data<b>"
      )
    if (dim(no_outliers)[1] > 0) {
      histogram <- histogram %>%
        plotly::add_histogram(
          x = no_outliers$TADA.ResultMeasureValue,
          xbins = list(start = min(plot.data$TADA.ResultMeasureValue)),
          marker = list(color = "#00bde3"),
          stroke = I("#005ea2"),
          bingroup = 1,
          name = paste0("<b>Outliers Removed</b>", "\nUpper Threshold: ", box_upper, "\nLower Threshold: ", box_lower),
          visible = "legendonly"
        )
    }

    mrg <- list(
      l = 50, r = 20,
      b = 20, t = 55,
      pad = 0
    )

    # histogram layout and labels
    histogram <- histogram %>%
      plotly::layout(
        xaxis = list(
          title = unit, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
          showgrid = FALSE, tickcolor = "black"
        ),
        yaxis = list(
          title = y_label, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
          showgrid = FALSE, tickcolor = "black"
        ),
        hoverlabel = list(bgcolor = "white"),
        title = paste0("Histogram of \n", groupid),
        plot_bgcolor = "#e5ecf6",
        barmode = "overlay",
        legend = list(title = list(text = "<b>Select 'Outliers Removed' \nand Deselect 'All Data' \nto View a Subset of the Data<b>")),
        margin = mrg
      ) %>%
      plotly::config(displayModeBar = TRUE)


    histograms[[i]] <- histogram

    names(histograms)[i] <- groupid
  }

  if (length(histograms) == 1) {
    histograms <- histograms[[1]]
  }

  return(histograms)
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
#' a pop-up window with exact counts for measurements (i.e. number of rows),
#' visits (number of unique Activity ID's), and characteristics associated with each site.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example datasets:
#' data(Data_Nutrients_UT)
#' data(Data_NCTCShepherdstown_HUC12)
#' data(Data_6Tribes_5y_Harmonized)
#'
#' # Create maps:
#' TADA_OverviewMap(Data_Nutrients_UT)
#' TADA_OverviewMap(Data_NCTCShepherdstown_HUC12)
#' TADA_OverviewMap(Data_6Tribes_5y_Harmonized)
#' }
#'
TADA_OverviewMap <- function(.data) {
  suppressWarnings({
    # taken from this stackoverflow: https://stackoverflow.com/questions/58505589/circles-in-legend-for-leaflet-map-with-addcirclemarkers-in-r-without-shiny
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5) {
      colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

      return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = "Measurements"))
    }

    sumdat <- .data %>%
      dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>%
      dplyr::summarise("Sample_Count" = length(unique(ResultIdentifier)), "Visit_Count" = length(unique(ActivityStartDate)), "Parameter_Count" = length(unique(TADA.CharacteristicName)), "Organization_Count" = length(unique(OrganizationIdentifier)))

    param_counts <- sort(unique(sumdat$Parameter_Count))
    param_length <- length(param_counts)
    param_diff <- diff(param_counts)

    pt_sizes <- round(stats::quantile(sumdat$Sample_Count, probs = c(0.1, 0.25, 0.5, 0.75)), 0)
    pt_labels <- c(
      paste0("<=", pt_sizes[1]),
      paste0(">", pt_sizes[1]),
      paste0(">", pt_sizes[2]),
      paste0(">", pt_sizes[3]),
      paste0(">", pt_sizes[4])
    )

    sumdat$radius <- 5
    sumdat$radius <- ifelse(sumdat$Sample_Count > pt_sizes[1], 10, sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count > pt_sizes[2], 15, sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count > pt_sizes[3], 20, sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count > pt_sizes[4], 30, sumdat$radius)

    site_size <- data.frame(Sample_n = pt_labels, Point_size = c(5, 10, 15, 20, 30))

    site_legend <- subset(site_size, site_size$Point_size %in% unique(sumdat$radius))

    # set color palette
    # set color palette for small number of characteristics (even intervals, no bins)
    if (length(unique(param_diff)) == 1 & param_length < 10) {
      pal <- leaflet::colorFactor(
        palette = "Blues",
        levels = param_counts
      )
    } else if (length(unique(param_counts)) == 1) {
      pal <- "orange"
    } else {
      # set breaks to occur only at integers for data sets requiring bins
      pretty.breaks <- unique(round(pretty(sumdat$Parameter_Count)))

      pal <- leaflet::colorBin(
        palette = "Blues",
        bins = pretty.breaks
      )
    }

    # create custom fill color function so that data sets with one value for parameter count are displayed correctly
    customFillColor <- function(category, pal) {
      if (length(param_diff > 0)) {
        return(pal(category))
      } else {
        return("#2171b5")
      }
    }


    # Tribal layers will load by default in the overview map, restricted by the bounding box of the current dataset
    # They can be toggled on and off using a button (all layers work together and can't be turned on/off individually).
    # Colors and icons are as discussed previously (orange/tan colors and open triangle icons for points) but can be changed to match HMW if desired.
    bbox <- sf::st_bbox(
      c(
        xmin = min(sumdat$TADA.LongitudeMeasure),
        ymin = min(sumdat$TADA.LatitudeMeasure),
        xmax = max(sumdat$TADA.LongitudeMeasure),
        ymax = max(sumdat$TADA.LatitudeMeasure)
      ),
      crs = sf::st_crs(sumdat)
    )
    vbbox <- bbox %>%
      as.vector()

    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = leaflet::providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      leaflet::clearShapes() %>% # get rid of whatever was there before if loading a second dataset
      leaflet::fitBounds(lng1 = vbbox[1], lat1 = vbbox[2], lng2 = vbbox[3], lat2 = vbbox[4]) %>% # fit to bounds of data in tadat$raw
      leaflet.extras::addResetMapButton() %>% # button to reset to initial zoom and lat/long
      leaflet::addCircleMarkers(
        data = sumdat,
        lng = ~TADA.LongitudeMeasure,
        lat = ~TADA.LatitudeMeasure,
        # sets color of monitoring site circles
        color = "red",
        fillColor = customFillColor(sumdat$Parameter_Count, pal),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1.5,
        radius = sumdat$radius,
        popup = paste0(
          "Site ID: ", sumdat$MonitoringLocationIdentifier,
          "<br> Site Name: ", sumdat$MonitoringLocationName,
          "<br> Measurement Count: ", sumdat$Sample_Count,
          "<br> Visit Count: ", sumdat$Visit_Count,
          "<br> Characteristic Count: ", sumdat$Parameter_Count
        )
      ) %>%
      addLegendCustom(
        colors = "black",
        labels = site_legend$Sample_n, sizes = site_legend$Point_size * 2
      )

    # create conditional map legend
    # create legend for single parameter count value data sets
    if (length(param_diff) == 0) {
      map <- map %>% leaflet::addLegend("bottomright",
        color = "#2171b5", labels = param_counts,
        title = "Characteristics",
        opacity = 0.5
      )
    }
    # create legend for data sets with multiple factors/bins for parameter count
    if (length(param_diff) > 0) {
      map <- map %>% leaflet::addLegend("bottomright",
        pal = pal, values = sumdat$Parameter_Count,
        title = "Characteristics",
        opacity = 0.5
      )
    }

    # TADA_addPolys and TADA_addPoints are in Utilities.R
    map <- TADA_addPolys(map, AKAllotmentsUrl, "Tribes", "Alaska Allotments", bbox)
    map <- TADA_addPolys(map, AmericanIndianUrl, "Tribes", "American Indian", bbox)
    map <- TADA_addPolys(map, OffReservationUrl, "Tribes", "Off Reservation", bbox)
    map <- TADA_addPolys(map, OKTribeUrl, "Tribes", "Oklahoma Tribe", bbox)
    map <- TADA_addPoints(map, AKVillagesUrl, "Tribes", "Alaska Native Villages", bbox)
    map <- TADA_addPoints(map, VATribeUrl, "Tribes", "Virginia Tribe", bbox)
    map <- leaflet::addLayersControl(map,
      overlayGroups = c("Tribes"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

    return(map)
  })
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
#' data(Data_Nutrients_UT)
#'
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter:
#' TADA_FieldValuesPie(Data_Nutrients_UT, field = "TADA.CharacteristicName")
#'
TADA_FieldValuesPie <- function(.data, field = "null", characteristicName = "null") {
  dat <- TADA_FieldValuesTable(.data = .data, field = field, characteristicName = characteristicName)

  dat$Legend <- paste0(dat$Value, " - ", dat$Count, " results")
  dat <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Legend = TADA::TADA_InsertBreaks(Legend))

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



#' Create Scatterplot(s)
#'
#' @param .data TADA data frame containing the data downloaded from the
#'   WQP, where each row represents a unique data record. Data frame must
#'   include the columns 'TADA.ComparableDataIdentifier',
#'   'TADA.ResultMeasureValue', and 'TADA.ResultMeasure.MeasureUnitCode' to run
#'   this function. The 'TADA.ComparableDataIdentifier' column is added to the
#'   data frame automatically when WQP data is retrieved using TADADataRetrieval.
#'   This TADA.ComparableDataIdentifier can be updated to harmonize synonyms
#'   by running the function TADA_HarmonizeSynonyms(). You can also include
#'   additional grouping columns in the id_cols input if desired. If more than
#'   one TADA.ComparableDataIdentifier exists in the dataset, the function
#'   creates a list of plots, where each list element name is a unique
#'   TADA.ComparableDataIdentifier.
#'
#' @param id_cols The column(s) in the dataset used to identify the unique groups
#'   to be plotted. Defaults to 'TADA.ComparableDataIdentifier'.
#'
#' @return A list of plotly scatterplot figures showing the distribution of
#' sample values for each comparable data group (TADA.ComparableDataIdentifier).
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_6Tribes_5y_Harmonized)
#'
#' # Create a scatterplot for each comparable data group (TADA.ComparableDataIdentifier)
#' # in the input dataframe:
#' TADA_Scatterplot(Data_6Tribes_5y_Harmonized, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create a single scatterplot using defaults. The input dataframe in this
#' # example is filtered so it includes only one TADA.ComparableDataIdentifier
#' df <- dplyr::filter(Data_6Tribes_5y_Harmonized, TADA.ComparableDataIdentifier == "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L")
#' TADA_Scatterplot(df, id_cols = "TADA.ComparableDataIdentifier")
#' # Creates a scatterplot for each monitoring location
#' TADA_Scatterplot(df, id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationName"))
#'
#' # Create multiple scatterplots with additional grouping columns and view the first
#' # plot in list. In this example, we will group by both TADA.ComparableDataIdentifier
#' # and MonitoringLocationTypeName (e.g. stream, reservoir, canal, etc.)
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' Scatterplot_output <- TADA_Scatterplot(Data_Nutrients_UT, id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationTypeName"))
#' # This example generates 45 scatterplots
#' Scatterplot_output[[10]]
#' Scatterplot_output[[25]]
#' Scatterplot_output[[35]]
#'
TADA_Scatterplot <- function(.data, id_cols = c("TADA.ComparableDataIdentifier")) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }

  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    warning("TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended: plotting without it may produce errors in the plot.")
  }

  # check .data has required columns
  TADA_CheckColumns(.data, id_cols)

  # check .data has required columns
  TADA_CheckColumns(.data, c(
    "ActivityStartDate",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  ))

  .data <- .data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::mutate(Group = dplyr::cur_group_id())

  all_scatterplots <- list()

  for (i in 1:max(.data$Group)) {
    plot.data <- subset(.data, .data$Group == i)
    groupid <- paste0(unique(plot.data[, id_cols]), collapse = " ")
    groupid <- gsub("_NA", "", groupid)
    groupid <- gsub("_", " ", groupid)

    # units label for y axis
    unit <- unique(plot.data$TADA.ResultMeasure.MeasureUnitCode)
    y_label <- "Activity Start Date"

    # construct plotly scatterplot
    one_scatterplot <- plotly::plot_ly(
      data = plot.data,
      type = "scatter",
      mode = "markers",
      x = plot.data$ActivityStartDate, # currently uses start date only, may want to change to just ActivityStartDateTime in the future, but for now ActivityStartDateTime includes NAs when time is not available. Including ActivityStartDateTime in hover feature instead.
      y = plot.data$TADA.ResultMeasureValue,
      # consider adding color or shapes to make it easier to see sites and/or possible realtive result values
      # color = ~MonitoringLocationName,
      # colors = RColorBrewer::brewer.pal(3, "Set2"),
      marker = list(color = "#00bde3"), # marker color
      stroke = I("#005ea2"), # marker border color
      name = "<b>All Data<b>",
      hoverinfo = "text",
      hovertext = paste(
        "Result:", paste0(plot.data$TADA.ResultMeasureValue, " ", plot.data$TADA.ResultMeasure.MeasureUnitCode), "<br>",
        "Activity Start Date:", plot.data$ActivityStartDate, "<br>",
        "Activity Start Date Time:", plot.data$ActivityStartDateTime, "<br>",
        "Monitoring Location Name:", plot.data$MonitoringLocationName, "<br>",
        "Media:", plot.data$TADA.ActivityMediaName, "<br>",
        "Media Subdivision:", plot.data$ActivityMediaSubdivisionName, "<br>",
        "Result Depth:", paste0(
          plot.data$TADA.ResultDepthHeightMeasure.MeasureValue, " ",
          plot.data$TADA.ResultDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Relative Depth:", plot.data$ActivityRelativeDepthName, "<br>",
        "Activity Depth:", paste0(
          plot.data$TADA.ActivityDepthHeightMeasure.MeasureValue, " ",
          plot.data$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Top Depth:", paste0(
          plot.data$TADA.ActivityTopDepthHeightMeasure.MeasureValue, " ",
          plot.data$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Bottom Depth:", paste0(
          plot.data$TADA.ActivityBottomDepthHeightMeasure.MeasureValue, " ",
          plot.data$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode
        ), "<br>"
      )
    )

    # figure margin
    mrg <- list(
      l = 50, r = 20,
      b = 20, t = 55,
      pad = 0
    )

    # scatterplot layout and labels
    one_scatterplot <- one_scatterplot %>%
      plotly::layout(
        yaxis = list(
          title = unit, titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
          showgrid = FALSE, tickcolor = "black"
        ),
        xaxis = list(
          title = y_label, titlefont = list(size = 16, family = "Arial"), tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
          showgrid = FALSE, tickcolor = "black"
        ),
        hoverlabel = list(bgcolor = "white"),
        title = paste0("Scatterplot of \n", groupid),
        plot_bgcolor = "#e5ecf6",
        margin = mrg
      ) %>%
      # config options https://plotly.com/r/configuration-options/
      plotly::config(displaylogo = FALSE) # , displayModeBar = TRUE) # TRUE makes bar always visible

    # create plot for all groupid's
    all_scatterplots[[i]] <- one_scatterplot

    names(all_scatterplots)[i] <- groupid
  }

  if (length(all_scatterplots) == 1) {
    all_scatterplots <- all_scatterplots[[1]]
  }

  return(all_scatterplots)
}

#' Create A Two-Characteristic Scatterplot
#'
#' @param .data TADA data frame containing the data downloaded from the WQP,
#'   where each row represents a unique data record. Data frame must include the
#'   columns 'TADA.ComparableDataIdentifier', 'TADA.ResultMeasureValue', and 'TADA.ResultMeasure.MeasureUnitCode'
#'   to run this function.
#'
#' @param id_cols The column in the dataset used to identify the unique groups to
#'   be plotted. Defaults to 'TADA.ComparableDataIdentifier', which should be
#'   sufficient for most TADA use cases of this function. This input is flexible,
#'   however, for the specific use case in the TADAShiny app where a user might
#'   create groups based on a concatenation of the comparable data identifier
#'   with other additional grouping variables (e.g. site type, site name, year,
#'   organization, etc.)
#'
#' @param groups A vector of two identifiers from the id_cols column. For
#'   example, if the id_cols is 'TADA.ComparableDataIdentifier', the groups could
#'   be 'DISSOLVED OXYGEN (DO)_NA_NA_UG/L' and 'PH_NA_NA_NA'. These groups will
#'   be specific to your dataset. If the id_cols is 'MonitoringLocationName',
#'   the groups could be 'Upper Red Lake: West' and 'Upper Red Lake: West-Central'.
#'
#' @return A single plotly scatterplot figure with one x-axis (Date/Time) and a
#'   left and right y-axis showing the units of the two characteristic groups
#'   plotted on the same figure area.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_Nutrients_UT)
#' # Create a single scatterplot with two specified groups from TADA.ComparableDataIdentifier
#' TADA_TwoCharacteristicScatterplot(Data_Nutrients_UT, id_cols = "TADA.ComparableDataIdentifier", groups = c("AMMONIA_UNFILTERED_AS N_UG/L", "NITRATE_UNFILTERED_AS N_UG/L"))
#'
#' # Load example dataset:
#' data(Data_6Tribes_5y_Harmonized)
#' # Filter the example data so it includes only one TADA.ComparableDataIdentifier
#' df <- dplyr::filter(Data_6Tribes_5y_Harmonized, TADA.ComparableDataIdentifier == "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L")
#' # Creates a scatterplot including the two specified sites in the same plot:
#' TADA_TwoCharacteristicScatterplot(df, id_cols = "MonitoringLocationName", groups = c("Upper Red Lake: West", "Upper Red Lake: West-Central"))
#'
TADA_TwoCharacteristicScatterplot <- function(.data, id_cols = "TADA.ComparableDataIdentifier", groups) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # check .data has required columns
  TADA_CheckColumns(.data, id_cols)

  # check .data has required columns
  reqcols <- c(
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ActivityStartDate"
  )

  # check .data has required columns
  TADA_CheckColumns(.data, reqcols)

  # if left blank, ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }

  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    print("Note: TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended.")
  }

  # check that groups are in id_cols
  id <- unlist(unique(.data[, id_cols]))
  if (any(!groups %in% id)) {
    stop("The 'groups' vector contains one or more inputs that are not found within your input dataset. Check spelling and try again.")
  }

  depthcols <- names(.data)[grepl("DepthHeightMeasure", names(.data))]
  depthcols <- depthcols[grepl("TADA.", depthcols)]

  plot.data <- as.data.frame(.data)

  # this subset must include all fields included in plot hover below
  plot.data <- subset(plot.data, plot.data[, id_cols] %in% groups)[, c(id_cols, reqcols, depthcols, "ActivityStartDateTime", "MonitoringLocationName", "TADA.ActivityMediaName", "ActivityMediaSubdivisionName", "ActivityRelativeDepthName", "TADA.CharacteristicName", "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText")]
  plot.data$name <- gsub("_NA", "", plot.data[, id_cols])
  plot.data$name <- gsub("_", " ", plot.data$name)

  plot.data <- dplyr::arrange(plot.data, ActivityStartDate)

  param1 <- subset(plot.data, plot.data[, id_cols] %in% groups[1])
  param2 <- subset(plot.data, plot.data[, id_cols] %in% groups[2])

  title <- TADA::TADA_InsertBreaks(
    paste0(
      param1$TADA.CharacteristicName[1],
      " and ",
      param2$TADA.CharacteristicName[1],
      " Over Time"
    ),
    len = 45
  )

  # figure margin
  mrg <- list(
    l = 50, r = 75,
    b = 25, t = 75,
    pad = 0
  )

  scatterplot <- plotly::plot_ly(type = "scatter", mode = "markers") %>%
    plotly::layout(
      xaxis = list(
        # title = "Activity Start Date", # not necessary?
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
        showgrid = FALSE, tickcolor = "black"
      ),
      yaxis = list(
        title = paste0(param1$TADA.CharacteristicName[1], "  ", param1$TADA.ResultMeasure.MeasureUnitCode[1]),
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
        showgrid = FALSE, tickcolor = "black"
      ),
      yaxis2 = list(
        side = "right",
        overlaying = "y",
        title = paste0(param2$TADA.CharacteristicName[1], "  ", param2$TADA.ResultMeasure.MeasureUnitCode[1]),
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r", linecolor = "black", rangemode = "tozero",
        showgrid = FALSE, tickcolor = "black"
      ),
      hoverlabel = list(bgcolor = "white"),
      title = title,
      plot_bgcolor = "#e5ecf6",
      margin = mrg,
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2
      )
    ) %>%
    # config options https://plotly.com/r/configuration-options/
    plotly::config(displaylogo = FALSE) %>% # , displayModeBar = TRUE) # TRUE makes bar always visible
    plotly::add_trace(
      data = param1,
      x = ~ as.Date(ActivityStartDate),
      y = ~TADA.ResultMeasureValue,
      name = paste0(
        param1$TADA.ResultSampleFractionText, " ",
        param1$TADA.CharacteristicName, " ",
        param1$TADA.MethodSpeciationName
      ),
      marker = list(
        size = 10,
        color = "#E34234",
        line = list(color = "#005ea2", width = 2)
      ),
      hoverinfo = "text",
      hovertext = paste(
        "Result:", paste0(param1$TADA.ResultMeasureValue, " ", param1$TADA.ResultMeasure.MeasureUnitCode), "<br>",
        "Activity Start Date:", param1$ActivityStartDate, "<br>",
        "Activity Start Date Time:", param1$ActivityStartDateTime, "<br>",
        "Monitoring Location Name:", param1$MonitoringLocationName, "<br>",
        "Media:", param1$TADA.ActivityMediaName, "<br>",
        "Media Subdivision:", param1$ActivityMediaSubdivisionName, "<br>",
        "Result Depth:", paste0(
          param1$TADA.ResultDepthHeightMeasure.MeasureValue, " ",
          param1$TADA.ResultDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Relative Depth Name:", param1$ActivityRelativeDepthName, "<br>",
        "Activity Depth:", paste0(
          param1$TADA.ActivityDepthHeightMeasure.MeasureValue, " ",
          param1$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Top Depth:", paste0(
          param1$TADA.ActivityTopDepthHeightMeasure.MeasureValue, " ",
          param1$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Bottom Depth:", paste0(
          param1$TADA.ActivityBottomDepthHeightMeasure.MeasureValue, " ",
          param1$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode
        ), "<br>"
      )
    ) %>%
    plotly::add_trace(
      data = param2,
      x = ~ as.Date(ActivityStartDate),
      y = ~TADA.ResultMeasureValue,
      name = paste0(
        param2$TADA.ResultSampleFractionText, " ",
        param2$TADA.CharacteristicName, " ",
        param2$TADA.MethodSpeciationName
      ),
      marker = list(
        size = 10, color = "#00bde3",
        line = list(color = "#005ea2", width = 2)
      ),
      yaxis = "y2",
      hoverinfo = "text",
      hovertext = paste(
        "Result:", paste0(param2$TADA.ResultMeasureValue, " ", param2$TADA.ResultMeasure.MeasureUnitCode), "<br>",
        "Activity Start Date:", param2$ActivityStartDate, "<br>",
        "Activity Start Date Time:", param2$ActivityStartDateTime, "<br>",
        "Monitoring Location Name:", param2$MonitoringLocationName, "<br>",
        "Media:", param2$TADA.ActivityMediaName, "<br>",
        "Media Subdivision:", param2$ActivityMediaSubdivisionName, "<br>",
        "Result Depth:", paste0(
          param2$TADA.ResultDepthHeightMeasure.MeasureValue, " ",
          param2$TADA.ResultDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Relative Depth Name:", param2$ActivityRelativeDepthName, "<br>",
        "Activity Depth:", paste0(
          param2$TADA.ActivityDepthHeightMeasure.MeasureValue, " ",
          param2$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Top Depth:", paste0(
          param2$TADA.ActivityTopDepthHeightMeasure.MeasureValue, " ",
          param2$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode
        ), "<br>",
        "Activity Bottom Depth:", paste0(
          param2$TADA.ActivityBottomDepthHeightMeasure.MeasureValue, " ",
          param2$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode
        ), "<br>"
      )
    )

  return(scatterplot)
}