#' Create Boxplot(s)
#'
#' @param .data TADA dataframe containing the data downloaded from the
#'   WQP, where each row represents a unique data record. dataframe must
#'   include the columns 'TADA.ComparableDataIdentifier',
#'   'TADA.ResultMeasureValue', and 'TADA.ResultMeasure.MeasureUnitCode' to run
#'   this function. 'TADA.ComparableDataIdentifier' can be added to the data
#'   frame by running the function TADA_HarmonizeSynonyms(). The user can include
#'   additional grouping columns in the id_cols input. If more than one group
#'   exists in the dataframe (i.e. two or more unique comparable data
#'   identifiers), the function creates a list of plots, where each list element
#'   name is a unique group identifier.
#'
#' @param id_cols The column(S) in the dataframe used to identify the unique groups
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
#' # Load example dataframe:
#' utils::data(Data_6Tribes_5y_Harmonized)
#' # Filter data down to a single TADA.ComparableDataIdentifier
#' df <- dplyr::filter(
#'   Data_6Tribes_5y_Harmonized,
#'   TADA.ComparableDataIdentifier ==
#'     "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L"
#' )
#' # Generate single boxplot
#' TADA_Boxplot(df, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create multiple boxplots with additional grouping columns and view the first
#' # plot in list. In this example, we will group data in the input dataframe
#' # by both the TADA.ComparableDataIdentifier and the OrganizationIdentifier
#' Boxplots_TPbyOrg <- TADA_Boxplot(df,
#'   id_cols =
#'     c("TADA.ComparableDataIdentifier", "OrganizationIdentifier")
#' )
#' # This example generates 2 box plots.
#' Boxplots_TPbyOrg[[1]]
#' Boxplots_TPbyOrg[[2]]
#'
#' # Create multiple boxplots with additional grouping columns and view the first
#' # plot in list. In this example, we will group data in the input dataframe
#' # by both the TADA.ComparableDataIdentifier and the MonitoringLocationTypeName
#' # (e.g. stream, reservoir, canal, etc.)
#' # Load example dataframe:
#' utils::data(Data_Nutrients_UT)
#' Boxplot_output <- TADA_Boxplot(Data_Nutrients_UT,
#'   id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationTypeName")
#' )
#' # This example generates 32 box plots.
#' Boxplot_output[[2]]
#' Boxplot_output[[25]]
#' Boxplot_output[[30]]
#'
TADA_Boxplot <- function(.data, id_cols = c("TADA.ComparableDataIdentifier")) {
  # ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }
  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    warning(
      "TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended: plotting without it may produce errors in the plot."
    )
  }

  # check .data is data.frame and has required columns (including id_cols)
  expected_cols <- c(
    id_cols,
    c("TADA.ResultMeasureValue", "TADA.ResultMeasure.MeasureUnitCode")
  )
  TADA_CheckColumns(.data, expected_cols)

  # load TADA color palette

  tada.pal <- TADA_ColorPalette(col_pair = TRUE)

  start <- dim(.data)[1]

  .data <- subset(.data, !is.na(.data$TADA.ResultMeasureValue))

  end <- dim(.data)[1]

  if (!start == end) {
    net <- start - end
    print(paste0(
      "Plotting function removed ",
      net,
      " results where TADA.ResultMeasureValue = NA. These results cannot be plotted."
    ))
  }

  .data <- .data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::mutate(Group = dplyr::cur_group_id())

  boxplots <- list()

  for (i in 1:max(.data$Group)) {
    plot.data <- subset(.data, .data$Group == i)
    groupid <- TADA_CharStringRemoveNA(paste0(
      unique(plot.data[, id_cols]),
      collapse = " "
    ))

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

    base_boxplot <- plotly::plot_ly(
      y = list(values),
      type = "box",
      fillcolor = tada.pal[1, 1],
      q1 = quant_25,
      median = box_median,
      q3 = quant_75,
      lowerfence = box_lower,
      hoverinfo = "y",
      upperfence = box_upper,
      boxpoints = "outliers",
      marker = list(color = tada.pal[1, 1]),
      stroke = I(tada.pal[1, 2])
    )

    # figure margin
    mrg <- list(l = 50, r = 20, b = 20, t = 55, pad = 0)

    # boxplot layout and labels
    base_boxplot <- base_boxplot |>
      plotly::layout(
        xaxis = list(showticklabels = FALSE),
        yaxis = list(
          title = unit,
          titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r",
          linecolor = "black",
          rangemode = "tozero",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        hoverlabel = list(bgcolor = "white"),
        title = paste0("Boxplot of \n", groupid),
        plot_bgcolor = "#e5ecf6",
        margin = mrg
      ) |>
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
#' @param .data TADA dataframe containing the data downloaded from the
#'   WQP, where each row represents a unique data record. Dataframe must
#'   include the columns 'TADA.ComparableDataIdentifier',
#'   'TADA.ResultMeasureValue', and 'TADA.ResultMeasure.MeasureUnitCode' to run
#'   this function. 'TADA.ComparableDataIdentifier' can be added to the data
#'   frame by running the function TADA_HarmonizeSynonyms(). The user can include
#'   additional grouping columns in the id_cols input. If more than one group
#'   exists in the dataframe (i.e. two or more unique comparable data
#'   identifiers), the function creates a list of plots, where each list element
#'   name is a unique group identifier.
#'
#' @param id_cols The column(S) in the dataframe used to identify the unique groups
#'   to be plotted. Defaults to 'TADA.ComparableDataIdentifier'.
#'
#' @return A list of plotly histogram figures showing the distribution of sample values
#' for each data group.
#'
#' @export
#'
#' @examples
#' # Load example dataframe:
#' utils::data(Data_6Tribes_5y_Harmonized)
#'
#' # Create a histogram for each comparable data group (TADA.ComparableDataIdentifier)
#' # in the input dataframe:
#' TADA_Histogram(Data_6Tribes_5y_Harmonized, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create a single histogram using defaults. The input dataframe in this example
#' # is filtered so it includes only one TADA.ComparableDataIdentifier
#' df <- dplyr::filter(
#'   Data_6Tribes_5y_Harmonized,
#'   TADA.ComparableDataIdentifier ==
#'     "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L"
#' )
#' TADA_Histogram(df, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create multiple histograms with additional grouping columns and view the first
#' # plot in list. In this example, we will group by both TADA.ComparableDataIdentifier
#' # and MonitoringLocationTypeName (e.g. stream, reservoir, canal, etc.)
#' # Load example dataframe:
#' utils::data(Data_Nutrients_UT)
#' Histogram_output <- TADA_Histogram(Data_Nutrients_UT,
#'   id_cols = c(
#'     "TADA.ComparableDataIdentifier",
#'     "MonitoringLocationTypeName"
#'   )
#' )
#' # This example generates 32 histograms
#' Histogram_output[[10]]
#' Histogram_output[[25]]
#' Histogram_output[[30]]
#'
TADA_Histogram <- function(
  .data,
  id_cols = c("TADA.ComparableDataIdentifier")
) {
  # ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }
  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    warning(
      "TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended: plotting without it may produce errors in the plot."
    )
  }

  # check .data is data.frame and has required columns (including id_cols)
  expected_cols <- c(
    id_cols,
    c("TADA.ResultMeasureValue", "TADA.ResultMeasure.MeasureUnitCode")
  )
  TADA_CheckColumns(.data, expected_cols)

  tada.pal <- TADA_ColorPalette(col_pair = TRUE)

  start <- dim(.data)[1]

  .data <- subset(.data, !is.na(.data$TADA.ResultMeasureValue))

  end <- dim(.data)[1]

  if (!start == end) {
    net <- start - end
    print(paste0(
      "Plotting function removed ",
      net,
      " results where TADA.ResultMeasureValue = NA. These results cannot be plotted."
    ))
  }

  .data <- .data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::mutate(Group = dplyr::cur_group_id())

  histograms <- list()

  for (i in 1:max(.data$Group)) {
    plot.data <- subset(.data, .data$Group == i)
    groupid <- TADA_CharStringRemoveNA(paste0(
      unique(plot.data[, id_cols]),
      collapse = " "
    ))

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
    no_outliers <- subset(
      plot.data,
      plot.data$TADA.ResultMeasureValue >= box_lower &
        plot.data$TADA.ResultMeasureValue <= box_upper
    )

    histogram <- plotly::plot_ly() |>
      plotly::add_histogram(
        x = plot.data$TADA.ResultMeasureValue,
        xbins = list(start = min(plot.data$TADA.ResultMeasureValue)),
        marker = list(color = tada.pal[1, 1]),
        stroke = I(tada.pal[1, 2]),
        bingroup = 1,
        name = "<b>All Data<b>"
      )
    if (dim(no_outliers)[1] > 0) {
      histogram <- histogram |>
        plotly::add_histogram(
          x = no_outliers$TADA.ResultMeasureValue,
          xbins = list(start = min(plot.data$TADA.ResultMeasureValue)),
          marker = list(color = tada.pal[1, 1]),
          stroke = I(tada.pal[1, 2]),
          bingroup = 1,
          name = paste0(
            "<b>Outliers Removed</b>",
            "\nUpper Threshold: ",
            box_upper,
            "\nLower Threshold: ",
            box_lower
          ),
          visible = "legendonly"
        )
    }

    mrg <- list(l = 50, r = 20, b = 20, t = 55, pad = 0)

    # histogram layout and labels
    histogram <- histogram |>
      plotly::layout(
        xaxis = list(
          title = unit,
          titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r",
          linecolor = "black",
          rangemode = "tozero",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        yaxis = list(
          title = y_label,
          titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r",
          linecolor = "black",
          rangemode = "tozero",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        hoverlabel = list(bgcolor = "white"),
        title = paste0("Histogram of \n", groupid),
        plot_bgcolor = "#e5ecf6",
        barmode = "overlay",
        legend = list(
          title = list(
            text = "<b>Select 'Outliers Removed' \nand Deselect 'All Data' \nto View a Subset of the Data<b>"
          )
        ),
        margin = mrg
      ) |>
      plotly::config(displayModeBar = TRUE)

    histograms[[i]] <- histogram

    names(histograms)[i] <- groupid
  }

  if (length(histograms) == 1) {
    histograms <- histograms[[1]]
  }

  return(histograms)
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
#' utils::data(Data_Nutrients_UT)
#'
#' # Create a list of parameters in the dataset and the number of records of
#' # each parameter:
#' TADA_FieldValuesPie(Data_Nutrients_UT, field = "TADA.CharacteristicName")
#' # If there are more than 12 categories to display, any remaining categories,
#' # the ones that have the smallest number of results,
#' # are combined into an "ALL OTHERS" category.
#' TADA_FieldValuesPie(Data_Nutrients_UT,
#'   field =
#'     "TADA.ComparableDataIdentifier"
#' )
#'
TADA_FieldValuesPie <- function(
  .data,
  field = "null",
  characteristicName = "null"
) {
  dat <- utils::head(
    TADA_FieldValuesTable(
      .data = .data,
      field = field,
      characteristicName = characteristicName
    ),
    12
  )
  # If data set contains more than 12 categories, dat2 aggregates 'all other' categories into its own row.
  dat2 <- data.frame(
    Value = "ALL OTHERS",
    Count = sum(TADA_FieldValuesTable(
      .data = .data,
      field = field,
      characteristicName = characteristicName
    )[-(1:12), ][2])
  )

  dat$Legend <- paste0(dat$Value, " - ", dat$Count, " results")
  dat2$Legend <- paste0(dat2$Value, " - ", dat2$Count, " results")
  dat <- dat |>
    dplyr::rowwise() |>
    dplyr::mutate(Legend = stringr::str_wrap(Legend, width = 50))

  # Only apply the all others category if there are greater than 12 categories to display.
  if (dat2$Count != 0) {
    dat <- dplyr::bind_rows(dat, dat2)
  }

  # create TADA color palette
  tada.pal <- TADA_ColorPalette()

  # define number of colors required for pie chart
  colorCount <- length(unique(dat$Legend))

  if (colorCount < 15) {
    tada.pal <- c(
      tada.pal[3],
      tada.pal[5],
      tada.pal[6],
      tada.pal[8],
      tada.pal[9],
      tada.pal[10],
      tada.pal[14],
      tada.pal[12],
      tada.pal[15],
      tada.pal[4],
      tada.pal[7],
      tada.pal[13],
      tada.pal[2],
      tada.pal[11]
    )

    tada.pal <- tada.pal[2:(1 + colorCount)]
  }

  if (colorCount > 14) {
    getPalette <- grDevices::colorRampPalette(tada.pal)(1 + colorCount)

    tada.pal <- getPalette[2:(1 + colorCount)]
  }

  # create pie chart
  pie <- ggplot2::ggplot(dat, ggplot2::aes(x = "", y = Count, fill = Legend)) +
    ggplot2::scale_fill_manual(values = tada.pal, name = field) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void()

  return(pie)
}


#' Create Scatterplot(s)
#'
#' @param .data TADA dataframe containing the data downloaded from the
#'   WQP, where each row represents a unique data record. Dataframe must
#'   include the columns 'TADA.ComparableDataIdentifier',
#'   'TADA.ResultMeasureValue', and 'TADA.ResultMeasure.MeasureUnitCode' to run
#'   this function. The 'TADA.ComparableDataIdentifier' column is added to the
#'   dataframe automatically when WQP data is retrieved using TADADataRetrieval.
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
#' utils::data(Data_6Tribes_5y_Harmonized)
#'
#' # Create a scatterplot for each comparable data group (TADA.ComparableDataIdentifier)
#' # in the input dataframe:
#' TADA_Scatterplot(Data_6Tribes_5y_Harmonized, id_cols = "TADA.ComparableDataIdentifier")
#'
#' # Create a single scatterplot using defaults. The input dataframe in this
#' # example is filtered so it includes only one TADA.ComparableDataIdentifier
#' df <- dplyr::filter(
#'   Data_6Tribes_5y_Harmonized,
#'   TADA.ComparableDataIdentifier ==
#'     "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L"
#' )
#' TADA_Scatterplot(df, id_cols = "TADA.ComparableDataIdentifier")
#' # Creates a scatterplot for each monitoring location
#' TADA_Scatterplot(df, id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationName"))
#'
#' # Create multiple scatterplots with additional grouping columns and view the first
#' # plot in list. In this example, we will group by both TADA.ComparableDataIdentifier
#' # and MonitoringLocationTypeName (e.g. stream, reservoir, canal, etc.)
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#' Scatterplot_output <- TADA_Scatterplot(Data_Nutrients_UT,
#'   id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationTypeName")
#' )
#' # This example generates 47 scatterplots
#' Scatterplot_output[[10]]
#' Scatterplot_output[[25]]
#' Scatterplot_output[[35]]
#'
TADA_Scatterplot <- function(
  .data,
  id_cols = c("TADA.ComparableDataIdentifier")
) {
  # ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }

  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    warning(
      "TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended: plotting without it may produce errors in the plot."
    )
  }

  # check .data is data.frame and has required columns (including id_cols)
  expected_cols <- c(
    id_cols,
    c(
      "ActivityStartDate",
      "TADA.ResultMeasureValue",
      "TADA.ResultMeasure.MeasureUnitCode"
    )
  )
  TADA_CheckColumns(.data, expected_cols)

  .data <- .data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::mutate(Group = dplyr::cur_group_id())

  all_scatterplots <- list()

  for (i in 1:max(.data$Group)) {
    plot.data <- subset(.data, .data$Group == i)
    groupid <- TADA_CharStringRemoveNA(paste0(
      unique(plot.data[, id_cols]),
      collapse = " "
    ))

    # units label for y axis
    unit <- unique(plot.data$TADA.ResultMeasure.MeasureUnitCode)
    y_label <- "Activity Start Date"

    # create TADA color palette
    tada.pal <- TADA_ColorPalette(col_pair = TRUE)

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
      marker = list(color = tada.pal[1, 1]), # marker color
      stroke = I(tada.pal[1, 2]), # marker border color
      name = "<b>All Data<b>",
      hoverinfo = "text",
      hovertext = paste(
        "Result:",
        paste0(
          plot.data$TADA.ResultMeasureValue,
          " ",
          plot.data$TADA.ResultMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Start Date:",
        plot.data$ActivityStartDate,
        "<br>",
        "Activity Start Date Time:",
        plot.data$ActivityStartDateTime,
        "<br>",
        "Monitoring Location Name:",
        plot.data$MonitoringLocationName,
        "<br>",
        "Organization Name: ",
        plot.data$OrganizationFormalName,
        "<br>",
        "Media:",
        plot.data$TADA.ActivityMediaName,
        "<br>",
        "Media Subdivision:",
        plot.data$ActivityMediaSubdivisionName,
        "<br>",
        "Result Depth:",
        paste0(
          plot.data$TADA.ResultDepthHeightMeasure.MeasureValue,
          " ",
          plot.data$TADA.ResultDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Relative Depth:",
        plot.data$ActivityRelativeDepthName,
        "<br>",
        "Activity Depth:",
        paste0(
          plot.data$TADA.ActivityDepthHeightMeasure.MeasureValue,
          " ",
          plot.data$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Top Depth:",
        paste0(
          plot.data$TADA.ActivityTopDepthHeightMeasure.MeasureValue,
          " ",
          plot.data$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Bottom Depth:",
        paste0(
          plot.data$TADA.ActivityBottomDepthHeightMeasure.MeasureValue,
          " ",
          plot.data$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>"
      )
    )

    # figure margin
    mrg <- list(l = 50, r = 20, b = 20, t = 55, pad = 0)

    # scatterplot layout and labels
    one_scatterplot <- one_scatterplot |>
      plotly::layout(
        yaxis = list(
          title = unit,
          titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r",
          linecolor = "black",
          rangemode = "tozero",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        xaxis = list(
          title = y_label,
          titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r",
          linecolor = "black",
          rangemode = "tozero",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        hoverlabel = list(bgcolor = "white"),
        title = paste0("Scatterplot of \n", groupid),
        plot_bgcolor = "#e5ecf6",
        margin = mrg
      ) |>
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
#' @param .data TADA dataframe containing the data downloaded from the WQP,
#'   where each row represents a unique data record. Dataframe must include the
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
#'   be 'DISSOLVED OXYGEN (DO)_NA_NA_UG/L' and 'PH_NA_NA_STD UNITS'. These groups will
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
#' utils::data(Data_Nutrients_UT)
#' # Create a single scatterplot with two specified groups from TADA.ComparableDataIdentifier
#' TADA_TwoCharacteristicScatterplot(Data_Nutrients_UT,
#'   id_cols = "TADA.ComparableDataIdentifier",
#'   groups = c(
#'     "AMMONIA_UNFILTERED_AS N_MG/L",
#'     "NITRATE_UNFILTERED_AS N_MG/L"
#'   )
#' )
#'
#' # Load example dataset:
#' utils::data(Data_6Tribes_5y_Harmonized)
#' # Create a single scatterplot with two specified groups from TADA.ComparableDataIdentifier
#' TADA_TwoCharacteristicScatterplot(Data_6Tribes_5y_Harmonized,
#'   id_cols = "TADA.ComparableDataIdentifier",
#'   groups = c("TEMPERATURE_NA_NA_DEG C", "PH_NONE_NONE_NONE")
#' )
#'
TADA_TwoCharacteristicScatterplot <- function(
  .data,
  id_cols = "TADA.ComparableDataIdentifier",
  groups
) {
  # if left blank, ensure comparable data identifier is in the id_cols vector
  if (is.null(id_cols)) {
    id_cols <- "TADA.ComparableDataIdentifier"
  }

  # check .data is data.frame and has required columns (including id_cols)
  expected_cols <- c(
    id_cols,
    c(
      "ActivityStartDate",
      "TADA.ResultMeasureValue",
      "TADA.ResultMeasure.MeasureUnitCode"
    )
  )
  TADA_CheckColumns(.data, expected_cols)

  if (!"TADA.ComparableDataIdentifier" %in% id_cols) {
    print(
      "Note: TADA.ComparableDataIdentifier not found in id_cols argument and is highly recommended."
    )
  }

  # check that groups are in id_cols
  id <- unlist(unique(.data[, id_cols]))
  if (any(!groups %in% id)) {
    stop(
      "The 'groups' vector contains one or more inputs that are not found within your input dataset. Check spelling and try again."
    )
  }

  depthcols <- names(.data)[grepl("DepthHeightMeasure", names(.data))]
  depthcols <- depthcols[grepl("TADA.", depthcols)]

  plot.data <- as.data.frame(.data)

  # this subset must include all fields included in plot hover below
  plot.data <- subset(plot.data, plot.data[, id_cols] %in% groups)[, c(
    id_cols,
    expected_cols,
    depthcols,
    "ActivityStartDateTime",
    "MonitoringLocationName",
    "OrganizationFormalName",
    "TADA.ActivityMediaName",
    "ActivityMediaSubdivisionName",
    "ActivityRelativeDepthName",
    "TADA.CharacteristicName",
    "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText"
  )]
  plot.data$name <- gsub("_NA", "", plot.data[, id_cols])
  plot.data$name <- gsub("_", " ", plot.data$name)

  plot.data <- dplyr::arrange(plot.data, ActivityStartDate)

  param1 <- subset(plot.data, plot.data[, id_cols] %in% groups[1])
  param2 <- subset(plot.data, plot.data[, id_cols] %in% groups[2])

  title <- stringr::str_wrap(
    paste0(
      param1$TADA.CharacteristicName[1],
      " and ",
      param2$TADA.CharacteristicName[1],
      " Over Time"
    ),
    width = 45
  )

  # figure margin
  mrg <- list(l = 50, r = 75, b = 25, t = 75, pad = 0)

  # create TADA color palette
  tada.pal <- TADA_ColorPalette(col_pair = TRUE)

  scatterplot <- plotly::plot_ly(type = "scatter", mode = "markers") |>
    plotly::layout(
      xaxis = list(
        # title = "Activity Start Date", # not necessary?
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r",
        linecolor = "black",
        rangemode = "tozero",
        showgrid = FALSE,
        tickcolor = "black"
      ),
      yaxis = list(
        title = stringr::str_remove_all(
          stringr::str_remove_all(
            stringr::str_remove_all(
              paste0(
                param1$TADA.CharacteristicName[1],
                "  ",
                param1$TADA.ResultMeasure.MeasureUnitCode[1]
              ),
              stringr::fixed(" (NA)")
            ),
            stringr::fixed("NA ")
          ),
          stringr::fixed(" NA")
        ),
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r",
        linecolor = "black",
        rangemode = "tozero",
        showgrid = FALSE,
        tickcolor = "black"
      ),
      yaxis2 = list(
        side = "right",
        overlaying = "y",
        title = stringr::str_remove_all(
          stringr::str_remove_all(
            stringr::str_remove_all(
              paste0(
                param2$TADA.CharacteristicName[1],
                "  ",
                param2$TADA.ResultMeasure.MeasureUnitCode[1]
              ),
              stringr::fixed(" (NA)")
            ),
            stringr::fixed("NA ")
          ),
          stringr::fixed(" NA")
        ),
        titlefont = list(size = 16, family = "Arial"),
        tickfont = list(size = 16, family = "Arial"),
        hoverformat = ",.4r",
        linecolor = "black",
        rangemode = "tozero",
        showgrid = FALSE,
        tickcolor = "black"
      ),
      hoverlabel = list(bgcolor = "white"),
      title = title,
      plot_bgcolor = "#e5ecf6",
      margin = mrg,
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2)
    ) |>
    # config options https://plotly.com/r/configuration-options/
    plotly::config(displaylogo = FALSE) |> # , displayModeBar = TRUE) # TRUE makes bar always visible
    plotly::add_trace(
      data = param1,
      x = ~ as.Date(ActivityStartDate),
      y = ~TADA.ResultMeasureValue,
      name = stringr::str_remove_all(
        stringr::str_remove_all(
          stringr::str_remove_all(
            paste0(
              param1$TADA.ResultSampleFractionText,
              " ",
              param1$TADA.CharacteristicName,
              " ",
              param1$TADA.MethodSpeciationName
            ),
            stringr::fixed(" (NA)")
          ),
          stringr::fixed("NA ")
        ),
        stringr::fixed(" NA")
      ),
      marker = list(
        size = 10,
        color = tada.pal[1, 1],
        line = list(color = tada.pal[1, 2], width = 2)
      ),
      hoverinfo = "text",
      hovertext = paste(
        "Result:",
        paste0(
          param1$TADA.ResultMeasureValue,
          " ",
          param1$TADA.ResultMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Start Date:",
        param1$ActivityStartDate,
        "<br>",
        "Activity Start Date Time:",
        param1$ActivityStartDateTime,
        "<br>",
        "Monitoring Location Name:",
        param1$MonitoringLocationName,
        "<br>",
        "Organization Name: ",
        param1$OrganizationFormalName,
        "<br>",
        "Media:",
        param1$TADA.ActivityMediaName,
        "<br>",
        "Media Subdivision:",
        param1$ActivityMediaSubdivisionName,
        "<br>",
        "Result Depth:",
        paste0(
          param1$TADA.ResultDepthHeightMeasure.MeasureValue,
          " ",
          param1$TADA.ResultDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Relative Depth Name:",
        param1$ActivityRelativeDepthName,
        "<br>",
        "Activity Depth:",
        paste0(
          param1$TADA.ActivityDepthHeightMeasure.MeasureValue,
          " ",
          param1$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Top Depth:",
        paste0(
          param1$TADA.ActivityTopDepthHeightMeasure.MeasureValue,
          " ",
          param1$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Bottom Depth:",
        paste0(
          param1$TADA.ActivityBottomDepthHeightMeasure.MeasureValue,
          " ",
          param1$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>"
      )
    ) |>
    plotly::add_trace(
      data = param2,
      x = ~ as.Date(ActivityStartDate),
      y = ~TADA.ResultMeasureValue,
      name = stringr::str_remove_all(
        stringr::str_remove_all(
          stringr::str_remove_all(
            paste0(
              param2$TADA.ResultSampleFractionText,
              " ",
              param2$TADA.CharacteristicName,
              " ",
              param2$TADA.MethodSpeciationName
            ),
            stringr::fixed(" (NA)")
          ),
          stringr::fixed("NA ")
        ),
        stringr::fixed(" NA")
      ),
      marker = list(
        size = 10,
        color = tada.pal[2, 1],
        line = list(color = tada.pal[2, 2], width = 2)
      ),
      yaxis = "y2",
      hoverinfo = "text",
      hovertext = paste(
        "Result:",
        paste0(
          param2$TADA.ResultMeasureValue,
          " ",
          param2$TADA.ResultMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Start Date:",
        param2$ActivityStartDate,
        "<br>",
        "Activity Start Date Time:",
        param2$ActivityStartDateTime,
        "<br>",
        "Monitoring Location Name:",
        param2$MonitoringLocationName,
        "<br>",
        "Organization Name: ",
        param2$OrganizationFormalName,
        "<br>",
        "Media:",
        param2$TADA.ActivityMediaName,
        "<br>",
        "Media Subdivision:",
        param2$ActivityMediaSubdivisionName,
        "<br>",
        "Result Depth:",
        paste0(
          param2$TADA.ResultDepthHeightMeasure.MeasureValue,
          " ",
          param2$TADA.ResultDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Relative Depth Name:",
        param2$ActivityRelativeDepthName,
        "<br>",
        "Activity Depth:",
        paste0(
          param2$TADA.ActivityDepthHeightMeasure.MeasureValue,
          " ",
          param2$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Top Depth:",
        paste0(
          param2$TADA.ActivityTopDepthHeightMeasure.MeasureValue,
          " ",
          param2$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>",
        "Activity Bottom Depth:",
        paste0(
          param2$TADA.ActivityBottomDepthHeightMeasure.MeasureValue,
          " ",
          param2$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode
        ),
        "<br>"
      )
    )

  return(scatterplot)
}

#' Create Scatterplot(s) for Each TADA.ComparableDataIdentifier with Multiple (Up to 4) Groupings by an Additional Column
#'
#' @param .data TADA dataframe where each row represents a unique record. Dataframe must include
#'    the columns 'TADA.ComparableDataIdentifier', 'TADA.ResultMeasureValue',
#'   'TADA.ResultMeasure.MeasureUnitCode', 'ActivityStartDate', 'ActivityStartDateTime',
#'    'ActivityStartDateTime', 'MonitoringLocationName', 'TADA.ActivityMediaName',
#'    'ActivityMediaSubdivisionName', 'TADA.ResultDepthHeightMeasure.MeasureValue',
#'    'TADA.ResultDepthHeightMeasure.MeasureValue', 'TADA.ResultDepthHeightMeasure.MeasureUnitCode',
#'    'ActivityRelativeDepthName', 'TADA.ActivityDepthHeightMeasure.MeasureValue',
#'    'TADA.ActivityDepthHeightMeasure.MeasureUnitCode', 'TADA.ActivityTopDepthHeightMeasure.MeasureValue',
#'    'TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode', 'TADA.ActivityBottomDepthHeightMeasure.MeasureValue',
#'    and TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode to run this function.
#'
#' @param group_col The column in the dataset used to identify the groups
#'    plotted. Defaults to MonitoringLocationName. This input is flexible, and allows for the use of
#'    other identifiers such as StateCode, CountyCode or user-created groups based on concatenation
#'    of other variables (e.g. characteristic name, site type, site name, year, organization, etc.)
#'
#' @param groups A vector of up to four identifiers from the id_cols column
#'   to specify the groups that will be plotted for a TADA.ComparableDataIdentifier.
#'   These groups will be specific to your dataset. For example, in the example data set
#'   Data_6Tribes_5y_Harmonized if group_col is 'MonitoringLocationName', the groups could be
#'   'Upper Red Lake: West', 'Upper Red Lake: West-Central', and 'Upper Red Lake: East Central'.
#'
#' @return A plotly scatterplot(s) figure with one x-axis (Date/Time) and a
#'   left axis showing the units of a single TADA.ComparableDataIdentifier plotted on the same
#'   figure area with. Groups are identified by different colored circle markers and are displayed
#'   in a legend.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' utils::data(Data_Nutrients_UT)
#' # UT Nutrients results grouped by county
#' # transform non-detect data
#' df2 <- TADA_SimpleCensoredMethods(Data_Nutrients_UT)
#' # create scatterplots for selected counties
#' UT_Nutrients_by_CountyCode <- TADA_GroupedScatterplot(
#'   df2,
#'   group_col = "CountyCode", groups = c("057", "011", "003", "037")
#' )
#' # view the 3rd and 4th plots
#' UT_Nutrients_by_CountyCode[[3]]
#' UT_Nutrients_by_CountyCode[[4]]
#'
#' # Load example dataset:
#' utils::data(Data_6Tribes_5y_Harmonized)
#'
#' # Filter the example data so it includes only one
#' # TADA.ComparableDataIdentifier
#' df <- dplyr::filter(
#'   Data_6Tribes_5y_Harmonized,
#'   TADA.ComparableDataIdentifier %in% c(
#'     "TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_UG/L"
#'   )
#' )
#' # Creates a scatterplot of the three specified sites of interest in the
#' # same plot.
#' TADA_GroupedScatterplot(df,
#'   group_col = "MonitoringLocationName",
#'   groups = c(
#'     "Upper Red Lake: West", "Upper Red Lake: West-Central",
#'     "Upper Red Lake: East Central"
#'   )
#' )
#'
#' # If no groups are selected, return the 4 groups
#' # (by MonitoringLocationName) with the greatest number of results
#' TADA_GroupedScatterplot(df, group_col = "MonitoringLocationName")
#'
TADA_GroupedScatterplot <- function(
  .data,
  group_col = "MonitoringLocationName",
  groups = NULL
) {
  # check .data is data.frame and has required columns (including group_col)
  required_cols <- c(
    "TADA.ComparableDataIdentifier",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode",
    "ActivityStartDate",
    "ActivityStartDateTime",
    "MonitoringLocationName",
    "OrganizationFormalName"
  )
  required_cols <- required_cols |> append(group_col) |> unique()
  TADA_CheckColumns(.data, required_cols)

  # only allows for 1 column selection in id_cols
  if (length(group_col) > 1) {
    stop(
      "TADA_GroupedScatterplot: group_col argument can only be a single value."
    )
  }

  # stop function if only one group is supplied
  if (!is.null(groups)) {
    if (length(groups) == 1) {
      stop(
        "TADA_GroupedScatterplot: requires at least two 'groups'. Use TADA_Scatterplot to plot results without grouping."
      )
    }
  }

  # if groups are not specified, select the top four groups by number of results.
  if (is.null(groups)) {
    assign.groups <- .data |>
      dplyr::group_by_at(group_col) |>
      dplyr::summarize(NResults = length(TADA.ResultMeasureValue)) |>
      dplyr::arrange(dplyr::desc(NResults)) |>
      dplyr::filter(!is.na(get(group_col)))

    # calculate how many total groups are available in TADA dataframe
    n.groups.total <- nrow(assign.groups)

    # select top four groups by number of results
    groups <- assign.groups |>
      dplyr::slice_head(n = 4) |>
      dplyr::select(as.character(group_col)) |>
      dplyr::pull()

    # create string of group names for printed message
    groups.string <- stringi::stri_replace_last(
      paste(groups, collapse = "; "),
      " and ",
      fixed = "; "
    )

    # calculate number of groups to be plotted
    n.groups.plotted <- length(groups)

    # convert integer to character string for use in printed message
    # could consider replacing with english::as_english
    n.groups.plotted <- dplyr::case_when(
      n.groups.plotted == 1 ~ "one",
      n.groups.plotted == 2 ~ "two",
      n.groups.plotted == 3 ~ "three",
      n.groups.plotted == 4 ~ "four"
    )

    # if only group is identified, stop and print message to use TADA_Scatterplot
    if (n.groups.plotted == "one") {
      stop(
        "TADA_GroupedScatterplot: requires at least two 'groups'. Use TADA_Scatterplot to plot results without grouping."
      )
    }

    # print message describing groups that will be plotted
    print(paste0(
      "TADA_GroupedScatterplot: No 'groups' selected for ",
      group_col,
      ". There are ",
      n.groups.total,
      " ",
      group_col,
      "s in the TADA dataframe. The top ",
      n.groups.plotted,
      " ",
      group_col,
      "s by number of results will be plotted: ",
      groups.string,
      ".",
      sep = ""
    ))

    # remove intermediate objects
    rm(groups.string, n.groups.plotted)
  }

  # check that groups are in group_col
  id <- unlist(unique(.data[, group_col]))
  if (any(!groups %in% id)) {
    # identify any groups missing from "groups" param
    missing.groups <- setdiff(groups, id)

    # create a character string of missing groups for printed message
    missing.groups.string <- stringi::stri_replace_last(
      paste(missing.groups, collapse = "; "),
      " and ",
      fixed = "; "
    )

    # stop function if any groups are not found in TADA dataframe
    stop(
      "TADA_GroupedScatterplot: The following ",
      group_col,
      "s are not found in the TADA dataframe: ",
      missing.groups.string,
      ". Revise param 'groups' before re-running function.",
      sep = ""
    )

    # remove intermediate objects
    rm(missing.group, missing.groups.string, id)
  }

  depthcols <- names(.data)[grepl("DepthHeightMeasure", names(.data))]
  depthcols <- depthcols[grepl("TADA.", depthcols)]

  plot.data <- as.data.frame(.data)

  # this subset must include all fields included in plot hover below
  plot.data <- subset(plot.data, plot.data[, group_col] %in% groups)[, unique(c(
    group_col,
    required_cols,
    depthcols,
    "TADA.ComparableDataIdentifier",
    "ActivityStartDateTime",
    "MonitoringLocationName",
    "TADA.ActivityMediaName",
    "ActivityMediaSubdivisionName",
    "ActivityRelativeDepthName",
    "TADA.CharacteristicName",
    "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText",
    "OrganizationFormalName"
  ))]

  plot.data <- dplyr::arrange(plot.data, ActivityStartDate)

  # returns the param groups for plotting. Up to 4 params are defined.
  param.data <- list()
  for (i in 1:length(unique(groups))) {
    param.data[[i]] <- subset(plot.data, plot.data[, group_col] %in% groups[i])
    # assign(paste0("param", as.character(i)), subset(plot.data, plot.data[, group_col] %in% groups[i]))
  }

  # create empty list to store scatterplots
  all_scatterplots <- list()

  for (i in 1:length(unique(plot.data$TADA.ComparableDataIdentifier))) {
    title <- stringr::str_wrap(
      paste0(
        "Scatterplot of ",
        TADA_CharStringRemoveNA(unique(plot.data$TADA.ComparableDataIdentifier)[
          i
        ]),
        " Over Time"
      ),
      width = 45
    )

    # figure margin
    mrg <- list(l = 50, r = 75, b = 25, t = 75, pad = 0)

    # units label for y axis
    unit <- unique(plot.data$TADA.ResultMeasure.MeasureUnitCode)
    y_label <- "Activity Start Date"

    # create TADA color palette
    tada.pal <- TADA_ColorPalette(col_pair = TRUE)

    plot.data.y <- subset(
      plot.data,
      plot.data[, "TADA.ComparableDataIdentifier"] %in%
        unique(plot.data$TADA.ComparableDataIdentifier)[i]
    )
    plot.data.y$name <- gsub(
      "_NA",
      "",
      plot.data.y[, "TADA.ComparableDataIdentifier"]
    )
    plot.data.y$name <- gsub("_", " ", plot.data.y$name)

    scatterplot <- plotly::plot_ly(type = "scatter", mode = "markers") |>
      plotly::layout(
        xaxis = list(
          # title = "Activity Start Date", # not necessary?
          titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r",
          linecolor = "black",
          rangemode = "tozero",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        yaxis = list(
          title = paste(
            TADA_CharStringRemoveNA(plot.data.y$TADA.CharacteristicName[1]),
            TADA_CharStringRemoveNA(unique(
              plot.data.y$TADA.ResultMeasure.MeasureUnitCode
            ))
          ),
          titlefont = list(size = 16, family = "Arial"),
          tickfont = list(size = 16, family = "Arial"),
          hoverformat = ",.4r",
          linecolor = "black",
          rangemode = "tozero",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        hoverlabel = list(bgcolor = "white"),
        title = title,
        plot_bgcolor = "#e5ecf6",
        margin = mrg,
        legend = list(
          title = list(
            text = paste0("<b>", group_col, "<b>"),
            x = 0.5,
            y = 100
          ),
          orientation = "h",
          xanchor = "center",
          x = 0.5
        )
      ) |>
      # config options https://plotly.com/r/configuration-options/
      plotly::config(displaylogo = FALSE) # , displayModeBar = TRUE) # TRUE makes bar always visible

    param <- list()
    for (j in 1:length(groups)) {
      if (length(groups) >= j) {
        param[[j]] <- subset(
          param.data[[j]],
          param.data[[j]][, "TADA.ComparableDataIdentifier"] %in%
            unique(plot.data$TADA.ComparableDataIdentifier)[i]
        )

        scatterplot <- scatterplot |>
          plotly::add_trace(
            data = param[[j]],
            x = ~ as.Date(ActivityStartDate),
            y = ~TADA.ResultMeasureValue,
            name = groups[j],
            marker = list(
              size = 10,
              color = tada.pal[j, 1],
              line = list(color = tada.pal[j, 2], width = 2)
            ),
            hoverinfo = "text",
            hovertext = paste(
              "Result:",
              paste0(
                param[[j]]$TADA.ResultMeasureValue,
                " ",
                param[[j]]$TADA.ResultMeasure.MeasureUnitCode
              ),
              "<br>",
              "Activity Start Date:",
              param[[j]]$ActivityStartDate,
              "<br>",
              "Activity Start Date Time:",
              param[[j]]$ActivityStartDateTime,
              "<br>",
              "Monitoring Location Name:",
              param[[j]]$MonitoringLocationName,
              "<br>",
              "Organization Name: ",
              param[[j]]$OrganizationFormalName,
              "<br>",
              "Media:",
              param[[j]]$TADA.ActivityMediaName,
              "<br>",
              "Media Subdivision:",
              param[[j]]$ActivityMediaSubdivisionName,
              "<br>",
              "Result Depth:",
              paste0(
                param[[j]]$TADA.ResultDepthHeightMeasure.MeasureValue,
                " ",
                param[[j]]$TADA.ResultDepthHeightMeasure.MeasureUnitCode
              ),
              "<br>",
              "Activity Relative Depth Name:",
              param[[j]]$ActivityRelativeDepthName,
              "<br>",
              "Activity Depth:",
              paste0(
                param[[j]]$TADA.ActivityDepthHeightMeasure.MeasureValue,
                " ",
                param[[j]]$TADA.ActivityDepthHeightMeasure.MeasureUnitCode
              ),
              "<br>",
              "Activity Top Depth:",
              paste0(
                param[[j]]$TADA.ActivityTopDepthHeightMeasure.MeasureValue,
                " ",
                param[[j]]$TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode
              ),
              "<br>",
              "Activity Bottom Depth:",
              paste0(
                param[[j]]$TADA.ActivityBottomDepthHeightMeasure.MeasureValue,
                " ",
                param[[j]]$TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode
              ),
              "<br>"
            )
          )
      }
    }

    # create plots and store as list
    all_scatterplots[[i]] <- scatterplot

    # rename scatterplots to reflect TADA.ComparbaleDataIdentifier (with NAs removed)
    names(all_scatterplots)[i] <- unique(TADA_CharStringRemoveNA(
      plot.data$TADA.ComparableDataIdentifier
    ))[i]
  }

  # filter to return one scatterplot, if only one was generated
  if (length(all_scatterplots) == 1) {
    all_scatterplots <- all_scatterplots[[1]]
  }

  # return scatterplot (one) or list of scatterplots (multiple)
  return(all_scatterplots)
}
