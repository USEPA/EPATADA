# tests for TADA_DayOfYearPlot
testthat::test_that("TADA_DayOfYearPlot validates inputs and returns expected output", {
  testthat::expect_s3_class(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "plotly"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT |> dplyr::select(-TADA.MonitoringLocationIdentifier),
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "TADA.MonitoringLocationIdentifier"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(Data_Nutrients_UT),
    "TADA.ComparableDataIdentifier"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(Data_Nutrients_UT, comparableDataId = "NOT_A_REAL_ID"),
    "was not found"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      location = "BAD_LOCATION",
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "not found"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      location = c("A", "B"),
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "must be one monitoring location identifier or 'all'"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
      monthRange = c(13, 14)
    ),
    "monthRange"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
      monthRange = c(10, 4)
    ),
    "monthRange"
  )

  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
      yearRange = c(2022, 2020)
    ),
    "yearRange"
  )

  testthat::expect_null(TADA_DayOfYearPlot(
    Data_Nutrients_UT,
    comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
    yearRange = c(1800, 1801)
  ))

  testthat::expect_s3_class(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT |>
        dplyr::select(
          -dplyr::any_of(c(
            "TADA.MonitoringLocationName",
            "OrganizationFormalName",
            "ActivityStartDateTime"
          ))
        ),
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "plotly"
  )

  res <- TADA_DayOfYearPlot(
    Data_Nutrients_UT,
    comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
  )
  built <- plotly::plotly_build(res)
  testthat::expect_true(length(built$x$data) >= 1)
})

# tests for TADA_Boxplot
testthat::test_that("TADA_Boxplot errors when required columns are missing", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("g1", "g1"),
    TADA.ResultMeasureValue = c(1, 2)
    # missing TADA.ResultMeasure.MeasureUnitCode
  )

  testthat::expect_error(TADA_Boxplot(df), "TADA.ResultMeasure.MeasureUnitCode")
})

testthat::test_that("TADA_Boxplot defaults id_cols to TADA.ComparableDataIdentifier when id_cols is NULL", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
    ),
    TADA.ResultMeasureValue = c(4.5, 6, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )

  p_null <- TADA_Boxplot(df, id_cols = NULL)
  p_explicit <- TADA_Boxplot(df, id_cols = "TADA.ComparableDataIdentifier")

  testthat::expect_s3_class(p_null, "plotly")
  testthat::expect_s3_class(p_explicit, "plotly")

  title_null <- p_null$x$layoutAttrs$`1f34f8b4693`$title
  if (is.list(title_null) && !is.null(title_null$text)) {
    title_null <- title_null$text
  }

  title_explicit <- p_explicit$x$layoutAttrs$`1f3452ee66ba`$title
  if (is.list(title_explicit) && !is.null(title_explicit$text)) {
    title_explicit <- title_explicit$text
  }

  testthat::expect_equal(title_null, title_explicit)
})

testthat::test_that("TADA_Boxplot warns when TADA.ComparableDataIdentifier is not included in id_cols", {
  df <- data.frame(
    OtherGroup = c("a", "a", "b"),
    TADA.ResultMeasureValue = c(1, 2, 3),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )

  testthat::expect_warning(
    TADA_Boxplot(df, id_cols = "OtherGroup"),
    "TADA.ComparableDataIdentifier not found in id_cols"
  )
})

testthat::test_that("TADA_Boxplot removes NA values and emits a message", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
    ),
    TADA.ResultMeasureValue = c(4.5, NA, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("mg/L", "mg/L", "mg/L"),
    stringsAsFactors = FALSE
  )

  testthat::expect_message(
    p <- TADA_Boxplot(df),
    "removed 1 results where TADA.ResultMeasureValue = NA"
  )

  testthat::expect_s3_class(p, "plotly")
})

testthat::test_that("TADA_Boxplot returns a single plotly object when there is only one group and a list for multiple groups", {
  single_df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
    ),
    TADA.ResultMeasureValue = c(4.5, 6, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L"),
    stringsAsFactors = FALSE
  )

  multi_df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE"
    ),
    TADA.ResultMeasureValue = c(4.5, 6, 5, 7, 4.5, 6.7),
    TADA.ResultMeasure.MeasureUnitCode = c(
      "MG/L",
      "MG/L",
      "MG/L",
      "NONE",
      "NONE",
      "NONE"
    ),
    stringsAsFactors = FALSE
  )

  p_single <- TADA_Boxplot(single_df)
  p_multi <- TADA_Boxplot(multi_df)

  testthat::expect_s3_class(p_single, "plotly")
  testthat::expect_s3_class(p_single, "htmlwidget")

  testthat::expect_type(p_multi, "list")
  testthat::expect_length(p_multi, 2)
  testthat::expect_true(all(vapply(
    p_multi,
    inherits,
    logical(1),
    what = "plotly"
  )))
})

testthat::test_that("TADA_Boxplot sets expected layout fields", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
    ),
    TADA.ResultMeasureValue = c(4.5, 6, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L"),
    stringsAsFactors = FALSE
  )

  p <- TADA_Boxplot(df)

  title <- p$x$layoutAttrs[[1]]$title
  if (is.list(title) && !is.null(title$text)) {
    title <- title$text
  }

  y_title <- p$x$layoutAttrs[[1]]$yaxis$title
  if (is.list(y_title) && !is.null(y_title$text)) {
    y_title <- y_title$text
  }

  testthat::expect_equal(title, "Boxplot of \nDISSOLVED OXYGEN (DO) MG/L")
  testthat::expect_equal(y_title, "MG/L")
  testthat::expect_false(isTRUE(
    p$x$layoutAttrs$`1f345e0241b3`$xaxis$showticklabels
  ))
  testthat::expect_false(isTRUE(p$x$config$displayModeBar))
})

testthat::test_that("TADA_Boxplot computes quartiles and median correctly", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
    ),
    TADA.ResultMeasureValue = c(4.5, 6, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L"),
    stringsAsFactors = FALSE
  )

  p <- TADA_Boxplot(df)

  values <- df$TADA.ResultMeasureValue

  expect_equal(
    as.numeric(p$x$attrs[[1]]$q1),
    as.numeric(stats::quantile(values, 0.25, type = 7))
  )
  expect_equal(
    as.numeric(p$x$attrs[[1]]$median),
    as.numeric(stats::median(values))
  )
  expect_equal(
    as.numeric(p$x$attrs[[1]]$q3),
    as.numeric(stats::quantile(values, 0.75, type = 7))
  )
})

testthat::test_that("TADA_Boxplot computes whiskers/fences consistently with outliers", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = rep("g1", 6),
    TADA.ResultMeasureValue = c(1, 2, 3, 4, 5, 100),
    TADA.ResultMeasure.MeasureUnitCode = rep("mg/L", 6),
    stringsAsFactors = FALSE
  )

  p <- TADA_Boxplot(df)
  trace <- p$x$data[[1]]

  values <- df$TADA.ResultMeasureValue
  q1 <- as.numeric(stats::quantile(values, 0.25, type = 7))
  q3 <- as.numeric(stats::quantile(values, 0.75, type = 7))
  iqr <- q3 - q1
  upper_thresh <- q3 + 1.5 * iqr
  lower_thresh <- q1 - 1.5 * iqr

  expected_upper <- max(values[values <= upper_thresh])
  expected_lower <- min(values[values >= lower_thresh])

  testthat::expect_equal(as.numeric(p$x$attrs[[1]]$upperfence), expected_upper)
  testthat::expect_equal(as.numeric(p$x$attrs[[1]]$lowerfence), expected_lower)
})

testthat::test_that("TADA_Boxplot handles multiple grouping columns and names plots by combined group id", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"
    ),
    TADA.ResultMeasureValue = c(4.5, 6, 5, 4),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L", "MG/L"),
    OrganizationIdentifier = c(
      "Test Org A",
      "Test Org A",
      "Test Org B",
      "Test Org B"
    ),
    stringsAsFactors = FALSE
  )

  p <- TADA_Boxplot(
    df,
    id_cols = c("TADA.ComparableDataIdentifier", "OrganizationIdentifier")
  )

  testthat::expect_type(p, "list")
  testthat::expect_length(p, 2)
  testthat::expect_true(all(
    c(
      "DISSOLVED OXYGEN (DO) MG/L Test Org A",
      "DISSOLVED OXYGEN (DO) MG/L Test Org B"
    ) %in%
      names(p)
  ))
  testthat::expect_true(all(vapply(p, inherits, logical(1), what = "plotly")))
})

# tests for TADA_Histogram
testthat::test_that("TADA_Histogram validates required inputs and core behavior", {
  testthat::expect_error(
    TADA_Histogram(data.frame(
      TADA.ComparableDataIdentifier = c(
        "PH_NONE_NONE_NONE",
        "PH_NONE_NONE_NONE"
      ),
      TADA.ResultMeasureValue = c(6, 7.2)
    )),
    "TADA.ResultMeasure.MeasureUnitCode"
  )

  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE"
    ),
    TADA.ResultMeasureValue = c(6, 7.2, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE"),
    stringsAsFactors = FALSE
  )

  p_null <- TADA_Histogram(df, id_cols = NULL)
  p_explicit <- TADA_Histogram(df, id_cols = "TADA.ComparableDataIdentifier")

  testthat::expect_equal(
    p_null$x$layoutAttrs[[1]]$title,
    p_explicit$x$layoutAttrs[[1]]$title
  )
  testthat::expect_equal(p_null$x$layoutAttrs[[1]]$title, "Histogram of \nPH")

  testthat::expect_warning(
    TADA_Histogram(
      data.frame(
        OtherGroup = c("a", "a", "b"),
        TADA.ResultMeasureValue = c(1, 2, 3),
        TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L"),
        stringsAsFactors = FALSE
      ),
      id_cols = "OtherGroup"
    ),
    "TADA.ComparableDataIdentifier not found in id_cols"
  )

  testthat::expect_message(
    p_na <- TADA_Histogram(data.frame(
      TADA.ComparableDataIdentifier = c(
        "PH_NONE_NONE_NONE",
        "PH_NONE_NONE_NONE",
        "PH_NONE_NONE_NONE"
      ),
      TADA.ResultMeasureValue = c(6, 7.2, NA),
      TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE"),
      stringsAsFactors = FALSE
    )),
    "removed 1 results where TADA.ResultMeasureValue = NA"
  )
  testthat::expect_s3_class(p_na, "plotly")

  testthat::expect_s3_class(TADA_Histogram(df), "plotly")
  testthat::expect_s3_class(TADA_Histogram(df), "htmlwidget")

  df_multi <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE",
      "TEMPERATURE_NONE_NONE_DEG C",
      "TEMPERATURE_NONE_NONE_DEG C",
      "TEMPERATURE_NONE_NONE_DEG C"
    ),
    TADA.ResultMeasureValue = c(6, 7.2, 5, 10, 12, 19),
    TADA.ResultMeasure.MeasureUnitCode = c(
      "NONE",
      "NONE",
      "NONE",
      "DEG C",
      "DEG C",
      "DEG C"
    ),
    stringsAsFactors = FALSE
  )

  p_multi <- TADA_Histogram(df_multi)
  testthat::expect_type(p_multi, "list")
  testthat::expect_length(p_multi, 2)
  testthat::expect_named(p_multi, c("PH", "TEMPERATURE DEG C"))
  testthat::expect_true(all(vapply(
    p_multi,
    inherits,
    logical(1),
    what = "plotly"
  )))

  p <- TADA_Histogram(df)

  testthat::expect_equal(p$x$layoutAttrs[[1]]$xaxis$title, "NONE")
  testthat::expect_equal(p$x$layoutAttrs[[1]]$yaxis$title, "Frequency")
  testthat::expect_equal(p$x$layoutAttrs[[1]]$barmode, "overlay")
  testthat::expect_true(isTRUE(p$x$config$displayModeBar))

  json <- plotly::plotly_json(
    TADA_Histogram(data.frame(
      TADA.ComparableDataIdentifier = rep("PH_NONE_NONE_NONE", 4),
      TADA.ResultMeasureValue = c(6, 7.2, 5, 22),
      TADA.ResultMeasure.MeasureUnitCode = rep("NONE", 4),
      stringsAsFactors = FALSE
    )),
    jsonedit = FALSE
  )
  testthat::expect_match(json, "Outliers Removed")
  testthat::expect_match(json, "All Data")
  testthat::expect_match(json, "legendonly")

  testthat::expect_message(
    out <- TADA_Histogram(data.frame(
      TADA.ComparableDataIdentifier = character(0),
      TADA.ResultMeasureValue = numeric(0),
      TADA.ResultMeasure.MeasureUnitCode = character(0),
      stringsAsFactors = FALSE
    )),
    "No data to plot; returning NULL."
  )
  testthat::expect_null(out)
})

# tests for TADA_FieldValuesPie
testthat::test_that("TADA_FieldValuesPie validates input and plot structure", {
  p <- TADA_FieldValuesPie(Data_Nutrients_UT, field = "TADA.CharacteristicName")
  testthat::expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)
  scale <- built$plot$scales$scales[[1]]
  testthat::expect_equal(scale$name, "TADA.CharacteristicName")
  testthat::expect_true(any(vapply(
    p$layers,
    function(x) inherits(x$geom, "GeomBar"),
    logical(1)
  )))
  testthat::expect_equal(p$coordinates$theta, "y")
})

testthat::test_that("TADA_FieldValuesPie handles category limits and filters", {
  df_6 <- Data_TribalNations_Harmonized |>
    dplyr::filter(
      TADA.ComparableDataIdentifier %in%
        c(
          "COPPER_DISSOLVED_NONE_UG/L",
          "IRON_DISSOLVED_NONE_UG/L",
          "LEAD_DISSOLVED_NONE_UG/L",
          "MAGNESIUM_DISSOLVED_NONE_UG/L",
          "MERCURY_DISSOLVED_NONE_UG/L",
          "ZINC_DISSOLVED_NONE_UG/L"
        )
    )

  p_6 <- TADA_FieldValuesPie(df_6, field = "TADA.CharacteristicName")
  testthat::expect_equal(nrow(ggplot2::ggplot_build(p_6)$data[[1]]), 6)

  p_13 <- TADA_FieldValuesPie(
    Data_TribalNations_Harmonized,
    field = "TADA.CharacteristicName"
  )
  testthat::expect_equal(nrow(ggplot2::ggplot_build(p_13)$data[[1]]), 13)

  p_filter <- TADA_FieldValuesPie(
    Data_Nutrients_UT,
    field = "TADA.CharacteristicName",
    characteristicName = "AMMONIA"
  )
  testthat::expect_equal(nrow(ggplot2::ggplot_build(p_filter)$data[[1]]), 1)
  testthat::expect_s3_class(p_filter, "ggplot")
})

# tests for TADA_Scatterplot

testthat::test_that("TADA_Scatterplot errors when required columns are missing", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "A",
    ActivityStartDate = as.Date("2020-01-01")
    # missing TADA.ResultMeasureValue and unit code
  )

  testthat::expect_error(
    TADA_Scatterplot(df),
    regexp = "missing|required|column",
    ignore.case = TRUE
  )
})

testthat::test_that("TADA_Scatterplot uses default id_cols when NULL is supplied", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE",
      "TEMPERATURE_NONE_NONE_DEG C"
    ),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    TADA.ResultMeasureValue = c(5, 7, 11),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "DEG C"),
    ActivityStartDateTime = as.POSIXct(
      c("2020-01-01 10:00:00", "2020-01-02 11:00:00", "2020-01-03 12:00:00"),
      tz = "UTC"
    ),
    MonitoringLocationName = c("Site 1", "Site 1", "Site 2"),
    OrganizationFormalName = c("Org", "Org", "Org"),
    TADA.ActivityMediaName = c("Water", "Water", "Water"),
    ActivityMediaSubdivisionName = c("River", "River", "Lake"),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA),
    ActivityRelativeDepthName = c(NA, NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA)
  )

  p <- TADA_Scatterplot(df, id_cols = NULL)

  testthat::expect_type(p, "list")
  testthat::expect_named(p, c("PH", "TEMPERATURE DEG C"))
  testthat::expect_true(all(vapply(p, inherits, logical(1), "plotly")))
})

testthat::test_that("TADA_Scatterplot warns when TADA.ComparableDataIdentifier is not in id_cols", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "TEMPERATURE_NONE_NONE_DEG C",
      "TEMPERATURE_NONE_NONE_DEG C"
    ),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02")),
    TADA.ResultMeasureValue = c(4, 6.5),
    TADA.ResultMeasure.MeasureUnitCode = c("DEG C", "DEG C"),
    ActivityStartDateTime = as.POSIXct(
      c("2020-01-01 10:00:00", "2020-01-02 11:00:00"),
      tz = "UTC"
    ),
    MonitoringLocationName = c("Site 1", "Site 1"),
    OrganizationFormalName = c("Org", "Org"),
    TADA.ActivityMediaName = c("Water", "Water"),
    ActivityMediaSubdivisionName = c("River", "River"),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    ActivityRelativeDepthName = c(NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode = c(NA, NA)
  )

  testthat::expect_warning(
    TADA_Scatterplot(df, id_cols = "MonitoringLocationName"),
    regexp = "highly recommended",
    fixed = FALSE
  )
})

testthat::test_that("TADA_Scatterplot returns a single plotly object for one group", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02")),
    TADA.ResultMeasureValue = c(5.5, 6),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE"),
    ActivityStartDateTime = as.POSIXct(
      c("2020-01-01 10:00:00", "2020-01-02 11:00:00"),
      tz = "UTC"
    ),
    MonitoringLocationName = c("Site 1", "Site 1"),
    OrganizationFormalName = c("Org", "Org"),
    TADA.ActivityMediaName = c("Water", "Water"),
    ActivityMediaSubdivisionName = c("River", "River"),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    ActivityRelativeDepthName = c(NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode = c(NA, NA)
  )

  p <- TADA_Scatterplot(df)

  testthat::expect_s3_class(p, "plotly")
  testthat::expect_s3_class(p, "htmlwidget")
  testthat::expect_match(p$x$layoutAttrs[[1]]$title, "Scatterplot of \nPH")
  testthat::expect_equal(
    p$x$layoutAttrs[[1]]$xaxis$title,
    "Activity Start Date"
  )
  testthat::expect_equal(p$x$layoutAttrs[[1]]$yaxis$title, "NONE")
})

testthat::test_that("TADA_Scatterplot returns a named list for multiple groups", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE",
      "TEMPERATURE_NONE_NONE_DEG C",
      "TEMPERATURE_NONE_NONE_DEG C"
    ),
    ActivityStartDate = as.Date(c(
      "2020-01-01",
      "2020-01-02",
      "2020-01-03",
      "2020-01-02"
    )),
    TADA.ResultMeasureValue = c(4.5, 5, 5, 8),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "DEG C", "DEG C"),
    ActivityStartDateTime = as.POSIXct(
      c(
        "2020-01-01 10:00:00",
        "2020-01-02 11:00:00",
        "2020-01-03 12:00:00",
        "2020-01-02 11:00:00"
      ),
      tz = "UTC"
    ),
    MonitoringLocationName = c("Site 1", "Site 1", "Site 2", "Site 2"),
    OrganizationFormalName = c("Org", "Org", "Org", "Org"),
    TADA.ActivityMediaName = c("Water", "Water", "Water", "Water"),
    ActivityMediaSubdivisionName = c("River", "River", "Lake", "Lake"),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(NA, NA, NA, NA),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA, NA),
    ActivityRelativeDepthName = c(NA, NA, NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(NA, NA, NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureValue = c(NA, NA, NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(NA, NA, NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA, NA)
  )

  p <- TADA_Scatterplot(df)

  testthat::expect_type(p, "list")
  testthat::expect_named(p, c("PH", "TEMPERATURE DEG C"))
  testthat::expect_true(all(vapply(p, inherits, logical(1), "plotly")))
})

testthat::test_that("TADA_Scatterplot populates trace x and y correctly", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02")),
    TADA.ResultMeasureValue = c(5.5, 6),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE"),
    ActivityStartDateTime = as.POSIXct(
      c("2020-01-01 10:00:00", "2020-01-02 11:00:00"),
      tz = "UTC"
    ),
    MonitoringLocationName = c("Site 1", "Site 1"),
    OrganizationFormalName = c("Org", "Org"),
    TADA.ActivityMediaName = c("Water", "Water"),
    ActivityMediaSubdivisionName = c("River", "River"),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    ActivityRelativeDepthName = c(NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode = c(NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode = c(NA, NA)
  )
  p <- TADA_Scatterplot(df)

  testthat::expect_equal(p$x$attrs[[1]]$x[1], df$ActivityStartDate[1])
  testthat::expect_equal(p$x$attrs[[1]]$y[1], df$TADA.ResultMeasureValue[1])
})

testthat::test_that("TADA_Scatterplot groups by multiple id_cols", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE",
      "PH_NONE_NONE_NONE"
    ),
    MonitoringLocationTypeName = c("Stream", "Stream", "Lake"),
    ActivityStartDate = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    TADA.ResultMeasureValue = c(5, 5.5, 7),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE"),
    ActivityStartDateTime = as.POSIXct(
      c("2020-01-01 10:00:00", "2020-01-02 11:00:00", "2020-01-03 12:00:00"),
      tz = "UTC"
    ),
    MonitoringLocationName = c("Site 1", "Site 1", "Site 2"),
    OrganizationFormalName = c("Org", "Org", "Org"),
    TADA.ActivityMediaName = c("Water", "Water", "Water"),
    ActivityMediaSubdivisionName = c("River", "River", "Lake"),
    TADA.ResultDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA),
    ActivityRelativeDepthName = c(NA, NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = c(NA, NA, NA),
    TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode = c(NA, NA, NA)
  )

  p <- TADA_Scatterplot(
    df,
    id_cols = c("TADA.ComparableDataIdentifier", "MonitoringLocationTypeName")
  )

  testthat::expect_type(p, "list")
  testthat::expect_length(p, 2)
})

testthat::test_that("TADA_Scatterplot errors when id_cols do not exist", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = "PH_NONE_NONE_NONE",
    ActivityStartDate = as.Date("2020-01-01"),
    TADA.ResultMeasureValue = 6.5,
    TADA.ResultMeasure.MeasureUnitCode = "NONE",
    ActivityStartDateTime = as.POSIXct("2020-01-01 10:00:00", tz = "UTC"),
    MonitoringLocationName = "Site 1",
    OrganizationFormalName = "Org",
    TADA.ActivityMediaName = "Water",
    ActivityMediaSubdivisionName = "River",
    TADA.ResultDepthHeightMeasure.MeasureValue = NA,
    TADA.ResultDepthHeightMeasure.MeasureUnitCode = NA,
    ActivityRelativeDepthName = NA,
    TADA.ActivityDepthHeightMeasure.MeasureValue = NA,
    TADA.ActivityDepthHeightMeasure.MeasureUnitCode = NA,
    TADA.ActivityTopDepthHeightMeasure.MeasureValue = NA,
    TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode = NA,
    TADA.ActivityBottomDepthHeightMeasure.MeasureValue = NA,
    TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode = NA
  )

  suppressWarnings(testthat::expect_error(
    TADA_Scatterplot(df, id_cols = "NotAColumn"),
    regexp = "NotAColumn|column",
    ignore.case = TRUE
  ))
})
