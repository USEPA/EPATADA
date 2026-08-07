# tests for TADA_Boxplot

testthat::test_that("TADA_Boxplot errors when required columns are missing", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("g1", "g1"),
    TADA.ResultMeasureValue = c(1, 2)
    # missing TADA.ResultMeasure.MeasureUnitCode
  )

  testthat::expect_error(
    TADA_Boxplot(df),
    "TADA.ResultMeasure.MeasureUnitCode"
  )
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
  if (is.list(title_null) && !is.null(title_null$text)) title_null <- title_null$text

  title_explicit <- p_explicit$x$layoutAttrs$`1f3452ee66ba`$title
  if (is.list(title_explicit) && !is.null(title_explicit$text)) title_explicit <- title_explicit$text

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
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"),
    TADA.ResultMeasureValue = c(4.5, 6, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L"),
    stringsAsFactors = FALSE
  )

    multi_df <- data.frame(
      TADA.ComparableDataIdentifier = c("DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
                                        "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
                                        "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
                                        "PH_NONE_NONE_NONE",
                                        "PH_NONE_NONE_NONE",
                                        "PH_NONE_NONE_NONE"),
      TADA.ResultMeasureValue = c(4.5, 6, 5,
                                  7, 4.5, 6.7),
      TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L",
                                             "NONE", "NONE", "NONE"),
      stringsAsFactors = FALSE
    )

    p_single <- TADA_Boxplot(single_df)
    p_multi <- TADA_Boxplot(multi_df)

    testthat::expect_s3_class(p_single, "plotly")
    testthat::expect_s3_class(p_single, "htmlwidget")

    testthat::expect_type(p_multi, "list")
    testthat::expect_length(p_multi, 2)
    testthat::expect_true(all(vapply(p_multi, inherits, logical(1), what = "plotly")))
})

testthat::test_that("TADA_Boxplot sets expected layout fields", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"),
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
  testthat::expect_false(isTRUE(p$x$layoutAttrs$`1f345e0241b3`$xaxis$showticklabels))
  testthat::expect_false(isTRUE(p$x$config$displayModeBar))
})

testthat::test_that("TADA_Boxplot computes quartiles and median correctly", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c(
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"),
    TADA.ResultMeasureValue = c(4.5, 6, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L"),
    stringsAsFactors = FALSE
  )

  p <- TADA_Boxplot(df)

  values <- df$TADA.ResultMeasureValue

  expect_equal(as.numeric(p$x$attrs[[1]]$q1), as.numeric(stats::quantile(values, 0.25, type = 7)))
  expect_equal(as.numeric(p$x$attrs[[1]]$median), as.numeric(stats::median(values)))
  expect_equal(as.numeric(p$x$attrs[[1]]$q3), as.numeric(stats::quantile(values, 0.75, type = 7)))
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
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L"),
    TADA.ResultMeasureValue = c(4.5, 6, 5, 4),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L", "MG/L"),
    OrganizationIdentifier = c("Test Org A", "Test Org A", "Test Org B", "Test Org B"),
    stringsAsFactors = FALSE
  )

  p <- TADA_Boxplot(df, id_cols = c("TADA.ComparableDataIdentifier", "OrganizationIdentifier"))

  testthat::expect_type(p, "list")
  testthat::expect_length(p, 2)
  testthat::expect_true(all(c("DISSOLVED OXYGEN (DO) MG/L Test Org A",
                    "DISSOLVED OXYGEN (DO) MG/L Test Org B") %in% names(p)))
  testthat::expect_true(all(vapply(p, inherits, logical(1), what = "plotly")))
})

# test for TADA_Histogram

testthat::test_that("TADA_Histogram errors when required columns are missing", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE"),
    TADA.ResultMeasureValue = c(6, 7.2)
    # missing TADA.ResultMeasure.MeasureUnitCode
  )

  testthat::expect_error(
    TADA_Histogram(df),
    "TADA.ResultMeasure.MeasureUnitCode"
  )
})

testthat::test_that("TADA_Histogram defaults id_cols to TADA.ComparableDataIdentifier when id_cols is NULL", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE"),
    TADA.ResultMeasureValue = c(6, 7.2, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE"),
    stringsAsFactors = FALSE
  )

  p_null <- TADA_Histogram(df, id_cols = NULL)
  p_explicit <- TADA_Histogram(df, id_cols = "TADA.ComparableDataIdentifier")

  title_null <- p_null$x$layoutAttrs[[1]]$title

  title_explicit <- p_explicit$x$layoutAttrs[[1]]$title

  testthat::expect_equal(title_null, title_explicit)
  testthat::expect_equal(title_null, "Histogram of \nPH")
})

testthat::test_that("TADA_Histogram warns when TADA.ComparableDataIdentifier is not included in id_cols", {
  df <- data.frame(
    OtherGroup = c("a", "a", "b"),
    TADA.ResultMeasureValue = c(1, 2, 3),
    TADA.ResultMeasure.MeasureUnitCode = c("MG/L", "MG/L", "MG/L"),
    stringsAsFactors = FALSE
  )

  testthat::expect_warning(
    TADA_Histogram(df, id_cols = "OtherGroup"),
    "TADA.ComparableDataIdentifier not found in id_cols"
  )
})

testthat::test_that("TADA_Histogram removes NA values and emits a message", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE"),
    TADA.ResultMeasureValue = c(6, 7.2, NA),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE"),
    stringsAsFactors = FALSE
  )

  testthat::expect_message(
    p <- TADA_Histogram(df),
    "removed 1 results where TADA.ResultMeasureValue = NA"
  )

  testthat::expect_s3_class(p, "plotly")
})

testthat::test_that("TADA_Histogram returns a single plotly object when there is only one group", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE"),
    TADA.ResultMeasureValue = c(6, 7.2, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE"),
    stringsAsFactors = FALSE
  )


  p <- TADA_Histogram(df)

  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
})

testthat::test_that("TADA_Histogram returns a named list when there are multiple groups", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE",
                                      "TEMPERATURE_NONE_NONE_DEG C" , "TEMPERATURE_NONE_NONE_DEG C" , "TEMPERATURE_NONE_NONE_DEG C" ),
    TADA.ResultMeasureValue = c(6, 7.2, 5, 10, 12, 19),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE", "DEG C", "DEG C", "DEG C"),
    stringsAsFactors = FALSE
  )


  p <- TADA_Histogram(df)

  testthat::expect_type(p, "list")
  testthat::expect_length(p, 2)
  testthat::expect_named(p, c("PH" ,  "TEMPERATURE DEG C"))
  testthat::expect_true(all(vapply(p, inherits, logical(1), what = "plotly")))
})

testthat::test_that("TADA_Histogram sets expected layout fields", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = c("PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE", "PH_NONE_NONE_NONE"),
    TADA.ResultMeasureValue = c(6, 7.2, 5),
    TADA.ResultMeasure.MeasureUnitCode = c("NONE", "NONE", "NONE"),
    stringsAsFactors = FALSE
  )

  p <- TADA_Histogram(df)

  title <- p$x$layoutAttrs[[1]]$title

  testthat::expect_equal(title, "Histogram of \nPH")
  testthat::expect_equal(p$x$layoutAttrs[[1]]$xaxis$title, "NONE")
  testthat::expect_equal(p$x$layoutAttrs[[1]]$yaxis$title, "Frequency")
  testthat::expect_equal(p$x$layoutAttrs[[1]]$barmode, "overlay")
  testthat::expect_true(isTRUE(p$x$config$displayModeBar))
})

testthat::test_that("TADA_Histogram includes outlier-removed trace when outliers exist", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = rep("PH_NONE_NONE_NONE", 4),
    TADA.ResultMeasureValue = c(6, 7.2, 5, 22),
    TADA.ResultMeasure.MeasureUnitCode = rep("NONE", 4),
    stringsAsFactors = FALSE
  )

  p <- TADA_Histogram(df)

  json <- plotly::plotly_json(p, jsonedit = FALSE)

  testthat::expect_match(json, "Outliers Removed")
  testthat::expect_match(json, "All Data")
  testthat::expect_match(json, "legendonly")
})


testthat::test_that("TADA_Histogram returns NULL when all data are removed or no groups remain", {
  df <- data.frame(
    TADA.ComparableDataIdentifier = character(0),
    TADA.ResultMeasureValue = numeric(0),
    TADA.ResultMeasure.MeasureUnitCode = character(0),
    stringsAsFactors = FALSE
  )

  testthat::expect_message(
    out <- TADA_Histogram(df),
    "No data to plot; returning NULL."
  )

  testthat::expect_null(out)
})
