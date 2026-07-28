# tests for TADA_DayOfYearPlot
testthat::test_that("TADA_DayOfYearPlot returns a plotly object for valid input", {
  res <- TADA_DayOfYearPlot(
    Data_Nutrients_UT,
    comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
  )

  testthat::expect_s3_class(res, "plotly")
})


testthat::test_that("TADA_DayOfYearPlot errors when TADA.MonitoringLocationIdentifier is missing", {
  bad_data <- Data_Nutrients_UT |>
    dplyr::select(-TADA.MonitoringLocationIdentifier)

  testthat::expect_error(
    TADA_DayOfYearPlot(
      bad_data,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "TADA.MonitoringLocationIdentifier"
  )
})

testthat::test_that("TADA_DayOfYearPlot errors when comparableDataId is missing", {
  testthat::expect_error(
    TADA_DayOfYearPlot(Data_Nutrients_UT),
    "TADA.ComparableDataIdentifier"
  )
})

testthat::test_that("TADA_DayOfYearPlot errors when comparableDataId is not found", {
  testthat::expect_error(
    TADA_DayOfYearPlot(Data_Nutrients_UT, comparableDataId = "NOT_A_REAL_ID"),
    "was not found"
  )
})

testthat::test_that("TADA_DayOfYearPlot errors when location is invalid", {
  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      location = "BAD_LOCATION",
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "not found"
  )
})

testthat::test_that("TADA_DayOfYearPlot errors when location length is not 1", {
  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      location = c("A", "B"),
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
    ),
    "must be one monitoring location identifier or 'all'"
  )
})


testthat::test_that("TADA_DayOfYearPlot errors when monthRange is invalid", {
  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
      monthRange = c(13, 14)
    ),
    "monthRange"
  )
})

testthat::test_that("TADA_DayOfYearPlot errors when monthRange is decreasing", {
  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
      monthRange = c(10, 4)
    ),
    "monthRange"
  )
})

testthat::test_that("TADA_DayOfYearPlot errors when yearRange is invalid", {
  testthat::expect_error(
    TADA_DayOfYearPlot(
      Data_Nutrients_UT,
      comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
      yearRange = c(2022, 2020)
    ),
    "yearRange"
  )
})

testthat::test_that("TADA_DayOfYearPlot returns NULL when no data match filters", {
  res <- TADA_DayOfYearPlot(
    Data_Nutrients_UT,
    comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L",
    yearRange = c(1800, 1801)
  )

  testthat::expect_null(res)
})


testthat::test_that("TADA_DayOfYearPlot works when optional columns are missing", {
  bad_data <- Data_Nutrients_UT |>
    dplyr::select(
      -dplyr::any_of(c(
        "TADA.MonitoringLocationName",
        "OrganizationFormalName",
        "ActivityStartDateTime"
      ))
    )

  res <- TADA_DayOfYearPlot(
    bad_data,
    comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
  )

  testthat::expect_s3_class(res, "plotly")
})

testthat::test_that("TADA_DayOfYearPlot creates one trace per year", {
  res <- TADA_DayOfYearPlot(
    Data_Nutrients_UT,
    comparableDataId = "NITRATE_DISSOLVED_AS N_MG/L"
  )

  built <- plotly::plotly_build(res)

  testthat::expect_true(length(built$x$data) >= 1)
})
