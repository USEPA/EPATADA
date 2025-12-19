# ignore warning
# file("") only supports open = "w+" and open = "w+b": using the former
suppressWarnings(test_that("URLs are not broken", {
  # urls required for EPATADA functions
  func.urls <- c(
    # WQP
    "https://www.waterqualitydata.us/",

    # ATTAINS GIS
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?",
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?",
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?",
    "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?",

    # Tribal GIS
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0/query",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1/query",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2/query",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3/query",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4/query",
    "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5/query",

    # WQX domain values
    "https://cdx.epa.gov/wqx/download/DomainValues/ActivityType.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/ResultMeasureQualifier.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/Organization.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation_CSV.zip",
    "https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.zip",
    "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit_CSV.zip",
    "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.zip",
    "https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicAlias_CSV.zip",
    "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/County_CSV.zip",
    "https://cdx.epa.gov/wqx/download/DomainValues/ResultDetectionCondition.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/DetectionQuantitationLimitType.CSV",
    "https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV"
  )

  # extract urls function
  extract_urls <- function(text) {
    stringr::str_extract_all(text, "http[s]?://[^\\s\\)\\]]+") |> unlist()
  }

  # clean urls function
  clean_url <- function(url) {
    stringr::str_remove_all(url, "[\\\\.,\\\")]+$|[{}].*") |>
      stringr::str_remove_all("[<>]")
  }

  # get workspace directory
  workspace_dir <- Sys.getenv("GITHUB_WORKSPACE")

  if (workspace_dir == "") {
    if (requireNamespace("here", quietly = TRUE)) {
      workspace_dir <- here::here() # project root (preferred if you use RStudio projects)
    } else {
      workspace_dir <- getwd() # current working directory
    }
  }

  # normalize path
  workspace_dir <- normalizePath(
    workspace_dir,
    winslash = "/",
    mustWork = FALSE
  )

  # create lists of files to check for URLs
  other_files <- c(
    file.path(workspace_dir, "README.md"),
    file.path(workspace_dir, "DESCRIPTION"),
    file.path(workspace_dir, "NAMESPACE")
  )

  vignettes <- list.files(
    file.path(workspace_dir, "vignettes"),
    pattern = "\\.Rmd$", # Escape the dot for regex
    full.names = TRUE
  )

  articles <- list.files(
    file.path(workspace_dir, "vignettes", "articles"),
    pattern = "\\.Rmd$", # Escape the dot for regex
    full.names = TRUE
  )

  r_files <- list.files(
    file.path(workspace_dir, "R"),
    pattern = "\\.R$", # Escape the dot for regex
    full.names = TRUE
  )

  # combine file lists
  files <- append(other_files, vignettes) |> append(articles) |> append(r_files)

  files <- purrr::map(files, normalizePath)

  # create list of urls
  urls <- purrr::map(files, ~ readr::read_file(.x)) |>
    unlist() |>
    extract_urls() |>
    clean_url() |>
    unique() |>
    # problematic URL I can't get a response from using multiple methods (itec)
    # and CRAN because its response is inconsistent, likely due to redirecting to mirrors (HRM 10/28/2024)
    setdiff(c(
      # url works (HRM 11/7/24), but does not provide a recognizable response code
      "https://www.itecmembers.org/attains/",
      # if included will get 500 response because this is an incomplete URL
      # additional query information is pasted in as part of geospatial functions
      "https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier=",
      # page loads but does not return a response code (NA)
      "http://cran.us.r-project.org"
    ))

  # retrieve http response headers from url list
  headers <- urls |>
    purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))

  # extract response code from first line of header response
  response_code <- sapply(headers, "[[", 1)

  # create dataframe of urls and response codes
  df <- data.frame(urls, response_code)

  # filter for any response codes that are not successful or redirect responses
  df_false <- df |>
    dplyr::filter(
      !grepl("200", response_code) &
        !grepl("301", response_code) &
        !grepl("302", response_code)
    )

  other.cols <- df_false |> dplyr::filter(!urls %in% func.urls)

  n.other.cols <- nrow(other.cols)

  if (is.null(n.other.cols)) {
    n.other.cols <- 0
  }

  func.cols <- df_false |> dplyr::filter(urls %in% func.urls)

  n.func.cols <- nrow(func.cols)

  if (is.null(n.func.cols)) {
    n.func.cols <- 0
  }

  # set up test to produce warning, not failure
  # based on https://www.youtube.com/watch?v=wIfduhx6sJs, "Throw Warnings Instead of Errors in your R Unit Tests with testthat"
  expect_equal_or_warn <- function(...) {
    tryCatch(testthat::expect_equal(...), error = function(e) warning(e))
  }

  # verify that there are zero function urls with failing response codes
  expect_equal_or_warn(n.func.cols, 0)

  # print url and response code for failures
  if (n.func.cols > 0) {
    print(
      "The following URLs are required for EPATADA functions and have failing response codes."
    )

    print(func.cols)
  }

  # verify that there are zero function urls with failing response codes
  expect_equal_or_warn(n.other.cols, 0)

  # print url and response code for failures
  if (n.other.cols > 0) {
    print(
      "The following URLs are required for EPATADA functions and have failing response codes."
    )

    print(other.cols)
  }
}))
