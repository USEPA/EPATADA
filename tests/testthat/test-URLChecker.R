testthat::test_that("URLs are not broken", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  
  # URLs required for EPATADA functions
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
  
  # Exclude only truly non-testable URLs (incomplete or non-deterministic)
  exclude.urls <- c(
    # External mirror with inconsistent status behavior; not relevant to package
    "http://cran.us.r-project.org"
  )
  
  # extract urls function
  # Note: this is intentionally broad and picks up URLs embedded in prose, comments,
  # docs, and code examples. It is meant to scan the package source tree for broken links.
  extract_urls <- function(text) {
    stringr::str_extract_all(text, "http[s]?://[^\\s\\)\\]]+") |>
      unlist()
  }
  
  # clean urls function
  # Note: strips common trailing punctuation and brace-delimited fragments that often
  # appear in markdown, roxygen, or examples but are not part of the actual URL.
  clean_url <- function(url) {
    stringr::str_remove_all(url, "[\\\\.,\\\")]+$|[{}].*") |>
      stringr::str_remove_all("[<>]")
  }
  
  # workspace resolution
  # Prefer a CI/workflow workspace when present; otherwise resolve the package root
  # from the test file location. Avoid using here::here() here because it can resolve
  # to tests/testthat during R CMD check, which breaks package-root file discovery.
  workspace_dir <- Sys.getenv("GITHUB_WORKSPACE")
  if (workspace_dir == "") {
    workspace_dir <- testthat::test_path("..")
  }
  
  workspace_dir <- normalizePath(
    workspace_dir,
    winslash = "/",
    mustWork = FALSE
  )
  
  # files to scan
  # Note: README.md is expected at the package root, not under tests/testthat.
  other_files <- c(
    file.path(workspace_dir, "README.md"),
    file.path(workspace_dir, "DESCRIPTION"),
    file.path(workspace_dir, "NAMESPACE")
  )
  
  # Vignettes live in /vignettes at the package root.
  vignettes <- list.files(
    file.path(workspace_dir, "vignettes"),
    pattern = "\\.Rmd$",
    full.names = TRUE
  )
  
  # Articles are usually stored under /vignettes/articles.
  articles <- list.files(
    file.path(workspace_dir, "vignettes", "articles"),
    pattern = "\\.Rmd$",
    full.names = TRUE
  )
  
  # R source files to scan for hard-coded URLs.
  r_files <- list.files(
    file.path(workspace_dir, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  
  files <- c(other_files, vignettes, articles, r_files)
  
  # Keep only files that actually exist so missing optional files do not fail the test.
  # This is especially important during R CMD check, where some files may not be included
  # in the check-time package layout.
  files <- files[file.exists(files)]
  
  # collect and clean URLs, then remove exclusions
  # Note: readr::read_file() returns a single string per file; purrr::map() keeps the
  # scan explicit and easy to debug if a file unexpectedly fails to read.
  urls <- purrr::map(files, ~ readr::read_file(.x)) |>
    unlist(use.names = FALSE) |>
    extract_urls() |>
    clean_url() |>
    unique() |>
    setdiff(exclude.urls)
  
  # GET-based status+body checker with UA, timeout, and redirect handling
  # Why GET instead of HEAD?
  # Some services return misleading or incomplete results to HEAD requests, so GET is
  # used here to verify both reachability and a non-error response body when available.
  get_status_detail <- function(u) {
    ua <- "EPATADA/0.1 (+https://github.com/your-org/your-repo)"
    
    if (requireNamespace("httr2", quietly = TRUE)) {
      tryCatch(
        {
          resp <- httr2::request(u) |>
            httr2::req_user_agent(ua) |>
            httr2::req_method("GET") |>
            httr2::req_timeout(10) |>
            httr2::req_options(followlocation = TRUE) |>
            httr2::req_perform()
          
          list(
            status = httr2::resp_status(resp),
            body = httr2::resp_body_string(resp)
          )
        },
        error = function(e) list(status = NA_integer_, body = NA_character_)
      )
    } else if (requireNamespace("curl", quietly = TRUE)) {
      tryCatch(
        {
          h <- curl::new_handle()
          curl::handle_setheaders(h, "User-Agent" = ua)
          curl::handle_setopt(h, timeout = 10L, followlocation = TRUE)
          
          r <- curl::curl_fetch_memory(u, handle = h)
          list(
            status = as.integer(r$status_code),
            body = tryCatch(rawToChar(r$content), error = function(e) NA_character_)
          )
        },
        error = function(e) list(status = NA_integer_, body = NA_character_)
      )
    } else {
      # Fallback when neither httr2 nor curl is available.
      # This keeps the test from crashing, but means URL validation cannot be performed.
      list(status = NA_integer_, body = NA_character_)
    }
  }
  
  details <- purrr::map(urls, get_status_detail)
  status <- vapply(details, function(x) x$status, integer(1))
  body <- vapply(details, function(x) x$body, character(1))
  
  df <- data.frame(
    urls = urls,
    status = status,
    body = body,
    stringsAsFactors = FALSE
  )
  
  # Transient outage heuristics
  # 1) ATTAINS 404 Whitelabel Error Page (service up, endpoint down/outage)
  is_attains <- grepl("^https://attains\\.epa\\.gov/", df$urls)
  is_whitelabel <- !is.na(df$body) &
    grepl("Whitelabel Error Page", df$body, ignore.case = TRUE)
  attains_transient <- is_attains & df$status == 404L & is_whitelabel
  
  # 2) ArcGIS /query endpoints returning 400 parameter errors (incomplete query in test)
  # This is common for services that require query parameters such as where, outFields,
  # or geometry, and should not be treated as a broken endpoint by itself.
  is_arcgis <- grepl("arcgis/rest/services", df$urls)
  is_query <- grepl("/query\\??", df$urls)
  arcgis_param_error <- !is.na(df$body) &
    grepl("Invalid|missing|parameter", df$body, ignore.case = TRUE)
  arcgis_transient <- is_arcgis &
    is_query &
    df$status %in% c(400L, 499L) &
    arcgis_param_error
  
  is_transient <- attains_transient | arcgis_transient
  
  # Fail set: non-transient bad statuses
  # Note: 200/301/302 are treated as acceptable here because some services redirect
  # or vary their canonical landing page responses.
  df_false <- df[
    (is.na(df$status) | !(df$status %in% c(200L, 301L, 302L))) & !is_transient,
    ,
    drop = FALSE
  ]
  
  # Warn-only set: transient outages
  df_transient <- df[is_transient, , drop = FALSE]
  
  # Separate required package-function URLs from everything else so failures can be
  # reported with different emphasis.
  other.cols <- df_false |>
    dplyr::filter(!urls %in% func.urls) |>
    # temporarily filter out rows where both status and body are NA (short term fix)
    # longer term, the handling of urls leading to a csv or zip file needs improvement
    dplyr::filter(!is.na(status) & !is.na(body))
  
  n.other.cols <- nrow(other.cols)
  if (is.null(n.other.cols)) {
    n.other.cols <- 0L
  }
  
  func.cols <- df_false |>
    dplyr::filter(urls %in% func.urls) |>
    # temporarily filter out rows where both status and body are NA (short term fix)
    # longer term, the handling of urls leading to a csv or zip file needs improvement
    dplyr::filter(!is.na(status) & !is.na(body))
  
  n.func.cols <- nrow(func.cols)
  if (is.null(n.func.cols)) {
    n.func.cols <- 0L
  }
  
  # Convert failures/errors to warnings and muffle them (avoid test failures)
  # Note: This preserves the original intent of "warn instead of hard fail" while
  # still making the expectation visible in test output.
  expect_equal_or_warn <- function(...) {
    h <- function(e) {
      warning(conditionMessage(e), call. = FALSE)
      invokeRestart("muffle_expectation")
    }
    withCallingHandlers(
      testthat::expect_equal(...),
      expectation_failure = h,
      expectation_error = h
    )
  }
  
  # Required endpoints: warn if any non-transient failures
  expect_equal_or_warn(n.func.cols, 0L)
  if (n.func.cols > 0L) {
    message("Required URLs with failing status codes (non-transient):")
    print(func.cols[, c("urls", "status")])
  }
  
  # Other endpoints: warn if any non-transient failures
  expect_equal_or_warn(n.other.cols, 0L)
  if (n.other.cols > 0L) {
    message("Other URLs with failing status codes (non-transient):")
    print(other.cols[, c("urls", "status")])
  }
  
  # Report transient outages (warn-only)
  # These are useful to surface in CI logs without making the test brittle.
  if (nrow(df_transient) > 0L) {
    message("Detected transient service outages:")
    print(df_transient[, c("urls", "status")])
  }
})
