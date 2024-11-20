test_that("URLs are not broken", {
  # extract urls function
  extract_urls <- function(text) {
    stringr::str_extract_all(text, "http[s]?://[^\\s\\)\\]]+") %>% unlist()
  }

  # clean urls function
  clean_url <- function(url) {
    stringr::str_remove_all(url, "[\\\\.,\\\")]+$|[{}].*") %>%
      stringr::str_remove_all("[<>]")
  }

  # create lists of files to check
  other_files <- c(
    system.file("README.md", package = "EPATADA"),
    system.file("DESCRIPTION", package = "EPATADA"),
    system.file("NAMESPACE", package = "EPATADA")
  )

  vignettes <- list.files(system.file("vignettes", package = "EPATADA"), pattern = ".Rmd", full.names = TRUE)

  articles <- list.files(system.file("vignettes/articles", package = "EPATADA"), pattern = ".Rmd", full.names = TRUE)

  r_files <- list.files(system.file("R", package = "EPATADA"), pattern = ".R", full.names = TRUE)

  # combine file lists
  files <- append(other_files, vignettes) %>%
    append(articles) %>%
    append(r_files)

  # create list of urls
  urls <- purrr::map(files, ~ readLines(.x)) %>%
    unlist() %>%
    extract_urls() %>%
    clean_url() %>%
    unique() %>%
    # problematic URL I can't get a response from using multiple methods (itec) and CRAN because its response is inconsistent, likely due to redirecting to mirrors (HRM 10/28/2024)
    setdiff(c(
      # url works (HRM 11/7/24), but does not provide a recognizable response code
      "https://www.itecmembers.org/attains/",
      # if included will get 500 response because this is an incomplete URL
      # additional query information is pasted in as part of geospatial functions
      "https://attains.epa.gov/attains-public/api/assessmentUnits?assessmentUnitIdentifier="
    ))

  # retrieve http response headers from url list
  headers <- urls %>%
    purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))

  # extract response code from first line of header response
  response_code <- sapply(headers, "[[", 1)

  # create data frame of urls and response codes
  df <- data.frame(urls, response_code)

  # filter for any response codes that are not successful or redirect responses
  df_false <- df %>%
    dplyr::filter(!grepl("200", response_code) &
      !grepl("301", response_code) &
      !grepl("302", response_code))

  # count number of failed responses
  n <- nrow(df_false)

  # print url and response code for failures
  print(df_false)

  # verify that there are zero urls with failing response codes
  testthat::expect_equal(n, 0)
})
