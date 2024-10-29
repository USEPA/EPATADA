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
  urls_from_r <- purrr::map(files, ~ readLines(.x)) %>%
    unlist() %>%
    extract_urls() %>%
    clean_url() %>%
    unique() %>%
    # problematic URL I can't get a response from using multiple methods (itec) and CRAN because its response is inconsistent, likely due to redirecting to mirrors (HRM 10/28/2024)
    setdiff(c(
      "https://www.itecmembers.org/attains/",
      "https://cran.us.r-project.org",
      "http://cran.us.r-project.org"
    ))


  headers <- urls_from_r %>%
    purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))

  header_list <- sapply(headers, "[[", 1)

  df <- data.frame(urls_from_r, header_list)

  df_false <- df %>%
    dplyr::filter(!header_list %in% c("HTTP/1.1 200 OK\r\n", "HTTP/1.1 200 \r\n"))

  n <- nrow(df_false)

  print(df_false)

  testthat::expect_equal(n, 0)
})
