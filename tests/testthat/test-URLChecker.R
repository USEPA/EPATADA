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
  other_files <- c(system.file("README.md", package="EPATADA"),
                   system.file("DESCRIPTION", package="EPATADA"),
                   system.file("NAMESPACE", package="EPATADA"))
  
  vignettes <- list.files("/vignettes", pattern = ".Rmd", full.names = TRUE)
  
  articles <- list.files("/vignettes/articles", pattern = ".Rmd", full.names = TRUE)
  
  r_files <- list.files("R", pattern = ".R", full.names = TRUE)
  
  files <- append(other_files, vignettes) %>%
    append(articles) %>%
    append(r_files)
  
  # create list of urls
  urls_from_r <- purrr::map(files, ~readLines(.x)) %>% unlist() %>% extract_urls() %>%
    clean_url() %>%
    unique() %>%
    # problematic URL I can't get a response from using multiple methods (itec) and cran (which redirects) (HRM 10/28/2024)
    setdiff(c("https://www.itecmembers.org/attains/", "http://cran.us.r-project.org"))
    
  
  headers <- urls_from_r %>%
    purrr::map(~ tryCatch(curlGetHeaders(.x), error = function(e) NA))
  
  header_list <- sapply(headers,"[[",1)
  
  df <- data.frame(urls_from_r, header_list) 
  
  df_false <- df %>%
    dplyr::filter(!header_list %in% c("HTTP/1.1 200 OK\r\n", "HTTP/1.1 200 \r\n"))
  
  testthat::expect_true(nrow(df_false) == 0)
})

