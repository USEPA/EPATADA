test_that("URLs are not broken", {
  check_package_urls <- function(pkg_path) {
    # Check URLs and get the results
    results <- url_check(pkg_path)
    # Filter out valid URLs
    broken_urls <- results[results$status_code >= 400, ]
    return(broken_urls)
  }
  
  pkg_path <- find.package("EPATADA")
  
  broken_urls <- check_package_urls(pkg_path)
  expect_true(nrow(broken_urls) == 0, info = paste("Broken URLs found:", broken_urls$url))
})

